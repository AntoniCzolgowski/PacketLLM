# history_manager.R

# Środowisko do przechowywania stanu historii
.history_env <- new.env(parent = emptyenv())
.history_env$conversations <- list()
.history_env$active_conversation_id <- NULL
.history_env$conversation_counter <- 0

#' Generuje unikalne ID konwersacji
#' @return String z ID konwersacji.
#' @noRd
generate_conversation_id <- function() {
  .history_env$conversation_counter <- .history_env$conversation_counter + 1
  paste0("conv_", .history_env$conversation_counter, "_", as.integer(Sys.time()))
}

#' Inicjalizuje menedżera historii
#'
#' Czyści stan i tworzy pierwszą, pustą konwersację.
#' @return ID pierwszej utworzonej konwersacji.
#' @export
initialize_history_manager <- function() {
  message("Inicjalizuję menedżera historii...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  first_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE, title = "Nowa Rozmowa")
  message(paste("Menedżer historii zainicjalizowany. Utworzono pierwszą rozmowę:", first_id))
  return(first_id)
}

#' Tworzy nową konwersację
#'
#' Dodaje nową konwersację do środowiska `.history_env`.
#'
#' @param activate Czy ustawić nową konwersację jako aktywną (domyślnie FALSE).
#' @param add_initial_settings Czy dodać domyślny komunikat systemowy do historii (domyślnie TRUE).
#' @param title Początkowy tytuł konwersacji (domyślnie generowany na podstawie czasu).
#' @return ID nowo utworzonej konwersacji.
#' @export
create_new_conversation <- function(activate = FALSE, add_initial_settings = TRUE, title = NULL) {
  conv_id <- generate_conversation_id()
  if (is.null(title)) {
    title <- paste("Rozmowa", format(Sys.time(), "%H:%M:%S"))
  }
  message(paste("Tworzę nową konwersację o ID:", conv_id, "i tytule:", title))

  default_model <- if (exists("available_openai_models") && length(available_openai_models) > 0) {
    available_openai_models[1] # Używamy zmiennej z openai_api.R
  } else {
    "gpt-4o" # Fallback
  }
  default_system_message <- "Jesteś pomocnym asystentem. Odpowiadaj w sposób czytelny i precyzyjny, zachowując formatowanie kodu, gdy jest to wymagane."
  default_temperature <- 0.5


  new_conv <- list(
    id = conv_id,
    title = title,
    history = list(),
    attachments = list(),
    created_at = Sys.time(),
    temperature = default_temperature,
    system_message = "", # Ustawiane poniżej
    model = default_model,
    model_locked = FALSE # Flaga blokady zmiany modelu
  )

  if (add_initial_settings) {
    # Dodaj komunikat systemowy tylko do pola `system_message`, niekoniecznie do `history` na start
    # new_conv$history <- list(list(role = "system", content = default_system_message))
    new_conv$system_message <- default_system_message
    message(paste("Dodano domyślne ustawienia (SysMsg, Temp) i model (", default_model, ") do rozmowy", conv_id))
  } else {
    new_conv$system_message <- "" # Pusty, jeśli nie dodajemy ustawień
    message(paste("Ustawiono domyślny model (", default_model, ") dla rozmowy", conv_id, " (bez początkowych ustawień)"))
  }

  .history_env$conversations[[conv_id]] <- new_conv

  if (activate) {
    set_active_conversation(conv_id)
  }
  return(conv_id)
}

#' Sprawdza, czy konwersacja została rozpoczęta (model zablokowany)
#'
#' Sprawdza, czy flaga `model_locked` jest TRUE. Model jest blokowany
#' po dodaniu pierwszej wiadomości **asystenta**.
#'
#' @param id ID konwersacji.
#' @return TRUE jeśli model jest zablokowany, FALSE w przeciwnym razie lub jeśli konwersacja nie istnieje.
#' @export
is_conversation_started <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Próba sprawdzenia stanu blokady dla nieistniejącej konwersacji: ", id)
    return(FALSE)
  }
  # Dostęp do flagi model_locked
  return(isTRUE(.history_env$conversations[[id]]$model_locked))
}


#' Dodawanie wiadomości do historii aktywnej konwersacji
#'
#' Aktualizuje tytuł konwersacji na podstawie treści, jeśli to pierwsza wiadomość użytkownika.
#' Ustawia flagę `model_locked` na TRUE przy pierwszej wiadomości **asystenta**.
#'
#' @param role Rola ("user", "assistant", "system").
#' @param content Treść wiadomości.
#' @return Lista z informacją o wyniku:
#'         - `list(type = "title_set", new_title = "...")` jeśli ustawiono nowy tytuł.
#'         - `list(type = "assistant_locked_model")` jeśli model został zablokowany.
#'         - `list(type = "message_added")` w pozostałych przypadkach dodania wiadomości.
#'         - `list(type = "error", message = "...")` w przypadku błędu.
#' @export
add_message_to_active_history <- function(role, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    warning("Nie można dodać wiadomości, brak aktywnej konwersacji.")
    return(list(type = "error", message = "Brak aktywnej konwersacji."))
  }
  if (!role %in% c("user", "assistant", "system")) {
    warning("Nieprawidłowa rola wiadomości: ", role)
    return(list(type = "error", message = paste("Nieprawidłowa rola:", role)))
  }

  # Sprawdź, czy konwersacja istnieje
  if (!active_id %in% names(.history_env$conversations)) {
    warning("Aktywna konwersacja (ID: ", active_id, ") nie istnieje w momencie dodawania wiadomości.")
    return(list(type = "error", message = "Aktywna konwersacja nie istnieje."))
  }

  # Bezpieczny dostęp do konwersacji
  # conv_ref <- .history_env$conversations[[active_id]] # Zamiast tryCatch

  return_value <- list(type = "message_added") # Domyślna wartość zwracana
  new_message <- list(role = role, content = content)

  # Sprawdź, czy model powinien zostać zablokowany (pierwsza odpowiedź asystenta)
  should_lock_model <- role == "assistant" && !isTRUE(.history_env$conversations[[active_id]]$model_locked)

  # Dodaj wiadomość do historii
  current_history <- .history_env$conversations[[active_id]]$history %||% list()
  .history_env$conversations[[active_id]]$history <- c(current_history, list(new_message))
  message(paste("Dodano wiadomość", role, "do", active_id))

  # Zablokuj model, jeśli trzeba
  if (should_lock_model) {
    .history_env$conversations[[active_id]]$model_locked <- TRUE
    message(paste("Model dla konwersacji", active_id, "został zablokowany."))
    # Ustaw typ zwrotny (może zostać nadpisany przez logikę tytułu)
    if (return_value$type == "message_added") {
      return_value <- list(type = "assistant_locked_model")
    }
  }

  # --- Logika ustawiania tytułu ---
  # Odśwież referencję po dodaniu wiadomości
  conv_history_updated <- .history_env$conversations[[active_id]]$history %||% list()
  user_message_count <- sum(sapply(conv_history_updated, function(m) !is.null(m$role) && m$role == "user"))

  # Sprawdź, czy to *pierwsza* wiadomość użytkownika w tej konwersacji
  is_first_user_message_ever <- role == "user" && user_message_count == 1

  if (is_first_user_message_ever && nzchar(trimws(content))) {
    words <- strsplit(trimws(content), "\\s+")[[1]]
    words <- words[nzchar(words)]
    max_words <- 5
    max_chars <- 40
    new_title <- paste(head(words, max_words), collapse = " ")
    truncated_by_words <- length(words) > max_words
    truncated_by_chars <- nchar(new_title) > max_chars

    if (truncated_by_chars) {
      new_title <- paste0(substr(new_title, 1, max_chars - 3), "...")
    } else if (truncated_by_words) {
      new_title <- paste0(new_title, "...")
    }
    if (!nzchar(new_title)) {
      new_title <- paste("Rozmowa", format(Sys.time(), "%M%S"))
    }

    .history_env$conversations[[active_id]]$title <- new_title
    message(paste("Ustawiono tytuł dla", active_id, "na:", new_title))
    return_value <- list(type = "title_set", new_title = new_title)
  }
  # --- Koniec logiki tytułu ---

  return(return_value)
}


#' Usuwa konwersację o podanym ID
#' @param id ID konwersacji do usunięcia.
#' @return TRUE jeśli usunięto, FALSE jeśli nie istniała.
#' @export
delete_conversation <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Próba usunięcia nieistniejącej konwersacji o ID: ", id)
    return(FALSE)
  }
  message(paste("Usuwam konwersację o ID:", id))
  .history_env$conversations[[id]] <- NULL

  current_active_id <- .history_env$active_conversation_id
  if (!is.null(current_active_id) && current_active_id == id) {
    message("Usunięto aktywną konwersację. Resetuję active_conversation_id.")
    .history_env$active_conversation_id <- NULL
  }
  return(TRUE)
}

#' Ustawia aktywną konwersację
#' @param id ID konwersacji do aktywacji lub NULL, aby zdezaktywować.
#' @return Invisible NULL.
#' @export
set_active_conversation <- function(id) {
  if (is.null(id)) {
    if (!is.null(.history_env$active_conversation_id)) {
      message("Dezaktywowano aktywną konwersację.")
      .history_env$active_conversation_id <- NULL
    }
    return(invisible(NULL))
  }

  if (!id %in% names(.history_env$conversations)) {
    warning("Próba ustawienia nieistniejącej konwersacji jako aktywnej: ", id)
    # Nie ustawiaj active_id na nieistniejący
    # .history_env$active_conversation_id <- NULL
    return(invisible(NULL))
  }

  if (is.null(.history_env$active_conversation_id) || .history_env$active_conversation_id != id) {
    message(paste("Ustawiono aktywną konwersację na ID:", id))
    .history_env$active_conversation_id <- id
  }
  return(invisible(NULL))
}

#' Pobiera ID aktywnej konwersacji
#' @return ID aktywnej konwersacji (string) lub NULL.
#' @export
get_active_conversation_id <- function() {
  active_id <- .history_env$active_conversation_id
  # Sprawdź, czy ID wskazuje na istniejącą konwersację
  if (!is.null(active_id) && !active_id %in% names(.history_env$conversations)) {
    warning(paste("Aktywny ID", active_id, "wskazuje na nieistniejącą konwersację. Resetuję."))
    .history_env$active_conversation_id <- NULL
    return(NULL)
  }
  return(active_id)
}

#' Pobiera pełny obiekt aktywnej konwersacji
#' @return Lista reprezentująca konwersację lub NULL.
#' @export
get_active_conversation <- function() {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    return(NULL)
  }
  # Zwróć dane dla istniejącego ID
  return(.history_env$conversations[[active_id]])
}

#' Pobiera historię czatu dla aktywnej konwersacji
#' @return Lista wiadomości lub pusta lista.
#' @export
get_active_chat_history <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    return(list())
  }
  return(active_conv$history %||% list())
}

#' Pobiera tytuł konwersacji o podanym ID
#' @param id ID konwersacji.
#' @return Tytuł (string) lub NULL, jeśli konwersacja nie istnieje lub nie ma tytułu.
#' @export
get_conversation_title <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    # warning("Próba pobrania tytułu dla nieistniejącej konwersacji: ", id)
    return(NULL)
  }
  # Dostęp do tytułu
  return(.history_env$conversations[[id]]$title %||% paste("[Brak tytułu - ID:", id, "]"))
}

#' Pobiera listę ID wszystkich istniejących konwersacji
#' @return Wektor znakowy z ID konwersacji.
#' @export
get_all_conversation_ids <- function() {
  return(names(.history_env$conversations))
}

#' Pobiera nazwę modelu dla konwersacji o podanym ID
#' @param id ID konwersacji.
#' @return Nazwa modelu (string) lub NULL.
#' @export
get_conversation_model <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  # Dostęp do modelu
  return(.history_env$conversations[[id]]$model %||% "gpt-4o") # Fallback
}

#' Ustawia model dla konwersacji, jeśli nie została rozpoczęta
#'
#' Sprawdza flagę `model_locked` ustawianą po pierwszej odpowiedzi asystenta.
#'
#' @param id ID konwersacji.
#' @param model_name Nazwa nowego modelu.
#' @return TRUE jeśli model został ustawiony, FALSE w przeciwnym razie.
#' @export
set_conversation_model <- function(id, model_name) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Próba ustawienia modelu dla nieistniejącej konwersacji: ", id)
    return(FALSE)
  }
  if (is_conversation_started(id)) {
    warning("Nie można zmienić modelu - konwersacja została już rozpoczęta.", call. = FALSE)
    return(FALSE)
  }
  # Walidacja modelu (opcjonalne, ale dobre)
  if (exists("available_openai_models") && !model_name %in% available_openai_models) {
    warning(paste("Próba ustawienia niedostępnego modelu:", model_name, "dla konwersacji", id))
    return(FALSE)
  }

  # Ustawienie modelu
  .history_env$conversations[[id]]$model <- model_name
  message(paste("Ustawiono model dla konwersacji", id, "na:", model_name))
  return(TRUE)
}


# --- NOWE FUNKCJE ---

#' Ustawia temperaturę dla konwersacji o podanym ID
#' @param id ID konwersacji.
#' @param temperature Nowa wartość temperatury (liczba 0-1).
#' @return TRUE jeśli ustawiono, FALSE jeśli konwersacja nie istnieje lub wartość jest nieprawidłowa.
#' @export
set_conversation_temperature <- function(id, temperature) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Próba ustawienia temperatury dla nieistniejącej konwersacji: ", id)
    return(FALSE)
  }
  if (!is.numeric(temperature) || temperature < 0 || temperature > 1) {
    warning(paste("Nieprawidłowa wartość temperatury:", temperature))
    return(FALSE)
  }
  # Ustawienie temperatury
  .history_env$conversations[[id]]$temperature <- temperature
  # message(paste("Ustawiono temperaturę dla konwersacji", id, "na:", temperature)) # Mniej gadatliwe
  return(TRUE)
}

#' Ustawia komunikat systemowy dla konwersacji o podanym ID
#' @param id ID konwersacji.
#' @param message Nowy komunikat systemowy (string).
#' @return TRUE jeśli ustawiono, FALSE jeśli konwersacja nie istnieje lub wartość jest nieprawidłowa.
#' @export
set_conversation_system_message <- function(id, message) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Próba ustawienia komunikatu systemowego dla nieistniejącej konwersacji: ", id)
    return(FALSE)
  }
  if (!is.character(message) || length(message) != 1) {
    warning("Nieprawidłowy format komunikatu systemowego.")
    return(FALSE)
  }
  # Ustawienie komunikatu
  .history_env$conversations[[id]]$system_message <- message
  # message(paste("Ustawiono komunikat systemowy dla konwersacji", id)) # Mniej gadatliwe
  return(TRUE)
}

#' Pobiera historię czatu dla konwersacji o podanym ID
#' @param id ID konwersacji.
#' @return Lista wiadomości lub NULL, jeśli konwersacja nie istnieje.
#' @export
get_conversation_history <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]]$history %||% list())
}

#' Pobiera listę załączników dla konwersacji o podanym ID
#' @param id ID konwersacji.
#' @return Lista załączników lub NULL, jeśli konwersacja nie istnieje.
#' @export
get_conversation_attachments <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]]$attachments %||% list())
}

#' Pobiera pełny obiekt danych konwersacji o podanym ID
#' @param id ID konwersacji.
#' @return Lista z danymi konwersacji lub NULL, jeśli nie istnieje.
#' @export
get_conversation_data <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]])
}

# --- KONIEC NOWYCH FUNKCJI ---


#' Resetuje cały stan menedżera historii
#' @return Invisible NULL.
#' @export
reset_history_manager <- function() {
  message("Resetowanie menedżera historii...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  invisible(NULL)
}

#' Dodaje załącznik do aktywnej konwersacji
#' @param name Nazwa pliku załącznika.
#' @param content Treść pliku jako string.
#' @return TRUE jeśli dodano, FALSE jeśli plik o tej nazwie już istnieje lub brak aktywnej konwersacji.
#' @export
add_attachment_to_active_conversation <- function(name, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    warning("Nie można dodać załącznika, brak aktywnej konwersacji.")
    return(FALSE)
  }
  if (!active_id %in% names(.history_env$conversations)) {
    warning(paste("Aktywna konwersacja (ID:", active_id, ") nie istnieje w momencie dodawania załącznika."))
    return(FALSE)
  }

  conv_attachments <- .history_env$conversations[[active_id]]$attachments %||% list()

  # Sprawdź, czy plik o tej nazwie już istnieje
  if (any(sapply(conv_attachments, function(att) !is.null(att$name) && att$name == name))) {
    message(paste("Plik o nazwie", name, "już istnieje w konwersacji", active_id, ". Nie dodaję ponownie."))
    return(FALSE) # Zwróć FALSE, aby UI wiedziało, że nie dodano
  }

  new_attachment <- list(name = name, content = content)
  .history_env$conversations[[active_id]]$attachments <- c(conv_attachments, list(new_attachment))
  message(paste("Dodano załącznik", name, "do konwersacji", active_id))

  return(TRUE)
}

#' Pobiera listę załączników dla aktywnej konwersacji
#' @return Lista załączników lub pusta lista.
#' @export
get_active_conversation_attachments <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    return(list())
  }
  return(active_conv$attachments %||% list())
}

# Helper %||% (może być zdefiniowany globalnie)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
