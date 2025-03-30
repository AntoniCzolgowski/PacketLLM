# history_manager.R

# Prywatne środowisko do zarządzania historią całej sesji
.history_env <- new.env(parent = emptyenv())
.history_env$conversations <- list() # Lista przechowująca poszczególne rozmowy
.history_env$active_conversation_id <- NULL # ID aktualnie aktywnej rozmowy
.history_env$conversation_counter <- 0 # Licznik do generowania unikalnych ID

#' Generowanie unikalnego ID dla konwersacji
#' @return Unikalny identyfikator (string)
generate_conversation_id <- function() {
  .history_env$conversation_counter <- .history_env$conversation_counter + 1
  paste0("conv_", .history_env$conversation_counter, "_", as.integer(Sys.time()))
}

#' Inicjalizacja menedżera historii
#'
#' Tworzy pierwszą konwersację. Wywoływana na starcie serwera gadżetu.
#' @return ID nowo utworzonej, pierwszej konwersacji.
initialize_history_manager <- function() {
  message("Inicjalizuję menedżera historii...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  first_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE) # Zmieniono flagę
  message(paste("Menedżer historii zainicjalizowany. Utworzono pierwszą rozmowę:", first_id))
  return(first_id)
}

#' Tworzenie nowej konwersacji
#'
#' Tworzy nowy obiekt konwersacji z domyślnymi ustawieniami zaawansowanymi.
#'
#' @param activate Czy ustawić nową konwersację jako aktywną? (Domyślnie FALSE)
#' @param add_initial_settings Czy dodać domyślny komunikat systemowy i temperaturę? (Domyślnie TRUE) # Zmieniono nazwę flagi
#' @param title Początkowy tytuł konwersacji (Domyślnie "Nowa rozmowa [czas]")
#' @return ID nowo utworzonej konwersacji.
#' @export
create_new_conversation <- function(activate = FALSE, add_initial_settings = TRUE, title = NULL) {
  conv_id <- generate_conversation_id()
  if (is.null(title)) {
    title <- paste("Rozmowa", format(Sys.time(), "%H:%M:%S"))
  }
  message(paste("Tworzę nową konwersację o ID:", conv_id, "i tytule:", title))

  # Domyślne wartości
  default_system_message <- "Jesteś pomocnym asystentem. Odpowiadaj w sposób czytelny i precyzyjny, zachowując formatowanie kodu, gdy jest to wymagane."
  default_temperature <- 0.5

  new_conv <- list(
    id = conv_id,
    title = title,
    history = list(),
    attachments = list(),
    created_at = Sys.time(),
    # ZMIANA: Dodanie pól dla ustawień zaawansowanych
    temperature = default_temperature,
    system_message = "" # Inicjalizuj puste, jeśli nie dodajemy od razu
  )

  if (add_initial_settings) {
    new_conv$history <- list(list(role = "system", content = default_system_message)) # Komunikat systemowy idzie do historii TYLKO RAZ przy tworzeniu
    new_conv$system_message <- default_system_message # Przechowujemy edytowalną wersję osobno
    message(paste("Dodano domyślny komunikat systemowy i temperaturę do rozmowy", conv_id))
  }

  .history_env$conversations[[conv_id]] <- new_conv

  if (activate) {
    set_active_conversation(conv_id)
  }
  return(conv_id)
}

#' Usuwanie konwersacji
#'
#' Usuwa konwersację o podanym ID.
#'
#' @param id ID konwersacji do usunięcia.
#' @return TRUE jeśli usunięto, FALSE jeśli ID nie istniało.
#' @export
delete_conversation <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Próba usunięcia nieistniejącej konwersacji o ID: ", id)
    return(FALSE)
  }
  message(paste("Usuwam konwersację o ID:", id))
  .history_env$conversations[[id]] <- NULL

  if (!is.null(.history_env$active_conversation_id) && .history_env$active_conversation_id == id) {
    message("Usunięto aktywną konwersację. Resetuję active_conversation_id.")
    .history_env$active_conversation_id <- NULL
  }
  return(TRUE)
}


#' Ustawianie aktywnej konwersacji
#' @param id ID konwersacji do aktywacji.
#' @return Invisible NULL.
#' @export
set_active_conversation <- function(id) {
  if (is.null(id)) {
    # message("Ustawiono aktywną konwersację na NULL") # Mniej gadatliwe logowanie
    .history_env$active_conversation_id <- NULL
    return(invisible(NULL))
  }
  if (!id %in% names(.history_env$conversations)) {
    warning("Próba ustawienia nieistniejącej konwersacji jako aktywnej: ", id)
    return(invisible(NULL))
  }
  if (is.null(.history_env$active_conversation_id) || .history_env$active_conversation_id != id) {
    # message(paste("Ustawiono aktywną konwersację na:", id)) # Mniej gadatliwe logowanie
    .history_env$active_conversation_id <- id
  }
  invisible(NULL)
}

#' Pobieranie ID aktywnej konwersacji
#' @return ID aktywnej konwersacji lub NULL.
#' @export
get_active_conversation_id <- function() {
  .history_env$active_conversation_id
}

#' Pobieranie obiektu aktywnej konwersacji
#' @return Pełny obiekt (lista) aktywnej konwersacji lub NULL.
#' @export
get_active_conversation <- function() {
  active_id <- get_active_conversation_id()
  if (is.null(active_id) || !active_id %in% names(.history_env$conversations)) {
    return(NULL)
  }
  return(.history_env$conversations[[active_id]])
}

#' Pobieranie historii czatu dla aktywnej konwersacji
#' @return Lista wiadomości ({role, content}) dla aktywnej konwersacji lub pusta lista.
#' @export
get_active_chat_history <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) return(list())
  return(active_conv$history %||% list())
}

#' Dodawanie wiadomości do historii aktywnej konwersacji
#'
#' Aktualizuje tytuł, jeśli to pierwsza wiadomość użytkownika.
#'
#' @param role Rola ("user", "assistant", "system").
#' @param content Treść wiadomości.
#' @return Nowy tytuł konwersacji (string) jeśli został zaktualizowany, w przeciwnym razie NULL.
#' @export
add_message_to_active_history <- function(role, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) stop("Nie można dodać wiadomości, brak aktywnej konwersacji.")
  if (!active_id %in% names(.history_env$conversations)) stop("Aktywna konwersacja (ID: ", active_id, ") już nie istnieje.")
  if (!role %in% c("user", "assistant", "system")) stop("Nieprawidłowa rola wiadomości: ", role)

  conv <- .history_env$conversations[[active_id]]
  new_title_generated <- NULL

  new_message <- list(role = role, content = content)
  conv$history <- c(conv$history %||% list(), list(new_message))

  # Aktualizacja tytułu
  is_first_user_message <- role == "user" && sum(sapply(conv$history, function(m) m$role == "user")) == 1
  if (is_first_user_message && nzchar(trimws(content))) {
    words <- strsplit(trimws(content), "\\s+")[[1]]
    new_title <- paste(head(words, 5), collapse = " ")
    if (nchar(new_title) > 35) new_title <- substr(new_title, 1, 32)
    if (nchar(new_title) < nchar(trimws(content))) new_title <- paste0(new_title, "...")

    conv$title <- new_title
    new_title_generated <- new_title
    message(paste("Ustawiono tytuł rozmowy", active_id, "na:", new_title))
  }

  .history_env$conversations[[active_id]] <- conv
  # message(paste("Zapisano historię (dł:", length(conv$history), ") dla", active_id)) # Mniej gadatliwe

  return(new_title_generated)
}


#' Pobieranie tytułu konwersacji
#' @param id ID konwersacji.
#' @return Tytuł konwersacji lub NULL.
#' @export
get_conversation_title <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]]$title %||% "[Brak tytułu]")
}

#' Pobieranie listy ID wszystkich konwersacji
#' @return Wektor znakowy z ID.
#' @export
get_all_conversation_ids <- function() {
  names(.history_env$conversations)
}

#' Resetowanie menedżera historii
#' @export
reset_history_manager <- function() {
  message("Resetowanie menedżera historii...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  invisible(NULL)
}

# --- FUNKCJE ZWIĄZANE Z ZAŁĄCZNIKAMI ---

#' Dodawanie załącznika do aktywnej konwersacji
#' @param name Nazwa pliku.
#' @param content Treść pliku.
#' @return TRUE jeśli dodano, FALSE jeśli plik istnieje.
#' @export
add_attachment_to_active_conversation <- function(name, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) stop("Nie można dodać załącznika, brak aktywnej konwersacji.")
  if (!active_id %in% names(.history_env$conversations)) stop("Aktywna konwersacja (ID: ", active_id, ") już nie istnieje.")

  conv <- .history_env$conversations[[active_id]]
  conv$attachments <- conv$attachments %||% list()

  if (any(sapply(conv$attachments, function(att) att$name == name))) {
    message(paste("Plik", name, "już istnieje w konwersacji", active_id))
    return(FALSE)
  }

  new_attachment <- list(name = name, content = content)
  conv$attachments <- c(conv$attachments, list(new_attachment))
  message(paste("Dodano załącznik", name, "do konwersacji", active_id))

  .history_env$conversations[[active_id]] <- conv
  # message(paste("Zapisano załączniki (liczba:", length(conv$attachments), ") dla", active_id)) # Mniej gadatliwe

  return(TRUE)
}

#' Pobieranie załączników dla aktywnej konwersacji
#' @return Lista załączników.
#' @export
get_active_conversation_attachments <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) return(list())
  return(active_conv$attachments %||% list())
}

# Helper %||%
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
