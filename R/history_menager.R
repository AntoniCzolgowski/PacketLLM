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
#' Tworzy pierwszą, pustą konwersację i ustawia ją jako aktywną.
#' Wywoływana na starcie serwera gadżetu.
#' @return ID nowo utworzonej, aktywnej konwersacji.
initialize_history_manager <- function() {
  message("Inicjalizuję menedżera historii...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  new_id <- create_new_conversation(activate = TRUE, add_initial_system_msg = TRUE)
  message(paste("Menedżer historii zainicjalizowany. Aktywna rozmowa:", new_id))
  return(new_id)
}

#' Tworzenie nowej konwersacji
#'
#' Tworzy nowy obiekt konwersacji, dodaje go do listy i opcjonalnie ustawia jako aktywny.
#'
#' @param activate Czy ustawić nową konwersację jako aktywną? (Domyślnie TRUE)
#' @param add_initial_system_msg Czy dodać domyślny komunikat systemowy? (Domyślnie TRUE)
#' @param title Początkowy tytuł konwersacji (Domyślnie "Nowa rozmowa [czas]")
#' @return ID nowo utworzonej konwersacji.
#' @export
create_new_conversation <- function(activate = TRUE, add_initial_system_msg = TRUE, title = NULL) {
  conv_id <- generate_conversation_id()
  if (is.null(title)) {
    title <- paste("Nowa rozmowa", format(Sys.time(), "%H:%M:%S"))
  }
  message(paste("Tworzę nową konwersację o ID:", conv_id, "i tytule:", title))

  new_conv <- list(
    id = conv_id,
    title = title,
    history = list(),
    attachments = list(), # Lista załączników dla tej konwersacji
    created_at = Sys.time()
  )

  if (add_initial_system_msg) {
    default_system_message <- "Jesteś pomocnym asystentem. Odpowiadaj w sposób czytelny i precyzyjny, zachowując formatowanie kodu, gdy jest to wymagane."
    new_conv$history <- list(list(role = "system", content = default_system_message))
    message(paste("Dodano domyślny komunikat systemowy do rozmowy", conv_id))
  }

  # Bezpośrednie przypisanie nowego obiektu do listy w środowisku
  .history_env$conversations[[conv_id]] <- new_conv

  if (activate) {
    set_active_conversation(conv_id)
  }
  return(conv_id)
}

#' Ustawianie aktywnej konwersacji
#'
#' @param id ID konwersacji do aktywacji.
#' @return Invisible NULL. Zgłasza błąd, jeśli ID nie istnieje.
#' @export
set_active_conversation <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    stop("Nie znaleziono konwersacji o ID: ", id)
  }
  .history_env$active_conversation_id <- id
  message(paste("Ustawiono aktywną konwersację na:", id))
  invisible(NULL)
}

#' Pobieranie ID aktywnej konwersacji
#' @return ID aktywnej konwersacji lub NULL, jeśli żadna nie jest aktywna.
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
    message("Ostrzeżenie: Brak aktywnej konwersacji lub ID nie istnieje w get_active_conversation()")
    return(NULL)
  }
  # Zwracamy obiekt bezpośrednio z listy środowiska
  return(.history_env$conversations[[active_id]])
}

#' Pobieranie historii czatu dla aktywnej konwersacji
#' @return Lista wiadomości ({role, content}) dla aktywnej konwersacji lub pusta lista.
#' @export
get_active_chat_history <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    message("Ostrzeżenie: Brak aktywnej konwersacji w get_active_chat_history()")
    return(list())
  }
  # Używamy %||% dla bezpieczeństwa, gdyby pole $history nie istniało
  return(active_conv$history %||% list())
}

#' Dodawanie wiadomości do historii aktywnej konwersacji
#'
#' !! ZMIANA: Pobiera obiekt konwersacji, modyfikuje go i zapisuje z powrotem !!
#'
#' @param role Rola ("user", "assistant", "system").
#' @param content Treść wiadomości.
#' @return Invisible NULL. Zgłasza błąd, jeśli żadna konwersacja nie jest aktywna.
#' @export
add_message_to_active_history <- function(role, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    stop("Nie można dodać wiadomości, żadna konwersacja nie jest aktywna.")
  }
  if (!active_id %in% names(.history_env$conversations)) {
    stop("Nie można dodać wiadomości, aktywna konwersacja (ID: ", active_id, ") nie istnieje.")
  }
  if (!role %in% c("user", "assistant", "system")) {
    stop("Nieprawidłowa rola wiadomości: ", role)
  }

  # Pobierz aktualny obiekt konwersacji
  conv <- .history_env$conversations[[active_id]]

  # Dodaj nową wiadomość do historii w pobranym obiekcie
  new_message <- list(role = role, content = content)
  conv$history <- c(conv$history %||% list(), list(new_message)) # Użyj %||% na wypadek, gdyby historia była NULL

  # Aktualizacja tytułu (jeśli to pierwsza wiadomość użytkownika)
  is_first_user_message <- role == "user" && sum(sapply(conv$history, function(m) m$role == "user")) == 1 # Sprawdzamy czy jest dokładnie 1
  if (is_first_user_message && nzchar(trimws(content))) {
    new_title <- substr(trimws(content), 1, 35)
    if (nchar(trimws(content)) > 35) new_title <- paste0(new_title, "...")
    conv$title <- new_title # Zaktualizuj tytuł w pobranym obiekcie
    message(paste("Ustawiono tytuł rozmowy", active_id, "na:", new_title))
  }

  # !! WAŻNE: Zapisz zmodyfikowany obiekt konwersacji z powrotem do środowiska !!
  .history_env$conversations[[active_id]] <- conv
  message(paste("Zapisano zaktualizowaną historię (długość:", length(conv$history), ") dla konwersacji", active_id))

  invisible(NULL)
}

#' Aktualizacja tytułu konwersacji
#'
#' !! ZMIANA: Pobiera obiekt konwersacji, modyfikuje go i zapisuje z powrotem !!
#'
#' @param id ID konwersacji.
#' @param title Nowy tytuł.
#' @return Invisible NULL. Zgłasza błąd, jeśli ID nie istnieje.
#' @export
update_conversation_title <- function(id, title) {
  if (!id %in% names(.history_env$conversations)) {
    stop("Nie znaleziono konwersacji o ID: ", id, " do aktualizacji tytułu.")
  }
  # Pobierz obiekt
  conv <- .history_env$conversations[[id]]
  # Zmodyfikuj
  conv$title <- title
  # Zapisz z powrotem
  .history_env$conversations[[id]] <- conv
  invisible(NULL)
}

#' Pobieranie listy konwersacji do wyświetlenia
#'
#' Zwraca listę, gdzie każdy element to lista z `id` i `title`.
#' Sortuje wg czasu utworzenia (najnowsze na początku).
#' @return Lista konwersacji (każda to list(id, title)).
#' @export
get_conversation_list_for_display <- function() {
  convs <- .history_env$conversations
  if (length(convs) == 0) {
    return(list())
  }
  creation_times <- sapply(convs, function(c) c$created_at %||% Sys.time()) # Dodano %||%
  sorted_indices <- order(creation_times, decreasing = TRUE)
  sorted_convs <- convs[sorted_indices]

  lapply(sorted_convs, function(conv) {
    list(id = conv$id %||% "BRAK_ID", title = conv$title %||% "Brak tytułu") # Dodano %||%
  })
}

#' Resetowanie całego menedżera historii (na wszelki wypadek)
#' @export
reset_history_manager <- function() {
  message("Resetowanie menedżera historii...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  invisible(NULL)
}

# --- FUNKCJE ZWIĄZANE Z ZAŁĄCZNIKAMI KONWERSACJI ---

#' Dodawanie załącznika do aktywnej konwersacji
#'
#' !! ZMIANA: Pobiera obiekt konwersacji, modyfikuje go i zapisuje z powrotem !!
#' Dodaje plik (nazwę i treść) do listy załączników *całej* aktywnej konwersacji.
#' Sprawdza, czy plik o tej nazwie już nie istnieje w danej konwersacji.
#'
#' @param name Nazwa pliku.
#' @param content Treść pliku jako pojedynczy string.
#' @return TRUE jeśli dodano, FALSE jeśli plik o tej nazwie już istnieje. Zgłasza błąd, jeśli brak aktywnej konwersacji.
#' @export
add_attachment_to_active_conversation <- function(name, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    stop("Nie można dodać załącznika, żadna konwersacja nie jest aktywna.")
  }
  if (!active_id %in% names(.history_env$conversations)) {
    stop("Nie można dodać załącznika, aktywna konwersacja (ID: ", active_id, ") nie istnieje.")
  }

  # Pobierz obiekt konwersacji
  conv <- .history_env$conversations[[active_id]]
  # Upewnij się, że pole attachments istnieje
  conv$attachments <- conv$attachments %||% list()

  # Sprawdź, czy plik o tej nazwie już istnieje w tej konwersacji
  if (any(sapply(conv$attachments, function(att) att$name == name))) {
    message(paste("Plik o nazwie", name, "już istnieje w konwersacji", active_id))
    return(FALSE)
  }

  # Dodaj nowy załącznik do pobranego obiektu
  new_attachment <- list(name = name, content = content)
  conv$attachments <- c(conv$attachments, list(new_attachment))
  message(paste("Dodano załącznik", name, "do obiektu konwersacji", active_id))

  # !! WAŻNE: Zapisz zmodyfikowany obiekt konwersacji z powrotem do środowiska !!
  .history_env$conversations[[active_id]] <- conv
  message(paste("Zapisano zaktualizowane załączniki (liczba:", length(conv$attachments), ") dla konwersacji", active_id))

  return(TRUE)
}

#' Pobieranie załączników dla aktywnej konwersacji
#'
#' Zwraca listę załączników (każdy to `list(name=..., content=...)`) dla aktywnej konwersacji.
#'
#' @return Lista załączników lub pusta lista.
#' @export
get_active_conversation_attachments <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    message("Ostrzeżenie: Brak aktywnej konwersacji w get_active_conversation_attachments()")
    return(list())
  }
  # Zwróć pustą listę jeśli pole attachments nie istnieje lub jest NULL
  return(active_conv$attachments %||% list())
}

# Helper do zwracania wartości domyślnej jeśli x jest NULL
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
