# Utworzenie prywatnego środowiska do przechowywania historii konwersacji oraz załączników
.chat_env <- new.env(parent = emptyenv())
.chat_env$history <- list()
.chat_env$attachments <- list()  # Lista załączników (dla bieżącego zapytania)

#' Inicjalizacja nowej rozmowy
#'
#' Funkcja resetuje historię konwersacji oraz listę załączników dla bieżącego zapytania
#' i inicjuje nową rozmowę z domyślnym komunikatem systemowym.
#'
#' @return Invisible NULL
#' @export
new_chat <- function() {
  .chat_env$history <- list()
  .chat_env$attachments <- list()  # Reset listy załączników dla bieżącego zapytania
  default_system_message <- "Jesteś pomocnym asystentem. Odpowiadaj w sposób czytelny i precyzyjny, zachowując formatowanie kodu, gdy jest to wymagane."
  # Zawsze zaczynaj historię od wiadomości systemowej
  .chat_env$history <- list(list(role = "system", content = default_system_message))
  invisible(NULL)
}

#' Dodanie wiadomości użytkownika
#'
#' Funkcja dodaje wiadomość użytkownika do historii konwersacji.
#'
#' @param text Tekst wiadomości użytkownika (łańcuch znaków).
#' @return Invisible NULL
#' @export
add_user_message <- function(text) {
  if (!is.character(text) || length(text) != 1) {
    stop("Tekst wiadomości musi być pojedynczym łańcuchem znaków.")
  }
  # Dodaj wiadomość użytkownika do historii
  .chat_env$history[[length(.chat_env$history) + 1]] <- list(role = "user", content = text)
  invisible(NULL)
}

#' Dołączenie treści pliku do bieżącego zapytania
#'
#' Funkcja zapisuje załączony plik (nazwę i treść) w liście załączników
#' dla bieżącego zapytania (`.chat_env$attachments`). Ta lista zostanie użyta
#' przy następnym wywołaniu `get_assistant_response` i następnie wyczyszczona.
#'
#' @param file_name Nazwa załączonego pliku.
#' @param file_content Treść pliku (łańcuch znaków).
#' @return Invisible NULL
#' @export
attach_file <- function(file_name, file_content) {
  if (!is.character(file_name) || length(file_name) != 1) {
    stop("file_name musi być pojedynczym łańcuchem znaków.")
  }
  if (!is.character(file_content) || length(file_content) != 1) {
    stop("file_content musi być pojedynczym łańcuchem znaków.")
  }
  # Zapisujemy załącznik w liście dla bieżącego zapytania
  .chat_env$attachments[[length(.chat_env$attachments) + 1]] <- list(name = file_name, content = file_content)
  message(paste("Załączono plik:", file_name)) # Komunikat w konsoli R
  invisible(NULL)
}

#' Pobranie historii konwersacji
#'
#' Funkcja zwraca pełną historię konwersacji przechowywaną w `.chat_env$history`.
#'
#' @return Lista wiadomości (każda wiadomość to lista z polami 'role' i 'content').
#' @export
get_chat_history <- function() {
  return(.chat_env$history)
}

#' Pobranie listy nazw załączonych plików dla bieżącego zapytania
#'
#' Funkcja zwraca wektor nazw plików aktualnie przechowywanych w `.chat_env$attachments`.
#'
#' @return Wektor znakowy z nazwami plików lub pusty wektor.
#' @export
get_current_attachment_names <- function() {
  if (length(.chat_env$attachments) > 0) {
    return(sapply(.chat_env$attachments, function(att) att$name))
  } else {
    return(character(0))
  }
}


#' Pobranie odpowiedzi asystenta
#'
#' Funkcja wysyła aktualną historię konwersacji (`.chat_env$history`)
#' wraz z treścią plików z listy bieżących załączników (`.chat_env$attachments`)
#' do API OpenAI. Otrzymana odpowiedź asystenta jest dodawana do historii.
#' Lista bieżących załączników (`.chat_env$attachments`) jest czyszczona po wysłaniu zapytania.
#'
#' @param model Model OpenAI do użycia (domyślnie "gpt-4o").
#' @param temperature Parametr temperatury dla API (domyślnie 0.5).
#' @return Tekst odpowiedzi asystenta.
#' @export
get_assistant_response <- function(model = "gpt-4o", temperature = 0.5) {
  # Jeśli historia jest pusta (np. błąd), zainicjuj ją
  if (length(.chat_env$history) == 0) {
    warning("Historia czatu była pusta. Inicjalizuję nową rozmowę.")
    new_chat()
  }

  # Sprawdź, czy ostatnia wiadomość to wiadomość użytkownika. Jeśli nie, dodaj pustą.
  # Jest to potrzebne, aby mieć gdzie dokleić treść załączników.
  last_msg_index <- length(.chat_env$history)
  if (last_msg_index == 0 || .chat_env$history[[last_msg_index]]$role != "user") {
    # Można dodać pustą wiadomość użytkownika lub wiadomość informacyjną
    add_user_message("(Użytkownik pyta na podstawie załączonych plików)")
    last_msg_index <- length(.chat_env$history) # Zaktualizuj indeks
  }


  # Przygotowujemy kopię historii, którą wyślemy do API
  conversation_for_api <- .chat_env$history

  # Jeśli są załączniki w bieżącym zapytaniu, dołączamy ich treść do ostatniej wiadomości użytkownika w kopii
  if (!is.null(.chat_env$attachments) && length(.chat_env$attachments) > 0) {
    file_names <- sapply(.chat_env$attachments, function(att) att$name)
    message(paste("Wysyłanie zapytania z załącznikami:", paste(file_names, collapse = ", "))) # Komunikat w konsoli R

    # Łączymy pełne treści załączników – każdy poprzedzony nazwą pliku
    attachment_full_text <- ""
    for (att in .chat_env$attachments) {
      attachment_full_text <- paste0(attachment_full_text, "\n\n--- ZAŁĄCZONY PLIK: ", att$name, " ---\n", att$content)
    }

    # Dołączamy treść załączników do ostatniej wiadomości użytkownika w kopii historii wysyłanej do API
    # Upewniamy się, że ostatnia wiadomość jest od użytkownika (powinno tak być po sprawdzeniu wyżej)
    if (conversation_for_api[[last_msg_index]]$role == "user") {
      conversation_for_api[[last_msg_index]]$content <- paste0(
        conversation_for_api[[last_msg_index]]$content,
        "\n\n--- TREŚĆ ZAŁĄCZONYCH PLIKÓW ---\n", # Dodajemy wyraźny separator
        attachment_full_text
      )
    } else {
      # Ten przypadek nie powinien się zdarzyć po dodaniu sprawdzenia powyżej, ale na wszelki wypadek
      warning("Nie znaleziono wiadomości użytkownika na końcu historii do dołączenia plików. Pliki mogą nie zostać poprawnie przetworzone.")
      # Można by też dodać nową wiadomość 'user' z samymi plikami, ale to może zaburzyć kontekst.
    }
  } else {
    message("Wysyłanie zapytania bez nowych załączników.") # Komunikat w konsoli R
  }

  # Wywołujemy API OpenAI, przekazując kopię historii z potencjalnie dołączonymi treściami załączników
  response_text <- tryCatch({
    call_openai_chat(conversation_for_api, model = model, temperature = temperature)
  }, error = function(e) {
    # Obsługa błędu API - zwracamy komunikat błędu jako odpowiedź "asystenta"
    error_message <- paste("Błąd API:", e$message)
    warning(error_message) # Zapisz błąd w logach R
    return(error_message) # Zwróć użytkownikowi informację o błędzie
  })


  # --- SEKCJA MODYFIKOWANA ---
  # Aktualizacja widocznej historii: Czy dodawać informację o załącznikach?
  # Komentujemy tę część, ponieważ UI będzie teraz pokazywać listę plików *przed* wysłaniem,
  # a lista ta zniknie *po* wysłaniu. Dodawanie tego do historii może być zbędne.
  # if (!is.null(.chat_env$attachments) && length(.chat_env$attachments) > 0) {
  #   file_names <- sapply(.chat_env$attachments, function(att) att$name)
  #   # Dodajemy wiadomość systemową informującą, jakie pliki *były* dołączone do tego konkretnego zapytania
  #   .chat_env$history[[length(.chat_env$history) + 1]] <- list(
  #     role = "system",
  #     content = paste("Do zapytania dołączono pliki:", paste(file_names, collapse = ", "))
  #   )
  # }
  # --- KONIEC SEKCJI MODYFIKOWANEJ ---


  # Dodajemy odpowiedź asystenta do widocznej historii (nawet jeśli to komunikat błędu)
  .chat_env$history[[length(.chat_env$history) + 1]] <- list(role = "assistant", content = response_text)

  # Czyszczenie listy załączników dla bieżącego zapytania – *po* wysłaniu zapytania i przetworzeniu odpowiedzi
  # Niezależnie od tego, czy były załączniki, czy nie, czy wystąpił błąd API
  .chat_env$attachments <- list()
  message("Lista załączników dla bieżącego zapytania wyczyszczona.") # Komunikat w konsoli R

  return(response_text)
}
