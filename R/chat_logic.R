# Utworzenie prywatnego środowiska do przechowywania historii konwersacji
.chat_env <- new.env(parent = emptyenv())
.chat_env$history <- list()

#' Inicjalizacja nowej rozmowy
#'
#' Funkcja resetuje historię konwersacji i inicjuje nową rozmowę z domyślnym komunikatem systemowym.
#'
#' @return Invisible NULL
#' @export
new_chat <- function() {
  .chat_env$history <- list()
  default_system_message <- "Jesteś pomocnym asystentem. Odpowiadaj w sposób czytelny i precyzyjny, zachowując formatowanie kodu, gdy jest to wymagane."
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
  .chat_env$history[[length(.chat_env$history) + 1]] <- list(role = "user", content = text)
  invisible(NULL)
}

#' Dołączenie treści pliku do konwersacji
#'
#' Funkcja dodaje treść załączonego pliku do historii konwersacji.
#' Logika wczytywania i przetwarzania pliku powinna być zaimplementowana w module file_utils.R.
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
  # Budujemy wiadomość zawierającą nazwę pliku oraz jego treść.
  file_message <- paste0("Załączony plik: ", file_name, "\n\nTreść pliku:\n", file_content)
  .chat_env$history[[length(.chat_env$history) + 1]] <- list(role = "user", content = file_message)
  invisible(NULL)
}

#' Pobranie historii konwersacji
#'
#' Funkcja zwraca pełną historię konwersacji jako listę wiadomości.
#'
#' @return Lista wiadomości (każda wiadomość to lista z polami 'role' i 'content').
#' @export
get_chat_history <- function() {
  return(.chat_env$history)
}

#' Pobranie odpowiedzi asystenta
#'
#' Funkcja wysyła aktualną historię konwersacji do API OpenAI i dodaje otrzymaną odpowiedź asystenta do historii.
#'
#' @param model Model OpenAI do użycia (domyślnie "gpt-4").
#' @param temperature Parametr temperatury dla API (domyślnie 1).
#' @return Tekst odpowiedzi asystenta.
#' @export
get_assistant_response <- function(model = "gpt-4o", temperature = 0.5) {
  if (length(.chat_env$history) == 0) {
    new_chat()
  }

  # Zakładamy, że funkcja call_openai_chat() jest dostępna (zdefiniowana w openai_api.R)
  response_text <- call_openai_chat(.chat_env$history, model = model, temperature = temperature)

  # Dodajemy odpowiedź asystenta do historii
  .chat_env$history[[length(.chat_env$history) + 1]] <- list(role = "assistant", content = response_text)

  return(response_text)
}
