#' Sprawdzenie klucza API
#'
#' Ta funkcja sprawdza, czy w zmiennych środowiskowych (np. w pliku .Renviron)
#' znajduje się klucz API przypisany do zmiennej `OPENAI_API_KEY`.
#' Jeśli klucz nie jest ustawiony, funkcja zgłosi błąd i przerwie działanie.
#' Jeśli klucz jest ustawiony, wyświetli komunikat potwierdzający i zwróci TRUE.
#'
#' @return Zwraca TRUE, jeśli klucz API jest ustawiony; w przeciwnym razie przerywa działanie.
#' @export
check_api_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(key)) {
    stop("Brak klucza API w .Renviron. Ustaw zmienną OPENAI_API_KEY i zrestartuj R.", call. = FALSE)
  }
  message("Klucz API jest ustawiony.")
  invisible(TRUE)
}

#' Wywołanie API OpenAI
#'
#' Funkcja wysyła historię konwersacji do API OpenAI i zwraca odpowiedź modelu.
#'
#' @param messages Lista wiadomości (każda wiadomość to lista z polami 'role' i 'content').
#' @param model Model OpenAI do użycia (np. "gpt-4o").
#' @param temperature Parametr temperatury (np. 0.5).
#'
#' @return Łańcuch znaków zawierający odpowiedź modelu.
#' @export
call_openai_chat <- function(messages, model = "gpt-4o", temperature = 0.5) {
  # Upewnij się, że klucz API jest ustawiony
  check_api_key()

  # Upewnij się, że pakiet httr jest dostępny
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Pakiet 'httr' jest wymagany do wywoływania API. Zainstaluj go.", call. = FALSE)
  }

  url <- "https://api.openai.com/v1/chat/completions"

  # Przygotuj ładunek JSON zgodnie z dokumentacją API
  payload <- list(
    model = model,
    messages = messages,
    temperature = temperature
  )

  response <- httr::POST(
    url,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
    ),
    body = payload,
    encode = "json",
    httr::timeout(120)  # ustaw timeout na 120 sekund
  )


  if (httr::http_error(response)) {
    stop("Błąd podczas wywoływania API: ", httr::http_status(response)$message, call. = FALSE)
  }

  content_response <- httr::content(response, "parsed")

  if (!is.null(content_response$error)) {
    stop("Błąd API: ", content_response$error$message, call. = FALSE)
  }

  # Zakładamy, że odpowiedź znajduje się w choices[[1]]$message$content
  return(content_response$choices[[1]]$message$content)
}
