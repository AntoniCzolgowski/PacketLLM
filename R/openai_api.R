# openai_api.R

#' Lista dostępnych modeli OpenAI do wyboru w UI
#' @export
available_openai_models <- c("gpt-4o", "gpt-4o-mini", "o1", "o3-mini")

#' Modele, które nie obsługują dodatkowych parametrów (np. temperatury)
#' @noRd
simplified_models_list <- c("o1", "o3-mini")

#' Sprawdzenie klucza API
#'
#' Ta funkcja sprawdza, czy w zmiennych środowiskowych (np. w pliku .Renviron)
#' znajduje się klucz API przypisany do zmiennej `OPENAI_API_KEY`.
#' Jeśli klucz nie jest ustawiony, funkcja zgłosi błąd i przerwie działanie.
#' Jeśli klucz jest ustawiony, zwraca TRUE.
#'
#' @return Zwraca TRUE, jeśli klucz API jest ustawiony; w przeciwnym razie przerywa działanie.
#' @export
check_api_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(key)) {
    stop("Brak klucza API w .Renviron. Ustaw zmienną OPENAI_API_KEY i zrestartuj R.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Wywołanie API OpenAI
#'
#' Funkcja wysyła historię konwersacji do API OpenAI i zwraca odpowiedź modelu.
#' Dla modeli z `simplified_models_list` NIE wysyła parametru `temperature`.
#'
#' @param messages Lista wiadomości (każda wiadomość to lista z polami 'role' i 'content').
#' @param model Model OpenAI do użycia.
#' @param temperature Parametr temperatury (używany tylko dla modeli, które go obsługują).
#'
#' @return Łańcuch znaków zawierający odpowiedź modelu.
#' @export
call_openai_chat <- function(messages, model, temperature = 0.5) {
  check_api_key()

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Pakiet 'httr' jest wymagany do wywoływania API. Zainstaluj go.", call. = FALSE)
  }

  url <- "https://api.openai.com/v1/chat/completions"

  # ZMIANA: Tworzenie podstawowego payloadu
  payload <- list(
    model = model,
    messages = messages
    # Na razie bez temperatury
  )

  # ZMIANA: Warunkowe dodanie temperatury tylko dla obsługiwanych modeli
  if (!model %in% simplified_models_list) {
    payload$temperature <- temperature
    message(paste("API Call: Dołączam parametr temperature =", temperature, "dla modelu", model))
  } else {
    message(paste("API Call: Pomijam parametr temperature dla modelu", model))
  }

  # Wywołanie API z przygotowanym payloadem
  response <- httr::POST(
    url,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
    ),
    body = payload, # Użyj przygotowanego payloadu
    encode = "json",
    httr::timeout(1200)
  )

  # Obsługa błędów (bez zmian)
  if (httr::http_error(response)) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("Błąd podczas wywoływania API: ", httr::http_status(response)$message, "\nTreść błędu: ", error_content, call. = FALSE)
  }

  content_response <- httr::content(response, "parsed")

  if (!is.null(content_response$error)) {
    stop("Błąd API: ", content_response$error$message, call. = FALSE)
  }

  if (is.null(content_response$choices) || length(content_response$choices) == 0 ||
      is.null(content_response$choices[[1]]$message) || is.null(content_response$choices[[1]]$message$content)) {
    print(content_response)
    stop("Nieoczekiwana struktura odpowiedzi z API OpenAI.", call. = FALSE)
  }

  return(content_response$choices[[1]]$message$content)
}

# Helper %||% (bez zmian)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
