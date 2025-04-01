# chat_logic.R

#' Dodanie wiadomości użytkownika do aktywnej konwersacji
#'
#' Wywołuje add_message_to_active_history, która blokuje model po pierwszej wiadomości.
#'
#' @param text Tekst wiadomości użytkownika.
#' @return Invisible NULL
#' @export
add_user_message <- function(text) {
  if (!is.character(text) || length(text) != 1) {
    stop("Tekst wiadomości musi być pojedynczym łańcuchem znaków.")
  }
  # Wywołanie funkcji z history_manager, która obsługuje aktywną konwersację i blokadę modelu
  add_message_to_active_history(role = "user", content = text)
  invisible(NULL)
}

#' Pobranie odpowiedzi asystenta dla aktywnej konwersacji
#'
#' Wysyła historię do API. Dla modeli `o1` i `o3-mini` wysyła tylko role
#' 'user' i 'assistant', ignoruje temperaturę, komunikat systemowy i załączniki.
#' Dla pozostałych modeli używa pełnych ustawień konwersacji.
#'
#' @return Tekst odpowiedzi asystenta lub komunikat o błędzie.
#' @export
get_assistant_response <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    error_content <- "Błąd krytyczny: Brak aktywnej konwersacji do przetworzenia."
    warning(error_content)
    return(error_content)
  }

  conversation_history <- active_conv$history %||% list()
  attachments <- active_conv$attachments %||% list()
  conversation_temp <- active_conv$temperature %||% 0.5
  conversation_system_message <- active_conv$system_message %||% "Jesteś pomocnym asystentem."
  conversation_model <- active_conv$model %||% "gpt-4o"

  message(paste("Przygotowuję zapytanie do API dla modelu:", conversation_model))

  # ZMIANA: Logika warunkowa dla modeli o1/o3-mini
  simplified_models <- c("o1", "o3-mini")
  use_simplified_logic <- conversation_model %in% simplified_models

  api_messages <- list()
  temp_to_use <- conversation_temp

  if (use_simplified_logic) {
    message("Używam uproszczonej logiki dla modelu ", conversation_model, ": tylko role user/assistant, ignoruję temp, sys_msg, załączniki.")

    # Filtruj historię, zachowując tylko role user i assistant
    api_messages <- Filter(function(msg) msg$role %in% c("user", "assistant"), conversation_history)

    # Ustaw domyślną temperaturę (API może wymagać jakiejś wartości)
    temp_to_use <- 0.5 # Lub inna domyślna, np. 0

    # Sprawdź, czy po filtracji cokolwiek zostało i czy ostatnia wiadomość jest od użytkownika
    last_msg_index_simple <- length(api_messages)
    if (last_msg_index_simple == 0 || api_messages[[last_msg_index_simple]]$role != "user") {
      # Jeśli historia jest pusta lub ostatnia wiadomość nie jest od użytkownika,
      # a oczekujemy odpowiedzi, to jest to sytuacja problematyczna dla tych modeli.
      # Możemy albo dodać placeholder, albo zgłosić błąd. Dodajmy placeholder.
      placeholder_text <- "(Oczekuję na odpowiedź)" # Prostszy placeholder
      api_messages[[length(api_messages) + 1]] <- list(role = "user", content = placeholder_text)
      message("Dodano zastępczą wiadomość użytkownika dla uproszczonego modelu.")
    }

  } else {
    # Logika dla modeli standardowych (gpt-4o, gpt-4o-mini, etc.) - jak poprzednio
    message("Używam standardowej logiki dla modelu ", conversation_model)
    api_messages <- conversation_history # Zaczynamy od pełnej historii

    # Obsługa komunikatu systemowego
    if (length(api_messages) == 0 || api_messages[[1]]$role != "system") {
      message("Ostrzeżenie: Historia czatu nie zaczyna się od wiadomości systemowej. Dodaję teraz.")
      api_messages <- c(list(list(role = "system", content = conversation_system_message)), api_messages)
    } else {
      api_messages[[1]]$content <- conversation_system_message # Zawsze używaj aktualnego z ustawień
      message("Używam komunikatu systemowego specyficznego dla konwersacji.")
    }

    # Obsługa załączników (tylko dla modeli standardowych)
    if (length(attachments) > 0) {
      message("Dołączam kontekst ", length(attachments), " załączników.")
      attachments_text <- ""
      for (att in attachments) {
        attachments_text <- paste0(
          attachments_text,
          "\n\n--- POCZĄTEK ZAŁĄCZNIKA: ", att$name, " ---\n",
          att$content,
          "\n--- KONIEC ZAŁĄCZNIKA: ", att$name, " ---"
        )
      }
      base_system_content <- api_messages[[1]]$content
      api_messages[[1]]$content <- paste0(
        base_system_content,
        "\n\n--- KONTEKST ZAŁĄCZONYCH PLIKÓW (DOSTĘPNY DLA CIEBIE W TEJ ROZMOWIE) ---",
        attachments_text,
        "\n--- KONIEC KONTEKSTU ZAŁĄCZONYCH PLIKÓW ---"
      )
      message("Dołączono treść załączników do wiadomości systemowej dla API.")
    } else {
      message("Brak załączników do dołączenia.")
    }

    # Sprawdzenie ostatniej wiadomości (jak poprzednio)
    last_msg_index_standard <- length(api_messages)
    if (last_msg_index_standard == 0 || api_messages[[last_msg_index_standard]]$role != "user") {
      placeholder_text <- "(Użytkownik oczekuje na odpowiedź na podstawie dostępnego kontekstu)"
      api_messages[[length(api_messages) + 1]] <- list(role = "user", content = placeholder_text)
      message("Dodano zastępczą wiadomość użytkownika na potrzeby wywołania API.")
    }
  } # Koniec logiki warunkowej dla modeli

  # Sprawdzenie, czy mamy jakiekolwiek wiadomości do wysłania
  if(length(api_messages) == 0) {
    error_content <- "Brak historii do wysłania do API po przetworzeniu."
    warning(error_content)
    # Można dodać błąd do historii widocznej
    active_id_for_error <- get_active_conversation_id()
    if (!is.null(active_id_for_error) && active_id_for_error %in% names(.history_env$conversations)) {
      tryCatch(add_message_to_active_history(role = "system", content = paste("Błąd:", error_content)), error=function(e2){})
    }
    return(error_content)
  }


  # Wywołanie API OpenAI z przygotowanymi wiadomościami i temperaturą
  response_text <- tryCatch({
    call_openai_chat(api_messages, model = conversation_model, temperature = temp_to_use)
  }, error = function(e) {
    error_message <- paste("Błąd API:", e$message)
    warning(error_message)
    # Zapis błędu do historii (jak poprzednio)
    active_id_for_error <- get_active_conversation_id()
    if (!is.null(active_id_for_error) && active_id_for_error %in% names(.history_env$conversations)) {
      tryCatch(add_message_to_active_history(role = "system", content = error_message), error=function(e2){
        warning(paste("Nie udało się zapisać błędu API do historii:", e2$message))
      })
    }
    return(error_message)
  })

  # Dodanie odpowiedzi asystenta do historii (jak poprzednio)
  if (!grepl("^Błąd(:| API:)", response_text)) { # Poprawione sprawdzenie błędu
    active_id_for_response <- get_active_conversation_id()
    if (!is.null(active_id_for_response) && active_id_for_response %in% names(.history_env$conversations)) {
      add_message_to_active_history(role = "assistant", content = response_text)
    } else {
      warning("Otrzymano odpowiedź API, ale aktywna konwersacja już nie istnieje lub nie jest ustawiona.")
    }
  }

  return(response_text)
}

# Helper %||%
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
