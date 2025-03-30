# chat_logic.R

#' Dodanie wiadomości użytkownika do aktywnej konwersacji
#'
#' @param text Tekst wiadomości użytkownika.
#' @return Invisible NULL
#' @export
add_user_message <- function(text) {
  if (!is.character(text) || length(text) != 1) {
    stop("Tekst wiadomości musi być pojedynczym łańcuchem znaków.")
  }
  add_message_to_active_history(role = "user", content = text)
  invisible(NULL)
}

#' Pobranie odpowiedzi asystenta dla aktywnej konwersacji
#'
#' Wysyła historię, załączniki ORAZ używa specyficznej dla konwersacji
#' temperatury i komunikatu systemowego.
#'
#' @param model Model OpenAI do użycia (domyślnie "gpt-4o").
#' @return Tekst odpowiedzi asystenta lub komunikat o błędzie.
#' @export
get_assistant_response <- function(model = "gpt-4o") { # Usunięto argument temperature z sygnatury
  # Pobieramy cały obiekt aktywnej konwersacji
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    error_content <- "Błąd krytyczny: Brak aktywnej konwersacji do przetworzenia."
    warning(error_content)
    # Nie można dodać do historii, bo nie ma aktywnej konwersacji
    return(error_content)
  }

  conversation_history <- active_conv$history %||% list()
  attachments <- active_conv$attachments %||% list()
  # ZMIANA: Pobranie ustawień specyficznych dla konwersacji
  conversation_temp <- active_conv$temperature %||% 0.5 # Domyślna 0.5 jeśli NULL
  conversation_system_message <- active_conv$system_message %||% "Jesteś pomocnym asystentem." # Proste domyślne jeśli NULL

  # Przygotowujemy kopię historii, którą wyślemy do API
  api_messages <- conversation_history

  # Sprawdzamy, czy historia zawiera jakąkolwiek wiadomość systemową na początku
  # Jeśli nie (co nie powinno się zdarzyć przy obecnej logice), dodajemy ją
  if (length(api_messages) == 0 || api_messages[[1]]$role != "system") {
    message("Ostrzeżenie: Historia czatu nie zaczyna się od wiadomości systemowej. Dodaję teraz.")
    api_messages <- c(list(list(role = "system", content = conversation_system_message)), api_messages)
  } else {
    # ZMIANA: Aktualizujemy *pierwszą* wiadomość systemową w kopii API
    # na tę z ustawień konwersacji, ZANIM dodamy kontekst plików
    api_messages[[1]]$content <- conversation_system_message
    message("Używam komunikatu systemowego specyficznego dla konwersacji.")
  }


  # Modyfikujemy *pierwszą* wiadomość (systemową) w kopii, dodając treść załączników
  if (length(attachments) > 0) {
    message("Przygotowuję zapytanie do API z kontekstem ", length(attachments), " załączników.")
    attachments_text <- ""
    for (att in attachments) {
      attachments_text <- paste0(
        attachments_text,
        "\n\n--- POCZĄTEK ZAŁĄCZNIKA: ", att$name, " ---\n",
        att$content,
        "\n--- KONIEC ZAŁĄCZNIKA: ", att$name, " ---"
      )
    }

    # Pobieramy bazową treść wiadomości systemowej (już zaktualizowaną powyżej)
    base_system_content <- api_messages[[1]]$content

    # Doklejamy kontekst plików
    api_messages[[1]]$content <- paste0(
      base_system_content, # Używamy już tej z ustawień
      "\n\n--- KONTEKST ZAŁĄCZONYCH PLIKÓW (DOSTĘPNY DLA CIEBIE W TEJ ROZMOWIE) ---",
      attachments_text,
      "\n--- KONIEC KONTEKSTU ZAŁĄCZONYCH PLIKÓW ---"
    )
    message("Dołączono treść załączników do wiadomości systemowej dla API.")
  } else {
    message("Wysyłanie zapytania do API bez dodatkowego kontekstu plików.")
  }

  # Sprawdź, czy ostatnia wiadomość jest od użytkownika (bez zmian)
  last_msg_index <- length(api_messages)
  if (last_msg_index == 0 || (last_msg_index > 0 && api_messages[[last_msg_index]]$role != "user")) {
    placeholder_text <- "(Użytkownik oczekuje na odpowiedź na podstawie dostępnego kontekstu)"
    api_messages[[length(api_messages) + 1]] <- list(role = "user", content = placeholder_text)
    message("Dodano zastępczą wiadomość użytkownika na potrzeby wywołania API.")
  }


  # Wywołujemy API OpenAI, przekazując zmodyfikowaną historię i temperaturę
  response_text <- tryCatch({
    # ZMIANA: Przekazanie temperatury z konwersacji
    call_openai_chat(api_messages, model = model, temperature = conversation_temp)
  }, error = function(e) {
    error_message <- paste("Błąd API:", e$message)
    warning(error_message)
    # Dodajemy błąd API jako wiadomość systemową do WIDOCZNEJ historii
    # Sprawdźmy czy konwersacja nadal istnieje
    if (!is.null(get_active_conversation_id())) {
      tryCatch(add_message_to_active_history(role = "system", content = error_message), error=function(e2){})
    }
    return(error_message) # Zwracamy błąd
  })

  # Jeśli nie było błędu API, dodajemy odpowiedź asystenta do WIDOCZNEJ historii
  if (!grepl("^Błąd API:", response_text)) {
    # Sprawdźmy czy konwersacja nadal istnieje
    if (!is.null(get_active_conversation_id())) {
      add_message_to_active_history(role = "assistant", content = response_text)
    } else {
      warning("Otrzymano odpowiedź API, ale aktywna konwersacja już nie istnieje.")
    }
  }

  return(response_text)
}
