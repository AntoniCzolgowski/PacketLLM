# chat_logic.R

#' Dodanie wiadomości użytkownika do aktywnej konwersacji
#'
#' Funkcja dodaje wiadomość użytkownika do historii aktywnej konwersacji,
#' używając `history_manager`. Jeśli jest to pierwsza wiadomość użytkownika,
#' tytuł konwersacji zostanie automatycznie zaktualizowany.
#'
#' @param text Tekst wiadomości użytkownika (łańcuch znaków).
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
#' Funkcja wysyła aktualną historię aktywnej konwersacji
#' wraz z treścią *wszystkich* załączników dołączonych do tej konwersacji
#' (wplecionych w wiadomość systemową) do API OpenAI.
#' Otrzymana odpowiedź asystenta jest dodawana do historii aktywnej konwersacji.
#'
#' @param model Model OpenAI do użycia (domyślnie "gpt-4o").
#' @param temperature Parametr temperatury dla API (domyślnie 0.5).
#' @return Tekst odpowiedzi asystenta lub komunikat o błędzie.
#' @export
get_assistant_response <- function(model = "gpt-4o", temperature = 0.5) {
  # Pobieramy historię i załączniki aktualnie aktywnej konwersacji
  conversation_history <- get_active_chat_history()
  attachments <- get_active_conversation_attachments()

  # Sprawdzamy, czy historia nie jest pusta i czy zawiera komunikat systemowy
  if (length(conversation_history) == 0 || conversation_history[[1]]$role != "system") {
    error_content <- "Błąd krytyczny: Aktywna historia czatu jest pusta lub nie zaczyna się od komunikatu systemowego."
    warning(error_content)
    # Dodajemy błąd do historii (jeśli to możliwe) i zwracamy
    tryCatch(add_message_to_active_history("system", error_content), error = function(e) {})
    return(error_content)
  }

  # Przygotowujemy kopię historii, którą wyślemy do API
  api_messages <- conversation_history

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

    # Oryginalna treść wiadomości systemowej
    original_system_content <- api_messages[[1]]$content

    # Nowa treść wiadomości systemowej z dołączonym kontekstem plików
    api_messages[[1]]$content <- paste0(
      original_system_content,
      "\n\n--- KONTEKST ZAŁĄCZONYCH PLIKÓW (DOSTĘPNY DLA CIEBIE W TEJ ROZMOWIE) ---",
      attachments_text,
      "\n--- KONIEC KONTEKSTU ZAŁĄCZONYCH PLIKÓW ---"
    )
    message("Dołączono treść załączników do wiadomości systemowej dla API.")
  } else {
    message("Wysyłanie zapytania do API bez dodatkowego kontekstu plików.")
  }

  # Sprawdź, czy ostatnia wiadomość w historii (przed wysłaniem) jest od użytkownika.
  # Jeśli nie (np. wysyłamy zaraz po dodaniu pliku, bez tekstu), dodaj wiadomość zastępczą.
  last_msg_index <- length(api_messages)
  if (last_msg_index == 0 || api_messages[[last_msg_index]]$role != "user") {
    # Dodajemy wiadomość zastępczą TYLKO do historii wysyłanej do API
    # Nie dodajemy jej do faktycznej historii konwersacji
    placeholder_text <- "(Użytkownik kontynuuje rozmowę na podstawie dostępnego kontekstu i załączników)"
    api_messages[[length(api_messages) + 1]] <- list(role = "user", content = placeholder_text)
    message("Dodano zastępczą wiadomość użytkownika na potrzeby wywołania API.")
  }


  # Wywołujemy API OpenAI, przekazując zmodyfikowaną historię
  response_text <- tryCatch({
    call_openai_chat(api_messages, model = model, temperature = temperature)
  }, error = function(e) {
    error_message <- paste("Błąd API:", e$message)
    warning(error_message)
    # Dodajemy błąd API jako wiadomość systemową do WIDOCZNEJ historii
    add_message_to_active_history(role = "system", content = error_message)
    return(error_message) # Zwracamy błąd
  })

  # Jeśli nie było błędu API, dodajemy odpowiedź asystenta do WIDOCZNEJ historii
  if (!grepl("^Błąd API:", response_text)) {
    add_message_to_active_history(role = "assistant", content = response_text)
  }

  return(response_text)
}
