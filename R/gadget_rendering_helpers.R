# gadget_rendering_helpers.R

# Zakładamy, że `%||%` jest dostępne globalnie lub zdefiniowane gdzie indziej.

#' Renderuje UI dla historii czatu
#'
#' Funkcja bez zmian - poprawnie wyświetli wiadomość użytkownika z dodaną sekcją "Załączono:".
#'
#' @param history Lista wiadomości (każda to lista z `role` i `content`).
#' @return Obiekt renderujący UI (wynik `renderUI`).
#' @noRd
#' @import shiny
render_chat_history_ui <- function(history) {
  renderUI({
    # Pomiń pierwszą wiadomość systemową, jeśli istnieje
    history_to_display <- if (length(history) > 0 && !is.null(history[[1]]$role) && history[[1]]$role == "system") {
      tail(history, -1)
    } else {
      history
    }

    if (is.null(history_to_display) || length(history_to_display) == 0) {
      return(tags$p(tags$em("Rozpocznij rozmowę.")))
    }

    formatted_messages <- lapply(history_to_display, function(msg) {
      content_display <- msg$content %||% "[BRAK TREŚCI]"
      role_display <- msg$role %||% "unknown"

      # Stylizacja błędów systemowych
      if (role_display == "system" && grepl("^Błąd(:| API:| wykonania Future:| przetwarzania)", content_display, ignore.case = TRUE)) {
        tags$div(
          style = "margin-bottom: 8px; padding: 8px; border-radius: 8px; background-color: #ffebee; color: #c62828; border: 1px dashed #ef9a9a; width: 90%; margin-right: 10%; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);",
          tags$strong("Informacja systemowa:"),
          tags$div(style = "white-space: pre-wrap; word-wrap: break-word;", content_display)
        )
      } else if (role_display == "system") {
        # Ukrywamy inne wiadomości systemowe
        NULL
      } else {
        # Wiadomości użytkownika i asystenta
        tags$div(
          style = paste(
            "margin-bottom: 8px; padding: 8px; border-radius: 8px;",
            if (role_display == "user") "background-color: #e1f5fe; text-align: left; margin-left: 10%; width: 90%; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);"
            else if (role_display == "assistant") "background-color: #f0f4c3; margin-right: 10%; width: 90%; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);"
            else "background-color: #eeeeee; font-style: italic; color: #555; border: 1px dashed #ccc;" # Styl dla 'unknown'
          ),
          tags$strong(ifelse(role_display == "user", "Ty:", "Asystent:")),
          # white-space: pre-wrap; zadba o wyświetlenie nowych linii dodanych w sekcji "Załączono:"
          tags$div(style = "white-space: pre-wrap; word-wrap: break-word;", HTML(content_display))
        )
      }
    })
    formatted_messages <- Filter(Negate(is.null), formatted_messages)
    tagList(formatted_messages, tags$div(style="height: 10px;")) # Dodatkowy margines na dole
  })
}


#' Renderuje UI dla listy plików w STAGING area (przed wysłaniem)
#'
#' Zmieniono nazwę funkcji i logikę, aby przyjmować wektor nazw plików.
#' Wyświetla "Brak plików", gdy lista jest pusta.
#'
#' @param staged_files Wektor znakowy zawierający nazwy plików w stagingu.
#' @return Obiekt renderujący UI (wynik `renderUI`).
#' @noRd
#' @import shiny
render_staged_attachments_list_ui <- function(staged_files) {
  renderUI({
    # Sprawdź, czy staged_files jest NULL, pusty lub zawiera tylko puste stringi
    if (is.null(staged_files) || length(staged_files) == 0 || all(staged_files == "")) {
      tags$p(tags$em("Brak plików"), style = "padding: 5px; margin: 0; color: #888; font-size: 0.9em;")
    } else {
      # Usuń puste stringi, jeśli jakieś są
      valid_files <- staged_files[nzchar(staged_files)]
      if (length(valid_files) == 0) {
        tags$p(tags$em("Brak plików"), style = "padding: 5px; margin: 0; color: #888; font-size: 0.9em;")
      } else {
        tagList(
          tags$ul(style = "margin: 0; padding-left: 15px; list-style-type: none;",
                  lapply(valid_files, function(file_name) {
                    tags$li(style="font-size: 0.9em; margin-bottom: 2px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
                            tags$span(class = "glyphicon glyphicon-paperclip", style="margin-right: 4px;"),
                            tags$span(title = file_name, file_name) # Używamy bezpośrednio nazwy pliku
                    )
                  })
          )
        )
      }
    }
  })
}
