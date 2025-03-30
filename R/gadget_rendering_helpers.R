# gadget_rendering_helpers.R

# Definicja lokalna lub upewnij się, że jest dostępna globalnie (np. z utils.R)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Renderuje UI dla historii czatu
#'
#' @param history Lista wiadomości (każda to lista z `role` i `content`).
#' @return Obiekt renderujący UI (wynik `renderUI`).
#' @noRd
#' @import shiny
render_chat_history_ui <- function(history) {
  renderUI({
    # Pomiń pierwszą wiadomość systemową, jeśli istnieje
    history_to_display <- if (length(history) > 0 && history[[1]]$role == "system") {
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

      # Pomiń wiadomości systemowe dodane w trakcie (np. o błędach API)
      if (role_display == "system") return(NULL)

      tags$div(
        style = paste(
          "margin-bottom: 8px; padding: 8px; border-radius: 8px;",
          if (role_display == "user") "background-color: #e1f5fe; text-align: left; margin-left: 10%; width: 90%; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);"
          else if (role_display == "assistant") "background-color: #f0f4c3; margin-right: 10%; width: 90%; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);"
          else "background-color: #eeeeee; font-style: italic; color: #555; border: 1px dashed #ccc;" # Styl dla 'unknown' lub innych
        ),
        tags$strong(ifelse(role_display == "user", "Ty:", "Asystent:")),
        tags$div(style = "white-space: pre-wrap; word-wrap: break-word;", content_display)
      )
    })
    # Usuń NULLe z listy (wiadomości systemowe)
    formatted_messages <- Filter(Negate(is.null), formatted_messages)

    tagList(formatted_messages, tags$div(style="height: 10px;")) # Dodatkowy margines na dole
  })
}

#' Renderuje UI dla listy załączników
#'
#' @param attachments_info Lista informacji o załącznikach (każda to lista z `name`).
#' @return Obiekt renderujący UI (wynik `renderUI`).
#' @noRd
#' @import shiny
render_attachments_list_ui <- function(attachments_info) {
  renderUI({
    if (length(attachments_info) == 0) {
      tags$p(tags$em("Brak plików w tej konwersacji."), style = "padding: 5px; margin: 0; color: #888;")
    } else {
      tagList(
        tags$ul(style = "margin: 0; padding-left: 15px; list-style-type: none;",
                lapply(attachments_info, function(att) {
                  tags$li(style="font-size: 0.9em; margin-bottom: 2px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
                          tags$span(class = "glyphicon glyphicon-paperclip", style="margin-right: 4px;"),
                          tags$span(title = att$name, att$name %||% "[Brak nazwy]")
                  )
                })
        )
      )
    }
  })
}
