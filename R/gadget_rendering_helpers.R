# gadget_rendering_helpers.R


#' Renders the UI for the chat history
#'
#' Function unchanged - will correctly display the user message with the added "Attached:" section.
#'
#' @param history List of messages (each is a list with `role` and `content`).
#' @return UI rendering object (result of `renderUI`).
#' @noRd
#' @import shiny
render_chat_history_ui <- function(history) {
  renderUI({
    # Skip the first system message if it exists
    history_to_display <- if (length(history) > 0 && !is.null(history[[1]]$role) && history[[1]]$role == "system") {
      tail(history, -1)
    } else {
      history
    }

    if (is.null(history_to_display) || length(history_to_display) == 0) {

      return(tags$p(tags$em("Start the conversation.")))
    }

    formatted_messages <- lapply(history_to_display, function(msg) {
      # placeholder
      content_display <- msg$content %||% "[NO CONTENT]"
      role_display <- msg$role %||% "unknown"

      # Styling system errors (checking for English "Error" prefix now)

      if (role_display == "system" && grepl("^(Error|Blad)(:| API:| execution error:| processing| wykonania Future:| przetwarzania)", content_display, ignore.case = TRUE)) {
        tags$div(
          style = "margin-bottom: 8px; padding: 8px; border-radius: 8px; background-color: #ffebee; color: #c62828; border: 1px dashed #ef9a9a; width: 90%; margin-right: 10%; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);",
          # label
          tags$strong("System Information:"),
          tags$div(style = "white-space: pre-wrap; word-wrap: break-word;", content_display)
        )
      } else if (role_display == "system") {
        # Hide other system messages
        NULL
      } else {
        # User and assistant messages
        tags$div(
          style = paste(
            "margin-bottom: 8px; padding: 8px; border-radius: 8px;",
            if (role_display == "user") "background-color: #e1f5fe; text-align: left; margin-left: 10%; width: 90%; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);"
            else if (role_display == "assistant") "background-color: #f0f4c3; margin-right: 10%; width: 90%; box-shadow: 1px 1px 3px rgba(0,0,0,0.1);"
            else "background-color: #eeeeee; font-style: italic; color: #555; border: 1px dashed #ccc;" # Style for 'unknown'
          ),
          # labels
          tags$strong(ifelse(role_display == "user", "You:", "Assistant:")),
          # white-space: pre-wrap; will handle new lines added in the "Attached:" section
          tags$div(style = "white-space: pre-wrap; word-wrap: break-word;", HTML(content_display))
        )
      }
    })
    formatted_messages <- Filter(Negate(is.null), formatted_messages)
    tagList(formatted_messages, tags$div(style="height: 10px;")) # Extra margin at the bottom
  })
}


#' Renders the UI for the list of files in the STAGING area (before sending)
#'
#' Function name and logic changed to accept a vector of filenames.
#' Displays "No files" when the list is empty.
#'
#' @param staged_files Character vector containing the names of staged files.
#' @return UI rendering object (result of `renderUI`).
#' @noRd
#' @import shiny
render_staged_attachments_list_ui <- function(staged_files) {
  renderUI({
    # Check if staged_files is NULL, empty, or contains only empty strings
    if (is.null(staged_files) || length(staged_files) == 0 || all(staged_files == "")) {
      # text
      tags$p(tags$em("No files"), style = "padding: 5px; margin: 0; color: #888; font-size: 0.9em;")
    } else {
      # Remove empty strings if any exist
      valid_files <- staged_files[nzchar(staged_files)]
      if (length(valid_files) == 0) {
        # text
        tags$p(tags$em("No files"), style = "padding: 5px; margin: 0; color: #888; font-size: 0.9em;")
      } else {
        tagList(
          tags$ul(style = "margin: 0; padding-left: 15px; list-style-type: none;",
                  lapply(valid_files, function(file_name) {
                    tags$li(style="font-size: 0.9em; margin-bottom: 2px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
                            tags$span(class = "glyphicon glyphicon-paperclip", style="margin-right: 4px;"),
                            tags$span(title = file_name, file_name) # Use the filename directly
                    )
                  })
          )
        )
      }
    }
  })
}
