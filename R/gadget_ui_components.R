# gadget_ui_components.R

#' Creates the UI for the content of a single chat tab
#'
#' Generates the HTML structure for the history area and the bottom input bar.
#' Changed the uiOutput ID for the attachment list to `staged_files_list_output`.
#'
#' @param conv_id Unique identifier for the conversation (used for namespacing).
#'
#' @return A tagList object containing the UI for the tab.
#' @noRd
#' @import shiny
create_tab_content_ui <- function(conv_id) {
  ns <- NS(conv_id) # Create namespace

  tagList(
    # Chat history container (no changes)
    tags$div(
      class = "chat-history-container-class",
      uiOutput(ns("chat_history_output")) # Output rendered by render_chat_history_ui
    ),

    # Input row (Flexbox)
    tags$div(
      class = "input-action-row",

      # 1. Add File button (+) (no text change needed)
      tags$button(
        id = ns("add_file_btn"),
        type = "button",
        class = "btn btn-default add-file-btn-class",
        `data-conv-id` = conv_id,
        "+"
      ),

      # 2. Container for attachment list (CHANGED uiOutput ID)
      tags$div(
        id = ns("staged-attachments-list-container"), # Container ID can remain
        class = "attachments-container",
        # Changed UI output ID to match the server
        uiOutput(ns("staged_files_list_output")) # Output rendered by render_staged_attachments_list_ui
      ),

      # 3. Message input field (placeholder translated)
      textAreaInput(
        ns("user_message_input"),
        label = NULL,
        placeholder = "Enter message...", # Translated placeholder
        width = "100%"
      ),

      # 4. Send button (text translated)
      tags$button(
        id = ns("send_query_btn"),
        type = "button",
        class = "btn btn-primary send-btn-class",
        `data-conv-id` = conv_id,
        "Send" # Translated button text
      )
    ) # End tags$div.input-action-row
  ) # End tagList
}
