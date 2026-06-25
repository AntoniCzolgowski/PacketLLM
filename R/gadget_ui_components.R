# gadget_ui_components.R

create_tab_content_ui <- function(conv_id) {
  ns <- NS(conv_id)

  tagList(
    tags$div(
      class = "packet-context-row",
      tags$div(
        class = "packet-context-controls",
        tags$span(class = "packet-context-caption", "Context:"),
        selectInput(
          ns("context_mode"),
          label = NULL,
          choices = context_mode_choices(),
          selected = "auto",
          width = "132px"
        )
      ),
      tags$div(class = "packet-context-status", uiOutput(ns("context_status_output")))
    ),
    tags$div(
      class = "packet-chat-history",
      uiOutput(ns("chat_history_output"))
    ),
    tags$div(
      class = "packet-composer",
      tags$div(
        class = "packet-attachments-row",
        tags$button(
          id = ns("add_file_btn"),
          type = "button",
          class = "packet-secondary-btn add-file-btn-class",
          `data-conv-id` = conv_id,
          "Attach"
        ),
        tags$div(
          id = ns("staged-attachments-list-container"),
          class = "packet-attachments",
          uiOutput(ns("staged_files_list_output"))
        )
      ),
      tags$div(
        class = "packet-input-row",
        textAreaInput(
          ns("user_message_input"),
          label = NULL,
          placeholder = "Ask PacketLLM...",
          width = "100%"
        ),
        tags$button(
          id = ns("send_query_btn"),
          type = "button",
          class = "packet-primary-btn send-btn-class",
          `data-conv-id` = conv_id,
          "Send"
        )
      )
    )
  )
}
