#' Advanced settings modal UI (model + system message)
#'
#' Renders model selector and system message textarea.
#'
#' @param available_models Character vector of available model names.
#' @param model_value Currently selected model.
#' @param sys_msg_value Current system message.
#' @param model_input_id Input id for model selector.
#' @param msg_input_id Input id for system message textarea.
#' @return A tagList with modal UI elements.
#' @noRd
#' @import shiny
create_advanced_settings_modal_ui <- function(available_models,
                                              model_value,
                                              sys_msg_value,
                                              model_input_id = "modal_model",
                                              msg_input_id = "modal_system_message") {
  tagList(
    tags$div(
      id = "model_selector_container",
      selectInput(
        model_input_id,
        label = "LLM Model (for this conversation):",
        choices = available_models,
        selected = model_value,
        width = "100%"
      ),
      tags$small(
        id = "model_locked_message",
        class = "text-muted",
        style = "display: none;",
        "Changing the model is locked after the conversation starts."
      )
    ),
    tags$div(
      id = "system_message_container",
      textAreaInput(
        msg_input_id,
        label = "System Message (instructions for the model in this conversation):",
        value = sys_msg_value,
        rows = 5,
        resize = "vertical",
        width = "100%",
        placeholder = "E.g., Respond concisely. Be formal."
      ),
      tags$small(
        id = "sysmsg_disabled_message",
        class = "text-muted",
        style = "display: none;",
        "System message is not available for the selected model."
      )
    )
  )
}
