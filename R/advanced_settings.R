# advanced_settings.R

#' Creates the UI for the advanced settings modal content
#'
#' Generates a model selector, temperature slider, and text area
#' for the system message. Adds containers for easier state management
#' (enabled/disabled) and space for messages.
#'
#' @param available_models Vector of available model names.
#' @param model_value Currently selected model (character).
#' @param temp_value Current temperature value (numeric).
#' @param sys_msg_value Current system message value (character).
#' @param model_input_id ID for the model selector.
#' @param temp_input_id ID for the temperature slider.
#' @param msg_input_id ID for the system message text area.
#'
#' @return A tagList object with the modal UI elements.
#' @noRd
#' @import shiny
create_advanced_settings_modal_ui <- function(available_models,
                                              model_value,
                                              temp_value,
                                              sys_msg_value,
                                              model_input_id = "modal_model",
                                              temp_input_id = "modal_temp",
                                              msg_input_id = "modal_system_message") {
  tagList(
    # Container for model selector and lock message
    tags$div(id = "model_selector_container",
             selectInput(model_input_id,
                         # label
                         label = "LLM Model (for this conversation):",
                         choices = available_models,
                         selected = model_value,
                         width = "100%"),
             tags$small(id = "model_locked_message", class = "text-muted", style = "display: none;",
                        # message
                        "Changing the model is locked after the conversation starts.") # Hidden by default
    ),
    # Container for temperature and message
    tags$div(id = "temperature_container",
             sliderInput(temp_input_id,
                         # label
                         label = "Temperature (response creativity):",
                         min = 0, max = 1, value = temp_value, step = 0.01),
             tags$small(id = "temp_disabled_message", class = "text-muted", style = "display: none;",
                        # message
                        "Temperature is not available for the selected models (o1, o3-mini).") # Hidden by default
    ),

    # Container for system message and message
    tags$div(id = "system_message_container",
             textAreaInput(msg_input_id,
                           # label
                           label = "System Message (instructions for the model in this conversation):",
                           value = sys_msg_value,
                           rows = 5,
                           resize = "vertical",
                           width = "100%",
                           # placeholder
                           placeholder = "E.g., Respond concisely. Be formal. Act like a pirate."),
             tags$small(id = "sysmsg_disabled_message", class = "text-muted", style = "display: none;",
                        # message
                        "System message is not available for the selected models (o1, o3-mini).") # Hidden by default
    )
  )
}
