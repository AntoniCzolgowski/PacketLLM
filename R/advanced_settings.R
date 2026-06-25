#' Advanced settings modal UI
#'
#' Renders model, behavior, and generation controls.
#'
#' @param model_value Currently selected model ID.
#' @param behavior_value Assistant behavior preset.
#' @param custom_instruction Current custom instruction text.
#' @param reasoning_value Current reasoning effort.
#' @param verbosity_value Current verbosity setting.
#' @param max_output_value Current max output value.
#' @return A tagList with modal UI elements.
#' @noRd
#' @import shiny
create_advanced_settings_modal_ui <- function(model_value,
                                              behavior_value,
                                              custom_instruction,
                                              reasoning_value,
                                              verbosity_value,
                                              max_output_value) {
  tagList(
    tags$div(
      class = "packet-settings-group",
      selectInput(
        "modal_model",
        label = "Model preset",
        choices = model_preset_choices(),
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
      class = "packet-settings-group",
      selectInput(
        "modal_assistant_behavior",
        label = "Assistant behavior",
        choices = assistant_behavior_choices(),
        selected = behavior_value,
        width = "100%"
      ),
      textAreaInput(
        "modal_custom_instruction",
        label = "Custom instruction",
        value = custom_instruction,
        rows = 4,
        resize = "vertical",
        width = "100%",
        placeholder = "Only used when Assistant behavior is Custom."
      )
    ),
    tags$div(
      class = "packet-settings-grid",
      selectInput(
        "modal_reasoning_effort",
        label = "Reasoning",
        choices = valid_reasoning_efforts(),
        selected = reasoning_value,
        width = "100%"
      ),
      selectInput(
        "modal_verbosity",
        label = "Verbosity",
        choices = valid_verbosity_levels(),
        selected = verbosity_value,
        width = "100%"
      )
    ),
    numericInput(
      "modal_max_output_tokens",
      label = "Max output tokens",
      value = if (is.na(max_output_value)) NA else max_output_value,
      min = 1,
      step = 256,
      width = "100%"
    )
  )
}
