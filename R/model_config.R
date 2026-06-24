#' Available model presets
#'
#' Returns the model presets shown in the PacketLLM UI.
#'
#' @return A data frame with preset labels, model IDs, and descriptions.
#' @export
available_model_presets <- function() {
  data.frame(
    preset = c("Best", "Balanced", "Fast"),
    model = c("gpt-5.5", "gpt-5.4", "gpt-5.4-mini"),
    description = c(
      "Highest quality for complex R and package work.",
      "Strong default for day-to-day development.",
      "Lower latency for focused questions and small edits."
    ),
    stringsAsFactors = FALSE
  )
}

#' List of available AI models for selection in the UI
#' @export
available_openai_models <- available_model_presets()$model

#' Models treated as simplified
#' @noRd
simplified_models_list <- character(0)

model_preset_choices <- function() {
  presets <- available_model_presets()
  stats::setNames(presets$model, presets$preset)
}

model_preset_label <- function(model) {
  presets <- available_model_presets()
  match_idx <- match(model, presets$model)
  if (!is.na(match_idx)) {
    return(presets$preset[match_idx])
  }
  model %||% "Custom"
}

default_model_settings <- function() {
  list(
    model = available_openai_models[1],
    reasoning_effort = "medium",
    verbosity = "low",
    max_output_tokens = NA_integer_,
    assistant_behavior = "default",
    custom_instruction = "",
    context_mode = "auto"
  )
}

valid_reasoning_efforts <- function() {
  c("low", "medium", "high", "xhigh")
}

valid_verbosity_levels <- function() {
  c("low", "medium", "high")
}

assistant_behavior_choices <- function() {
  c(
    "Default" = "default",
    "Concise" = "concise",
    "Code-focused" = "code",
    "Reviewer" = "reviewer",
    "Custom" = "custom"
  )
}

assistant_behavior_instruction <- function(behavior, custom_instruction = "") {
  behavior <- behavior %||% "default"
  if (identical(behavior, "custom") && nzchar(trimws(custom_instruction %||% ""))) {
    return(custom_instruction)
  }

  switch(
    behavior,
    concise = "Keep responses concise. Prefer short explanations and compact code.",
    code = paste(
      "Prioritize runnable R code and package-safe implementation details.",
      "Keep prose brief and preserve code formatting."
    ),
    reviewer = paste(
      "Use a code review stance. Lead with concrete bugs, risks, CRAN compatibility",
      "concerns, and missing tests. Avoid broad rewrites unless necessary."
    ),
    "Be practical, precise, and focused on helping with R and RStudio work."
  )
}

base_packetllm_instruction <- function() {
  paste(
    "You are PacketLLM, an AI assistant for RStudio workflows.",
    "Use the supplied RStudio context when it is relevant.",
    "Do not claim that code was inserted, replaced, saved, or executed unless the user explicitly performed that action.",
    "When suggesting a replacement for selected code, prefer a concise explanation followed by a fenced R code block containing the replacement.",
    "When writing math, use LaTeX delimiters \\(...\\) for inline math and \\[...\\] for display math.",
    "For multi-step changes, describe exact file targets and keep the proposed diff small.",
    "Preserve R code formatting and use testthat/roxygen2 conventions when appropriate.",
    sep = "\n"
  )
}

# Helper %||%
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
