editor_action_state <- function(context) {
  has_target <- !is.null(context) &&
    isTRUE(rstudio_available()) &&
    !is.null(context$document_id)

  has_selection <- has_target &&
    !is.null(context$selection_range) &&
    nzchar(trimws(context$selection_text %||% ""))

  list(
    can_insert = has_target,
    can_replace = has_selection,
    target_label = if (!is.null(context$path)) basename(context$path) else "active editor"
  )
}

insert_text_into_rstudio <- function(text, context) {
  state <- editor_action_state(context)
  if (!isTRUE(state$can_insert)) {
    return(list(ok = FALSE, message = "No verified editor target. Refresh context and try again."))
  }
  if (!is.character(text) || length(text) != 1 || !nzchar(text)) {
    return(list(ok = FALSE, message = "No text to insert."))
  }

  location <- context$cursor_position
  tryCatch({
    if (is.null(location)) {
      rstudioapi::insertText(text = text, id = context$document_id)
    } else {
      rstudioapi::insertText(location = location, text = text, id = context$document_id)
    }
    list(ok = TRUE, message = paste("Inserted into", state$target_label))
  }, error = function(e) {
    list(ok = FALSE, message = paste("Insert failed:", e$message))
  })
}

replace_selection_in_rstudio <- function(text, context) {
  state <- editor_action_state(context)
  if (!isTRUE(state$can_replace)) {
    return(list(ok = FALSE, message = "No verified selection. Refresh context and try again."))
  }
  if (!is.character(text) || length(text) != 1 || !nzchar(text)) {
    return(list(ok = FALSE, message = "No replacement text."))
  }

  validation <- validate_replacement_target(context)
  if (!isTRUE(validation$ok)) {
    return(validation)
  }

  tryCatch({
    rstudioapi::modifyRange(location = context$selection_range, text = text, id = context$document_id)
    list(ok = TRUE, message = paste("Replaced selection in", state$target_label))
  }, error = function(e) {
    list(ok = FALSE, message = paste("Replace failed:", e$message))
  })
}

validate_replacement_target <- function(context) {
  current <- tryCatch(
    rstudioapi::getSourceEditorContext(id = context$document_id),
    error = function(e) NULL
  )
  if (is.null(current)) {
    return(list(ok = FALSE, message = "Editor target is unavailable. Refresh context and try again."))
  }

  current_selection <- first_editor_selection(current)
  current_text <- current_selection$text %||% ""
  captured_text <- context$selection_text %||% ""
  if (!identical(trimws(current_text), trimws(captured_text))) {
    return(list(ok = FALSE, message = "Selection changed. Refresh context before replacing."))
  }

  list(ok = TRUE, message = "Selection verified.")
}

# Helper %||%
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
