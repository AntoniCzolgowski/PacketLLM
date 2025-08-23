#' Add user message to the active conversation
#' @param text Single character string.
#' @return Invisible NULL.
#' @export
add_user_message <- function(text) {
  if (!is.character(text) || length(text) != 1) stop("Message text must be a single character string.")
  add_message_to_active_history(role = "user", content = text)
  invisible(NULL)
}

#' Prepare messages for the OpenAI API call
#'
#' Internal helper that builds message list based on model, history, and attachments.
#'
#' @param conversation_history List of messages.
#' @param attachments List of attachments.
#' @param conversation_system_message System message.
#' @param conversation_model Model name.
#' @param verbose Logical for diagnostics.
#' @return List with `messages`, or list with `error`.
#' @noRd
#' @importFrom utils getFromNamespace
prepare_api_messages <- function(conversation_history,
                                 attachments,
                                 conversation_system_message,
                                 conversation_model,
                                 verbose = getOption("PacketLLM.verbose", default = FALSE)) {

  # No simplified models for GPT-5 family
  simplified_models <- tryCatch(
    getFromNamespace("simplified_models_list", "PacketLLM"),
    error = function(e) character(0)
  )

  use_simplified_logic <- conversation_model %in% simplified_models

  api_messages <- list()

  if (use_simplified_logic) {
    if (verbose) message("Preparing API messages: simplified logic for ", conversation_model)
    api_messages <- Filter(function(msg) !is.null(msg$role) && msg$role %in% c("user", "assistant"), conversation_history)
    last_idx <- length(api_messages)
    if (last_idx == 0 || api_messages[[last_idx]]$role != "user") {
      api_messages[[length(api_messages) + 1]] <- list(role = "user", content = "(Awaiting response)")
      if (verbose) message("Added placeholder user message for simplified model.")
    }
  } else {
    if (verbose) message("Preparing API messages: standard logic for ", conversation_model)
    api_messages <- conversation_history

    # Ensure system message at the front
    if (length(api_messages) == 0 || api_messages[[1]]$role != "system") {
      if (verbose) message("Prepending system message.")
      api_messages <- c(list(list(role = "system", content = conversation_system_message)), api_messages)
    } else {
      api_messages[[1]]$content <- conversation_system_message
    }

    # Append attachments into system message
    if (length(attachments) > 0) {
      if (verbose) message("Including context from ", length(attachments), " attachments.")
      attachments_text <- ""
      for (att in attachments) {
        if (is.list(att) && !is.null(att$name) && !is.null(att$content)) {
          attachments_text <- paste0(
            attachments_text,
            "\n\n--- START OF ATTACHMENT: ", att$name, " ---\n",
            att$content,
            "\n--- END OF ATTACHMENT: ", att$name, " ---"
          )
        } else {
          warning("Skipping invalid attachment format during message preparation.", call. = FALSE)
        }
      }
      if (nzchar(attachments_text)) {
        base_system_content <- api_messages[[1]]$content
        api_messages[[1]]$content <- paste0(
          base_system_content,
          "\n\n--- ATTACHED FILES CONTEXT (AVAILABLE TO YOU IN THIS CONVERSATION) ---",
          attachments_text,
          "\n--- END OF ATTACHED FILES CONTEXT ---"
        )
      }
    }

    # Ensure last message is from user
    last_idx <- length(api_messages)
    if (last_idx == 0 || api_messages[[last_idx]]$role != "user") {
      api_messages[[length(api_messages) + 1]] <- list(
        role = "user",
        content = "(User is awaiting a response based on the available context)"
      )
      if (verbose) message("Added placeholder user message for standard model.")
    }
  }

  list(messages = api_messages)
}

#' Get assistant response for the active conversation
#'
#' Prepares messages and calls the API. Adds the reply to history.
#' @return Character with assistant reply or error message.
#' @export
get_assistant_response <- function() {
  is_verbose <- function() getOption("PacketLLM.verbose", default = FALSE)

  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    error_content <- "Critical Error: No active conversation to process."
    warning(error_content); return(error_content)
  }

  conversation_history <- active_conv$history %||% list()
  attachments <- active_conv$attachments %||% list()
  conversation_system_message <- active_conv$system_message %||% "You are a helpful assistant."
  conversation_model <- active_conv$model %||% "gpt-5"

  if (is_verbose()) message(paste("Getting assistant response for model:", conversation_model))

  prepared_data <- tryCatch({
    prepare_api_messages(
      conversation_history = conversation_history,
      attachments = attachments,
      conversation_system_message = conversation_system_message,
      conversation_model = conversation_model,
      verbose = is_verbose()
    )
  }, error = function(e) {
    error_msg <- paste("Error preparing messages:", e$message)
    warning(error_msg, call. = FALSE)
    active_id <- get_active_conversation_id()
    if (!is.null(active_id) && active_id %in% names(.history_env$conversations)) {
      tryCatch(add_message_to_active_history(role = "system", content = error_msg), error = function(e2) {})
    }
    return(list(error = error_msg))
  })

  if (!is.null(prepared_data$error)) return(prepared_data$error)

  api_messages <- prepared_data$messages
  if (length(api_messages) == 0) {
    error_content <- "No history to send to the API after processing."
    warning(error_content)
    active_id <- get_active_conversation_id()
    if (!is.null(active_id) && active_id %in% names(.history_env$conversations)) {
      tryCatch(add_message_to_active_history(role = "system", content = paste("Error:", error_content)), error = function(e2) {})
    }
    return(error_content)
  }

  response_text <- tryCatch({
    call_openai_chat(api_messages, model = conversation_model)
  }, error = function(e) {
    error_message <- paste("API Error:", e$message)
    warning(error_message)
    active_id <- get_active_conversation_id()
    if (!is.null(active_id) && active_id %in% names(.history_env$conversations)) {
      tryCatch(add_message_to_active_history(role = "system", content = error_message), error = function(e2) {
        warning(paste("Failed to save API error to history:", e2$message))
      })
    }
    return(error_message)
  })

  if (!grepl("^(Error preparing messages:|API Error:|Critical Error:)", response_text, ignore.case = TRUE)) {
    if (!is.null(response_text)) {
      active_id_for_response <- get_active_conversation_id()
      if (!is.null(active_id_for_response) && active_id_for_response %in% names(.history_env$conversations)) {
        add_message_to_active_history(role = "assistant", content = response_text)
      } else {
        warning("Received API response, but the active conversation no longer exists or is not set.")
      }
    } else {
      warning("API call returned NULL or unexpected structure; response not added.", call. = FALSE)
    }
  }

  response_text
}

# Helper %||%
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

