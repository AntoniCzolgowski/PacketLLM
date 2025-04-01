# chat_logic.R

#' Add user message to the active conversation
#'
#' Calls add_message_to_active_history, which locks the model after the first message.
#'
#' @param text The user's message text.
#' @return Invisible NULL
#' @export
add_user_message <- function(text) {
  if (!is.character(text) || length(text) != 1) {
    stop("Message text must be a single character string.")
  }
  # Call the function from history_manager, which handles the active conversation and model locking
  add_message_to_active_history(role = "user", content = text)
  invisible(NULL)
}

#' Get assistant response for the active conversation
#'
#' Sends the history to the API. For models `o1` and `o3-mini`, it sends only
#' 'user' and 'assistant' roles, ignores temperature, system message, and attachments.
#' For other models, it uses the full conversation settings.
#'
#' @return Assistant's response text or an error message.
#' @export
get_assistant_response <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    error_content <- "Critical Error: No active conversation to process."
    warning(error_content)
    return(error_content)
  }

  conversation_history <- active_conv$history %||% list()
  attachments <- active_conv$attachments %||% list()
  conversation_temp <- active_conv$temperature %||% 0.5
  # Default system message translated
  conversation_system_message <- active_conv$system_message %||% "You are a helpful assistant."
  conversation_model <- active_conv$model %||% "gpt-4o"

  message(paste("Preparing API request for model:", conversation_model))

  # CHANGE: Conditional logic for o1/o3-mini models
  simplified_models <- c("o1", "o3-mini") # Already defined in openai_api.R, but kept here for clarity if run standalone
  use_simplified_logic <- conversation_model %in% simplified_models

  api_messages <- list()
  temp_to_use <- conversation_temp

  if (use_simplified_logic) {
    message("Using simplified logic for model ", conversation_model, ": only user/assistant roles, ignoring temp, sys_msg, attachments.")

    # Filter history, keeping only user and assistant roles
    api_messages <- Filter(function(msg) msg$role %in% c("user", "assistant"), conversation_history)

    # Set default temperature (API might require some value)
    temp_to_use <- 0.5 # Or another default, e.g., 0

    # Check if anything remains after filtering and if the last message is from the user
    last_msg_index_simple <- length(api_messages)
    if (last_msg_index_simple == 0 || api_messages[[last_msg_index_simple]]$role != "user") {
      # If history is empty or the last message is not from the user,
      # and we expect a response, this is problematic for these models.
      # We can either add a placeholder or report an error. Let's add a placeholder.
      placeholder_text <- "(Awaiting response)" # Simpler placeholder
      api_messages[[length(api_messages) + 1]] <- list(role = "user", content = placeholder_text)
      message("Added placeholder user message for simplified model.")
    }

  } else {
    # Logic for standard models (gpt-4o, gpt-4o-mini, etc.) - as before
    message("Using standard logic for model ", conversation_model)
    api_messages <- conversation_history # Start with the full history

    # Handle system message
    if (length(api_messages) == 0 || api_messages[[1]]$role != "system") {
      # System message added here is the default one, the specific one is set below
      message("Warning: Chat history does not start with a system message. Adding the conversation-specific one now.")
      api_messages <- c(list(list(role = "system", content = conversation_system_message)), api_messages)
    } else {
      api_messages[[1]]$content <- conversation_system_message # Always use the current one from settings
      message("Using conversation-specific system message.")
    }

    # Handle attachments (only for standard models)
    if (length(attachments) > 0) {
      message("Including context from ", length(attachments), " attachments.")
      attachments_text <- ""
      for (att in attachments) {
        attachments_text <- paste0(
          attachments_text,
          "\n\n--- START OF ATTACHMENT: ", att$name, " ---\n",
          att$content,
          "\n--- END OF ATTACHMENT: ", att$name, " ---"
        )
      }
      base_system_content <- api_messages[[1]]$content
      api_messages[[1]]$content <- paste0(
        base_system_content,
        "\n\n--- ATTACHED FILES CONTEXT (AVAILABLE TO YOU IN THIS CONVERSATION) ---",
        attachments_text,
        "\n--- END OF ATTACHED FILES CONTEXT ---"
      )
      message("Appended attachment content to the system message for the API.")
    } else {
      message("No attachments to include.")
    }

    # Check the last message (as before)
    last_msg_index_standard <- length(api_messages)
    if (last_msg_index_standard == 0 || api_messages[[last_msg_index_standard]]$role != "user") {
      placeholder_text <- "(User is awaiting a response based on the available context)"
      api_messages[[length(api_messages) + 1]] <- list(role = "user", content = placeholder_text)
      message("Added placeholder user message for the API call.")
    }
  } # End of conditional logic for models

  # Check if we have any messages to send
  if(length(api_messages) == 0) {
    error_content <- "No history to send to the API after processing."
    warning(error_content)
    # Add error to visible history
    active_id_for_error <- get_active_conversation_id()
    if (!is.null(active_id_for_error) && active_id_for_error %in% names(.history_env$conversations)) {
      tryCatch(add_message_to_active_history(role = "system", content = paste("Error:", error_content)), error=function(e2){})
    }
    return(error_content)
  }


  # Call OpenAI API with prepared messages and temperature
  response_text <- tryCatch({
    call_openai_chat(api_messages, model = conversation_model, temperature = temp_to_use)
  }, error = function(e) {
    error_message <- paste("API Error:", e$message)
    warning(error_message)
    # Save error to history (as before)
    active_id_for_error <- get_active_conversation_id()
    if (!is.null(active_id_for_error) && active_id_for_error %in% names(.history_env$conversations)) {
      tryCatch(add_message_to_active_history(role = "system", content = error_message), error=function(e2){
        warning(paste("Failed to save API error to history:", e2$message))
      })
    }
    return(error_message)
  })

  # Add assistant's response to history (as before)
  # Updated error check to be case-insensitive and match English "Error"
  if (!grepl("^Error(:| API:)", response_text, ignore.case = TRUE)) {
    active_id_for_response <- get_active_conversation_id()
    if (!is.null(active_id_for_response) && active_id_for_response %in% names(.history_env$conversations)) {
      add_message_to_active_history(role = "assistant", content = response_text)
    } else {
      warning("Received API response, but the active conversation no longer exists or is not set.")
    }
  }

  return(response_text)
}

# Helper %||%
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
