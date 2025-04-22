# chat_logic.R

#' Add user message to the active conversation
#' Calls add_message_to_active_history, which handles model locking and title setting.
#'
#' @param text The user's message text (a single character string).
#' @return Invisible `NULL` (`invisible(NULL)`). This function is called for its
#'         side effect of adding a message to the conversation history.
#'         Stops with an error if `text` is not a single character string.
#' @export
#' @examples
#' # This example modifies the internal state managed by history_manager.
#' # Ensure history_manager is initialized if running standalone.
#'
#' # Setup: Create and activate a conversation
#' conv_id_user <- tryCatch(create_new_conversation(activate = TRUE), error = function(e) NULL)
#'
#' if (!is.null(conv_id_user)) {
#'   # Add a message from the user
#'   add_user_message("Hello, this is my first message.")
#'
#'   # Verify the message was added (optional)
#'   history <- get_active_chat_history()
#'   print(tail(history, 1)) # Show the last message
#'
#'   # Clean up the conversation
#'   delete_conversation(conv_id_user)
#' } else {
#'   print("Skipping example as conversation setup failed.")
#' }
#'
#' # Reset active conversation if needed
#' set_active_conversation(NULL)
add_user_message <- function(text) {
  if (!is.character(text) || length(text) != 1) {
    stop("Message text must be a single character string.")
  }
  # Call the function from history_manager, which handles the active conversation and model locking
  # add_message_to_active_history returns a list, but add_user_message returns NULL
  add_message_to_active_history(role = "user", content = text) # Pass default verbose setting
  invisible(NULL)
}


# NEW INTERNAL FUNCTION (added before get_assistant_response)
#' Prepare messages and temperature for the OpenAI API call
#'
#' Internal helper function to encapsulate the logic of preparing the payload
#' based on model type, history, attachments, and settings.
#'
#' @param conversation_history List of message history.
#' @param attachments List of attachments.
#' @param conversation_temp Temperature setting.
#' @param conversation_system_message System message setting.
#' @param conversation_model Model name.
#' @param verbose Logical. Should diagnostic messages be printed?
#' @return A list containing `messages` (list) and `temperature` (numeric)
#'         ready for the API call, or a list containing `error` if preparation fails.
#' @noRd
#' @importFrom utils getFromNamespace
prepare_api_messages <- function(conversation_history, attachments, conversation_temp,
                                 conversation_system_message, conversation_model,
                                 verbose = getOption("PacketLLM.verbose", default = FALSE)) {

  # Get the list of simplified models. Access internal variable safely.
  simplified_models <- tryCatch(
    getFromNamespace("simplified_models_list", "PacketLLM"),
    error = function(e) {
      warning("Could not access internal simplified_models_list. Using fallback.", call. = FALSE)
      c("o1", "o3-mini") # Fallback definition
    }
  )

  use_simplified_logic <- conversation_model %in% simplified_models

  api_messages <- list()
  temp_to_use <- conversation_temp

  if (use_simplified_logic) {
    if (verbose) message("Preparing API messages: Using simplified logic for model ", conversation_model)
    # Filter history, keeping only user and assistant roles
    api_messages <- Filter(function(msg) !is.null(msg$role) && msg$role %in% c("user", "assistant"), conversation_history)
    temp_to_use <- 0.5 # Simplified models ignore temperature, use default for internal consistency

    last_msg_index_simple <- length(api_messages)
    # Add placeholder if history is empty OR last message is not from user
    if (last_msg_index_simple == 0 || api_messages[[last_msg_index_simple]]$role != "user") {
      placeholder_text <- "(Awaiting response)"
      api_messages[[length(api_messages) + 1]] <- list(role = "user", content = placeholder_text)
      if (verbose) message("Preparing API messages: Added placeholder user message for simplified model.")
    }

  } else {
    # Standard model logic
    if (verbose) message("Preparing API messages: Using standard logic for model ", conversation_model)
    api_messages <- conversation_history # Start with full history

    # Ensure system message is first and correct
    if (length(api_messages) == 0 || api_messages[[1]]$role != "system") {
      if (verbose) message("Preparing API messages: Prepending system message.")
      # Prepend the specific system message for this conversation
      api_messages <- c(list(list(role = "system", content = conversation_system_message)), api_messages)
    } else {
      if (verbose) message("Preparing API messages: Ensuring current system message is used.")
      # Ensure the system message content is the one currently set for the conversation
      api_messages[[1]]$content <- conversation_system_message
    }

    # Handle attachments by appending to the system message
    if (length(attachments) > 0) {
      if (verbose) message("Preparing API messages: Including context from ", length(attachments), " attachments.")
      attachments_text <- ""
      for (att in attachments) {
        # Ensure attachment format is valid before processing
        if(is.list(att) && !is.null(att$name) && !is.null(att$content)){
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
      # Check if there is actually text to append
      if (nzchar(attachments_text)){
        base_system_content <- api_messages[[1]]$content # Get current system message content
        api_messages[[1]]$content <- paste0(
          base_system_content,
          "\n\n--- ATTACHED FILES CONTEXT (AVAILABLE TO YOU IN THIS CONVERSATION) ---",
          attachments_text,
          "\n--- END OF ATTACHED FILES CONTEXT ---"
        )
        if (verbose) message("Preparing API messages: Appended attachment content to the system message.")
      } else {
        if (verbose) message("Preparing API messages: No valid attachment content to append.")
      }

    } else {
      if (verbose) message("Preparing API messages: No attachments to include.")
    }


    # Check last message role after potentially adding system message
    last_msg_index_standard <- length(api_messages)
    # Add placeholder if history is empty OR last message is not from user
    # The check `last_msg_index_standard == 0` shouldn't happen now because we ensure system message
    if (last_msg_index_standard == 0 || api_messages[[last_msg_index_standard]]$role != "user") {
      placeholder_text <- "(User is awaiting a response based on the available context)"
      api_messages[[length(api_messages) + 1]] <- list(role = "user", content = placeholder_text)
      if (verbose) message("Preparing API messages: Added placeholder user message for standard model API call.")
    }
  } # End of standard model logic

  # Return prepared data
  list(messages = api_messages, temperature = temp_to_use)
}


#' Get assistant response for the active conversation
#'
#' Sends the prepared history (including system message and attachments for standard
#' models) to the OpenAI API and returns the assistant's response. Handles model-specific
#' logic internally.
#'
#' @return Character string. Contains the assistant's response text. If an error
#'         occurs during message preparation or the API call, a descriptive error
#'         message (starting with "Error:" or "API Error:") is returned instead.
#'         Returns "Critical Error: No active conversation..." if no conversation is active.
#' @export
#' @examples
#' \dontrun{
#' # This function requires an active conversation with history,
#' # the OPENAI_API_KEY environment variable to be set, and internet access.
#'
#' # Setup: Create, activate, and add a user message
#' conv_id_resp <- tryCatch(create_new_conversation(activate = TRUE), error = function(e) NULL)
#' if (!is.null(conv_id_resp)) {
#'   add_user_message("What day is it today?") # Add a user message first
#'
#'   # Ensure the API key is set in your environment before running:
#'   # Sys.setenv(OPENAI_API_KEY = "your_actual_openai_api_key")
#'
#'   # Get the response from the assistant
#'   # For less console output during standard use, you can set the global option:
#'   # options(PacketLLM.verbose = FALSE)
#'   assistant_reply <- get_assistant_response()
#'   # options(PacketLLM.verbose = TRUE) # Optionally set back for debugging
#'
#'   # Print the response
#'   print(assistant_reply)
#'
#'   # Verify the assistant's response was added to history (optional)
#'   # print(get_active_chat_history())
#'
#'   # Clean up
#'   delete_conversation(conv_id_resp)
#'   set_active_conversation(NULL)
#' } else {
#'  print("Skipping example as conversation setup failed.")
#' }
#' }
get_assistant_response <- function() {
  # Option checker helper function
  is_verbose <- function() getOption("PacketLLM.verbose", default = FALSE)

  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    error_content <- "Critical Error: No active conversation to process."
    warning(error_content)
    return(error_content)
  }

  # Extract necessary data from the active conversation
  conversation_history <- active_conv$history %||% list()
  attachments <- active_conv$attachments %||% list()
  conversation_temp <- active_conv$temperature %||% 0.5
  conversation_system_message <- active_conv$system_message %||% "You are a helpful assistant."
  conversation_model <- active_conv$model %||% "gpt-4o"

  if (is_verbose()) message(paste("Getting assistant response for model:", conversation_model))

  # Call the internal helper function to prepare messages, passing verbose status
  prepared_data <- tryCatch({
    prepare_api_messages(
      conversation_history = conversation_history,
      attachments = attachments,
      conversation_temp = conversation_temp,
      conversation_system_message = conversation_system_message,
      conversation_model = conversation_model,
      verbose = is_verbose() # Pass the verbose flag
    )
  }, error = function(e) {
    # Handle errors during message preparation
    error_msg <- paste("Error preparing messages:", e$message)
    warning(error_msg, call.=FALSE) # Avoid showing call stack for this specific error
    # Optionally add this specific error to history as well
    active_id_for_prep_error <- get_active_conversation_id()
    if (!is.null(active_id_for_prep_error) && active_id_for_prep_error %in% names(.history_env$conversations)) {
      tryCatch(add_message_to_active_history(role = "system", content = error_msg), error=function(e2){})
    }
    return(list(error = error_msg)) # Return list indicating error
  })

  # If message preparation itself returned an error structure, return the error message
  if (!is.null(prepared_data$error)) {
    return(prepared_data$error)
  }

  # Extract prepared messages and temperature
  api_messages <- prepared_data$messages
  temp_to_use <- prepared_data$temperature

  # Check if we have any messages to send after preparation
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


  # Use prepared data in API call
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

  # Add assistant's valid response to history
  # Check for prep error OR api error before adding
  if (!grepl("^(Error preparing messages:|API Error:|Critical Error:)", response_text, ignore.case = TRUE)) {
    # Check response_text is not NULL (call_openai_chat might return NULL on parse error)
    if (!is.null(response_text)) {
      active_id_for_response <- get_active_conversation_id()
      if (!is.null(active_id_for_response) && active_id_for_response %in% names(.history_env$conversations)) {
        # add_message_to_active_history returns a list, result not needed here
        add_message_to_active_history(role = "assistant", content = response_text)
      } else {
        warning("Received API response, but the active conversation no longer exists or is not set.")
      }
    } else {
      warning("API call returned NULL or unexpected structure, assistant response not added to history.", call. = FALSE)
      # Optionally return an error message here? Or rely on the warning from call_openai_chat?
      # For now, return the NULL which indicates failure.
    }
  }

  return(response_text)
}

# Helper %||% (no change)
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
