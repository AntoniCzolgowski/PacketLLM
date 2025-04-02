# history_manager.R

# Environment to store history state
.history_env <- new.env(parent = emptyenv())
.history_env$conversations <- list()
.history_env$active_conversation_id <- NULL
.history_env$conversation_counter <- 0

#' Generates a unique conversation ID
#' @return String with the conversation ID.
#' @noRd
generate_conversation_id <- function() {
  .history_env$conversation_counter <- .history_env$conversation_counter + 1
  paste0("conv_", .history_env$conversation_counter, "_", as.integer(Sys.time()))
}

#' Initializes the history manager
#'
#' Clears the state and creates the first, empty conversation.
#' @return ID of the first created conversation.
#' @export
initialize_history_manager <- function() {
  message("Initializing history manager...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  # Default title translated
  first_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE, title = "New Conversation")
  message(paste("History manager initialized. Created first conversation:", first_id))
  return(first_id)
}

#' Creates a new conversation
#'
#' Adds a new conversation to the `.history_env` environment.
#'
#' @param activate Should the new conversation be set as active (default FALSE).
#' @param add_initial_settings Should a default system message be added to the history (default TRUE).
#' @param title Initial title of the conversation (default generated based on time).
#' @return ID of the newly created conversation.
#' @export
create_new_conversation <- function(activate = FALSE, add_initial_settings = TRUE, title = NULL) {
  conv_id <- generate_conversation_id()
  if (is.null(title)) {
    # Default title format translated
    title <- paste("Conversation", format(Sys.time(), "%H:%M:%S"))
  }
  message(paste("Creating new conversation with ID:", conv_id, "and title:", title))

  default_model <- if (exists("available_openai_models") && length(available_openai_models) > 0) {
    available_openai_models[1] # Use the variable from openai_api.R
  } else {
    "gpt-4o" # Fallback
  }
  # Default system message
  default_system_message <- "You are a helpful assistant. Respond clearly and precisely, maintaining code formatting when required."
  default_temperature <- 0.5


  new_conv <- list(
    id = conv_id,
    title = title,
    history = list(),
    attachments = list(),
    created_at = Sys.time(),
    temperature = default_temperature,
    system_message = "", # Set below
    model = default_model,
    model_locked = FALSE # Flag for locking model changes
  )

  if (add_initial_settings) {
    # Add system message only to the `system_message` field, not necessarily to `history` at the start
    # new_conv$history <- list(list(role = "system", content = default_system_message))
    new_conv$system_message <- default_system_message
    message(paste("Added default settings (SysMsg, Temp) and model (", default_model, ") to conversation", conv_id))
  } else {
    new_conv$system_message <- "" # Empty if not adding settings
    message(paste("Set default model (", default_model, ") for conversation", conv_id, " (without initial settings)"))
  }

  .history_env$conversations[[conv_id]] <- new_conv

  if (activate) {
    set_active_conversation(conv_id)
  }
  return(conv_id)
}

#' Checks if the conversation has started (model locked)
#'
#' Checks if the `model_locked` flag is TRUE. The model is locked
#' after the first **assistant** message is added.
#'
#' @param id Conversation ID.
#' @return TRUE if the model is locked, FALSE otherwise or if the conversation doesn't exist.
#' @export
is_conversation_started <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to check lock status for non-existent conversation: ", id)
    return(FALSE)
  }
  # Access the model_locked flag
  return(isTRUE(.history_env$conversations[[id]]$model_locked))
}


#' Adds a message to the active conversation's history
#'
#' Updates the conversation title based on content if it's the first user message.
#' Sets the `model_locked` flag to TRUE on the first **assistant** message.
#'
#' @param role Role ("user", "assistant", "system").
#' @param content Message content.
#' @return A list indicating the result:
#'         - `list(type = "title_set", new_title = "...")` if a new title was set.
#'         - `list(type = "assistant_locked_model")` if the model was locked.
#'         - `list(type = "message_added")` in other cases of adding a message.
#'         - `list(type = "error", message = "...")` in case of an error.
#' @export
add_message_to_active_history <- function(role, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    warning("Cannot add message, no active conversation.")
    return(list(type = "error", message = "No active conversation."))
  }
  if (!role %in% c("user", "assistant", "system")) {
    warning("Invalid message role: ", role)
    return(list(type = "error", message = paste("Invalid role:", role)))
  }

  # Check if conversation exists
  if (!active_id %in% names(.history_env$conversations)) {
    warning("Active conversation (ID: ", active_id, ") does not exist when adding message.")
    return(list(type = "error", message = "Active conversation does not exist."))
  }

  return_value <- list(type = "message_added") # Default return value
  new_message <- list(role = role, content = content)

  # Check if the model should be locked (first assistant response)
  should_lock_model <- role == "assistant" && !isTRUE(.history_env$conversations[[active_id]]$model_locked)

  # Add message to history
  current_history <- .history_env$conversations[[active_id]]$history %||% list()
  .history_env$conversations[[active_id]]$history <- c(current_history, list(new_message))
  message(paste("Added", role, "message to", active_id))

  # Lock the model if needed
  if (should_lock_model) {
    .history_env$conversations[[active_id]]$model_locked <- TRUE
    message(paste("Model for conversation", active_id, "has been locked."))
    if (return_value$type == "message_added") {
      return_value <- list(type = "assistant_locked_model")
    }
  }

  # --- Title setting logic ---
  conv_history_updated <- .history_env$conversations[[active_id]]$history %||% list()
  user_message_count <- sum(sapply(conv_history_updated, function(m) !is.null(m$role) && m$role == "user"))
  is_first_user_message_ever <- role == "user" && user_message_count == 1

  if (is_first_user_message_ever && nzchar(trimws(content))) {

    # ---TITLE LOGIC START ---
    title_content_base <- content # Start with the full content
    attachment_marker <- "\n\n<strong>Attached:</strong>"
    attachment_marker_start <- "<strong>Attached:</strong>"

    # Scenario 2: Text AND Attachments
    if (grepl(attachment_marker, title_content_base, fixed = TRUE)) {
      # Use only the text BEFORE the attachment marker for the title
      title_content_processed <- trimws(strsplit(title_content_base, attachment_marker, fixed = TRUE)[[1]][1])
      message("Title generation: Detected text and attachments. Using only text part for title.")
      # Scenario 1: Attachments ONLY
    } else if (startsWith(title_content_base, attachment_marker_start)) {
      # Use the content, but remove the <strong> tags for the title
      title_content_processed <- gsub("<strong>|</strong>", "", title_content_base, fixed = FALSE) # Use fixed=FALSE for regex |
      message("Title generation: Detected attachments only. Using message content without bold tags for title.")
      # Default Scenario: Regular text message
    } else {
      title_content_processed <- title_content_base
      message("Title generation: Detected regular text message.")
    }

    # Use the processed content for title generation
    words <- strsplit(trimws(title_content_processed), "\\s+")[[1]]
    words <- words[nzchar(words)] # Remove empty strings resulting from multiple spaces
    # --- TITLE LOGIC END ---

    # Existing truncation logic (applied to processed content)
    max_words <- 5
    max_chars <- 40
    new_title <- paste(head(words, max_words), collapse = " ")
    truncated_by_words <- length(words) > max_words
    truncated_by_chars <- nchar(new_title) > max_chars

    if (truncated_by_chars) {
      new_title <- paste0(substr(new_title, 1, max_chars - 3), "...")
    } else if (truncated_by_words) {
      new_title <- paste0(new_title, "...")
    }
    if (!nzchar(new_title)) {
      new_title <- paste("Conversation", format(Sys.time(), "%M%S"))
    }

    .history_env$conversations[[active_id]]$title <- new_title
    message(paste("Set title for", active_id, "to:", new_title))
    # Ensure this return value is set correctly even after modification
    return_value <- list(type = "title_set", new_title = new_title)
  }
  # --- End of title logic ---

  return(return_value)
}


#' Deletes the conversation with the given ID
#' @param id ID of the conversation to delete.
#' @return TRUE if deleted, FALSE if it did not exist.
#' @export
delete_conversation <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to delete non-existent conversation with ID: ", id)
    return(FALSE)
  }
  message(paste("Deleting conversation with ID:", id))
  .history_env$conversations[[id]] <- NULL

  current_active_id <- .history_env$active_conversation_id
  if (!is.null(current_active_id) && current_active_id == id) {
    message("Deleted the active conversation. Resetting active_conversation_id.")
    .history_env$active_conversation_id <- NULL
  }
  return(TRUE)
}

#' Sets the active conversation
#' @param id ID of the conversation to activate or NULL to deactivate.
#' @return Invisible NULL.
#' @export
set_active_conversation <- function(id) {
  if (is.null(id)) {
    if (!is.null(.history_env$active_conversation_id)) {
      message("Deactivated the active conversation.")
      .history_env$active_conversation_id <- NULL
    }
    return(invisible(NULL))
  }

  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set a non-existent conversation as active: ", id)
    # Do not set active_id to a non-existent one
    # .history_env$active_conversation_id <- NULL
    return(invisible(NULL))
  }

  if (is.null(.history_env$active_conversation_id) || .history_env$active_conversation_id != id) {
    message(paste("Set active conversation to ID:", id))
    .history_env$active_conversation_id <- id
  }
  return(invisible(NULL))
}

#' Gets the ID of the active conversation
#' @return ID of the active conversation (string) or NULL.
#' @export
get_active_conversation_id <- function() {
  active_id <- .history_env$active_conversation_id
  # Check if the ID points to an existing conversation
  if (!is.null(active_id) && !active_id %in% names(.history_env$conversations)) {
    warning(paste("Active ID", active_id, "points to a non-existent conversation. Resetting."))
    .history_env$active_conversation_id <- NULL
    return(NULL)
  }
  return(active_id)
}

#' Gets the full object of the active conversation
#' @return List representing the conversation or NULL.
#' @export
get_active_conversation <- function() {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    return(NULL)
  }
  # Return data for the existing ID
  return(.history_env$conversations[[active_id]])
}

#' Gets the chat history for the active conversation
#' @return List of messages or an empty list.
#' @export
get_active_chat_history <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    return(list())
  }
  return(active_conv$history %||% list())
}

#' Gets the title of the conversation with the given ID
#' @param id Conversation ID.
#' @return Title (string) or NULL if the conversation doesn't exist or has no title.
#' @export
get_conversation_title <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    # warning("Attempting to get title for non-existent conversation: ", id)
    return(NULL)
  }
  # Access the title
  # Default title placeholder translated
  return(.history_env$conversations[[id]]$title %||% paste("[No Title - ID:", id, "]"))
}

#' Gets a list of IDs of all existing conversations
#' @return Character vector with conversation IDs.
#' @export
get_all_conversation_ids <- function() {
  return(names(.history_env$conversations))
}

#' Gets the model name for the conversation with the given ID
#' @param id Conversation ID.
#' @return Model name (string) or NULL.
#' @export
get_conversation_model <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  # Access the model
  return(.history_env$conversations[[id]]$model %||% "gpt-4o") # Fallback
}

#' Sets the model for the conversation, if it hasn't started
#'
#' Checks the `model_locked` flag set after the first assistant response.
#'
#' @param id Conversation ID.
#' @param model_name Name of the new model.
#' @return TRUE if the model was set, FALSE otherwise.
#' @export
set_conversation_model <- function(id, model_name) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set model for non-existent conversation: ", id)
    return(FALSE)
  }
  if (is_conversation_started(id)) {
    warning("Cannot change model - conversation has already started.", call. = FALSE)
    return(FALSE)
  }
  # Model validation (optional, but good practice)
  if (exists("available_openai_models") && !model_name %in% available_openai_models) {
    warning(paste("Attempting to set unavailable model:", model_name, "for conversation", id))
    return(FALSE)
  }

  # Set the model
  .history_env$conversations[[id]]$model <- model_name
  message(paste("Set model for conversation", id, "to:", model_name))
  return(TRUE)
}




#' Sets the temperature for the conversation with the given ID
#' @param id Conversation ID.
#' @param temperature New temperature value (number 0-1).
#' @return TRUE if set, FALSE if conversation doesn't exist or value is invalid.
#' @export
set_conversation_temperature <- function(id, temperature) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set temperature for non-existent conversation: ", id)
    return(FALSE)
  }
  if (!is.numeric(temperature) || temperature < 0 || temperature > 1) {
    warning(paste("Invalid temperature value:", temperature))
    return(FALSE)
  }
  # Set temperature
  .history_env$conversations[[id]]$temperature <- temperature
  # message(paste("Set temperature for conversation", id, "to:", temperature)) # Less verbose
  return(TRUE)
}

#' Sets the system message for the conversation with the given ID
#' @param id Conversation ID.
#' @param message New system message (string).
#' @return TRUE if set, FALSE if conversation doesn't exist or value is invalid.
#' @export
set_conversation_system_message <- function(id, message) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set system message for non-existent conversation: ", id)
    return(FALSE)
  }
  if (!is.character(message) || length(message) != 1) {
    warning("Invalid system message format.")
    return(FALSE)
  }
  # Set message
  .history_env$conversations[[id]]$system_message <- message
  # message(paste("Set system message for conversation", id)) # Less verbose
  return(TRUE)
}

#' Gets the chat history for the conversation with the given ID
#' @param id Conversation ID.
#' @return List of messages or NULL if conversation doesn't exist.
#' @export
get_conversation_history <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]]$history %||% list())
}

#' Gets the list of attachments for the conversation with the given ID
#' @param id Conversation ID.
#' @return List of attachments or NULL if conversation doesn't exist.
#' @export
get_conversation_attachments <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]]$attachments %||% list())
}

#' Gets the full conversation data object for the given ID
#' @param id Conversation ID.
#' @return List with conversation data or NULL if it doesn't exist.
#' @export
get_conversation_data <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]])
}



#' Resets the entire state of the history manager
#' @return Invisible NULL.
#' @export
reset_history_manager <- function() {
  message("Resetting history manager...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  invisible(NULL)
}

#' Adds an attachment to the active conversation
#' @param name Name of the attachment file.
#' @param content File content as a string.
#' @return TRUE if added, FALSE if a file with this name already exists or no active conversation.
#' @export
add_attachment_to_active_conversation <- function(name, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    warning("Cannot add attachment, no active conversation.")
    return(FALSE)
  }
  if (!active_id %in% names(.history_env$conversations)) {
    warning(paste("Active conversation (ID:", active_id, ") does not exist when adding attachment."))
    return(FALSE)
  }

  conv_attachments <- .history_env$conversations[[active_id]]$attachments %||% list()

  # Check if a file with this name already exists
  if (any(sapply(conv_attachments, function(att) !is.null(att$name) && att$name == name))) {
    message(paste("File named", name, "already exists in conversation", active_id, ". Not adding again."))
    return(FALSE) # Return FALSE so UI knows it wasn't added
  }

  new_attachment <- list(name = name, content = content)
  .history_env$conversations[[active_id]]$attachments <- c(conv_attachments, list(new_attachment))
  message(paste("Added attachment", name, "to conversation", active_id))

  return(TRUE)
}

#' Gets the list of attachments for the active conversation
#' @return List of attachments or an empty list.
#' @export
get_active_conversation_attachments <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    return(list())
  }
  return(active_conv$attachments %||% list())
}

# Helper %||%
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
