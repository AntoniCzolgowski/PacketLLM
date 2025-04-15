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
#' @examples
#' # Initialize the manager
#' first_conv_id <- initialize_history_manager()
#' print(paste("First conversation ID:", first_conv_id))
#'
#' # Verify initialization
#' print(paste("Active ID after init:", get_active_conversation_id())) # Should be NULL initially
#' print(paste("Total conversations after init:", length(get_all_conversation_ids())))
#'
#' # Clean up (reset state for other examples if needed)
#' reset_history_manager()
initialize_history_manager <- function() {
  message("Initializing history manager...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  # Default title translated
  first_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE, title = "New Conversation")
  message(paste("History manager initialized. Created first conversation:", first_id))
  # Set the first conversation as active *after* creating it
  set_active_conversation(first_id)
  message(paste("Set active conversation to:", first_id))
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
#' @examples
#' # Ensure manager is initialized (or reset)
#' reset_history_manager()
#' initialize_history_manager() # Creates one initial conversation
#'
#' # Create a new conversation without activating it
#' conv1_id <- create_new_conversation(activate = FALSE, title = "My First Topic")
#' print(paste("Created conv1 ID:", conv1_id))
#' print(paste("Active ID:", get_active_conversation_id())) # Should still be the initial one
#'
#' # Create another conversation and activate it immediately
#' conv2_id <- create_new_conversation(activate = TRUE, title = "My Second Topic")
#' print(paste("Created conv2 ID:", conv2_id))
#' print(paste("Active ID:", get_active_conversation_id())) # Should be conv2_id now
#'
#' # Check total conversations
#' print(paste("Total conversations:", length(get_all_conversation_ids())))
#'
#' # Clean up
#' delete_conversation(conv1_id)
#' delete_conversation(conv2_id)
#' reset_history_manager() # Reset fully
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_lock_id <- create_new_conversation(activate = TRUE)
#'
#' # Check status of a new conversation (should be FALSE)
#' print(paste("Locked initially:", is_conversation_started(conv_lock_id)))
#'
#' # Add a user message (does NOT lock the model)
#' add_message_to_active_history(role = "user", content = "First message")
#' print(paste("Locked after user msg:", is_conversation_started(conv_lock_id)))
#'
#' # Add an assistant message using the proper function
#' # This function correctly handles setting the model_locked flag internally
#' add_message_to_active_history(role = "assistant", content = "Assistant reply")
#'
#' # Check status after assistant message (should be TRUE)
#' print(paste("Locked after assistant msg:", is_conversation_started(conv_lock_id)))
#'
#' # Check non-existent conversation
#' print(paste("Locked for non-existent:", is_conversation_started("conv_non_existent")))
#'
#' # Clean up
#' delete_conversation(conv_lock_id)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_add_id <- create_new_conversation(activate = TRUE, title = "Initial Title")
#'
#' # Add first user message (should set title)
#' result1 <- add_message_to_active_history(role = "user", content = "This is the very first post.")
#' print("Result after first user message:")
#' print(result1)
#' print(paste("New Title:", get_conversation_title(conv_add_id)))
#'
#' # Add another user message (should just add message)
#' result2 <- add_message_to_active_history(role = "user", content = "Another question.")
#' print("Result after second user message:")
#' print(result2)
#'
#' # Add first assistant message (should lock model)
#' result3 <- add_message_to_active_history(role = "assistant", content = "Here is the answer.")
#' print("Result after first assistant message:")
#' print(result3)
#' print(paste("Is model locked?", is_conversation_started(conv_add_id)))
#'
#' # Add system message (just adds message)
#' result4 <- add_message_to_active_history(role = "system", content = "System notification.")
#' print("Result after system message:")
#' print(result4)
#'
#' # Check final history
#' print("Final history:")
#' print(get_conversation_history(conv_add_id))
#'
#' # Clean up
#' delete_conversation(conv_add_id)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_del_id1 <- create_new_conversation(activate = FALSE, title = "To Delete")
#' conv_del_id2 <- create_new_conversation(activate = TRUE, title = "To Keep Active")
#' print(paste("Initial conversations:", paste(get_all_conversation_ids(), collapse=", ")))
#' print(paste("Initial active ID:", get_active_conversation_id()))
#'
#' # Delete the non-active conversation
#' deleted1 <- delete_conversation(conv_del_id1)
#' print(paste("Deleted conv_del_id1:", deleted1))
#' print(paste("Conversations after delete 1:", paste(get_all_conversation_ids(), collapse=", ")))
#' print(paste("Active ID after delete 1:", get_active_conversation_id())) # Should be unchanged
#'
#' # Delete the active conversation
#' deleted2 <- delete_conversation(conv_del_id2)
#' print(paste("Deleted conv_del_id2:", deleted2))
#' print(paste("Conversations after delete 2:", paste(get_all_conversation_ids(), collapse=", ")))
#' print(paste("Active ID after delete 2:", get_active_conversation_id())) # Should be NULL
#'
#' # Try deleting a non-existent conversation
#' deleted3 <- delete_conversation("conv_non_existent")
#' print(paste("Deleted non-existent:", deleted3))
#'
#' # Clean up
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_set_id1 <- create_new_conversation(activate = FALSE, title = "Conv 1")
#' conv_set_id2 <- create_new_conversation(activate = FALSE, title = "Conv 2")
#' print(paste("Initial active ID:", get_active_conversation_id())) # NULL
#'
#' # Set conv1 as active
#' set_active_conversation(conv_set_id1)
#' print(paste("Active ID after set 1:", get_active_conversation_id())) # conv_set_id1
#'
#' # Set conv2 as active
#' set_active_conversation(conv_set_id2)
#' print(paste("Active ID after set 2:", get_active_conversation_id())) # conv_set_id2
#'
#' # Set non-existent ID (should warn and not change active ID)
#' set_active_conversation("conv_non_existent")
#' print(paste("Active ID after set non-existent:", get_active_conversation_id())) # conv_set_id2
#'
#' # Deactivate
#' set_active_conversation(NULL)
#' print(paste("Active ID after set NULL:", get_active_conversation_id())) # NULL
#'
#' # Clean up
#' delete_conversation(conv_set_id1)
#' delete_conversation(conv_set_id2)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_get_id <- create_new_conversation(activate = FALSE)
#'
#' # Check when no conversation is active
#' print(paste("Active ID initially:", get_active_conversation_id())) # NULL
#'
#' # Activate the conversation
#' set_active_conversation(conv_get_id)
#'
#' # Get the active ID
#' print(paste("Active ID after set:", get_active_conversation_id())) # conv_get_id
#'
#' # Deactivate
#' set_active_conversation(NULL)
#' print(paste("Active ID after unset:", get_active_conversation_id())) # NULL
#'
#' # Clean up
#' delete_conversation(conv_get_id)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_get_obj_id <- create_new_conversation(activate = TRUE, title = "Test Object")
#' add_message_to_active_history("user", "Message for object test")
#'
#' # Get the active conversation object
#' active_obj <- get_active_conversation()
#' if (!is.null(active_obj)) {
#'   print("Active conversation object:")
#'   print(str(active_obj)) # Use str() for concise structure view
#' } else {
#'   print("No active conversation found.")
#' }
#'
#' # Deactivate and try again
#' set_active_conversation(NULL)
#' active_obj_null <- get_active_conversation()
#' print(paste("Active object when none active:", is.null(active_obj_null))) # TRUE
#'
#' # Clean up
#' delete_conversation(conv_get_obj_id)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_hist_id <- create_new_conversation(activate = TRUE)
#'
#' # Get history when empty
#' print("Initial history:")
#' print(get_active_chat_history())
#'
#' # Add messages
#' add_message_to_active_history("user", "Question 1")
#' add_message_to_active_history("assistant", "Answer 1")
#'
#' # Get history after adding messages
#' print("History after messages:")
#' print(get_active_chat_history())
#'
#' # Deactivate and check (should be empty list)
#' set_active_conversation(NULL)
#' print("History when none active:")
#' print(get_active_chat_history())
#'
#' # Clean up
#' delete_conversation(conv_hist_id)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_title_id1 <- create_new_conversation(title = "Specific Title")
#' conv_title_id2 <- create_new_conversation() # Default title
#'
#' # Get title by ID
#' print(paste("Title for ID1:", get_conversation_title(conv_title_id1)))
#' print(paste("Title for ID2:", get_conversation_title(conv_title_id2)))
#'
#' # Get title for non-existent ID
#' print(paste("Title for non-existent:", is.null(get_conversation_title("bad_id")))) # TRUE
#'
#' # Clean up
#' delete_conversation(conv_title_id1)
#' delete_conversation(conv_title_id2)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' print(paste("IDs initially:", paste(get_all_conversation_ids(), collapse=","))) # Empty
#'
#' # Create conversations
#' conv_all_id1 <- create_new_conversation()
#' conv_all_id2 <- create_new_conversation()
#'
#' # Get all IDs
#' all_ids <- get_all_conversation_ids()
#' print(paste("IDs after creation:", paste(all_ids, collapse=",")))
#' print(paste("Number of conversations:", length(all_ids)))
#'
#' # Delete one and check again
#' delete_conversation(conv_all_id1)
#' print(paste("IDs after deletion:", paste(get_all_conversation_ids(), collapse=",")))
#'
#' # Clean up
#' delete_conversation(conv_all_id2) # Delete the remaining one
#' reset_history_manager()
get_all_conversation_ids <- function() {
  return(names(.history_env$conversations))
}

#' Gets the model name for the conversation with the given ID
#' @param id Conversation ID.
#' @return Model name (string) or NULL.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_model_id <- create_new_conversation() # Uses default model
#'
#' # Get the model for the conversation
#' model_name <- get_conversation_model(conv_model_id)
#' print(paste("Model for conversation:", model_name))
#'
#' # Check a non-existent conversation
#' print(paste("Model for non-existent:", is.null(get_conversation_model("bad_id")))) # TRUE
#'
#' # Clean up
#' delete_conversation(conv_model_id)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_set_model_id <- create_new_conversation(activate = TRUE)
#' print(paste("Initial model:", get_conversation_model(conv_set_model_id)))
#'
#' # Set a new valid model (assuming "gpt-4.1" is in available_openai_models)
#' result_set <- set_conversation_model(conv_set_model_id, "gpt-4.1")
#' print(paste("Model set successful:", result_set))
#' print(paste("Model after set:", get_conversation_model(conv_set_model_id)))
#'
#' # Try setting an invalid model name
#' result_invalid <- set_conversation_model(conv_set_model_id, "invalid-model-name")
#' print(paste("Invalid model set successful:", result_invalid))
#' print(paste("Model after invalid set:", get_conversation_model(conv_set_model_id))) # Unchanged
#'
#' # Simulate conversation start (add assistant message to lock model)
#' add_message_to_active_history("user", "Lock question")
#' add_message_to_active_history("assistant", "Lock answer") # This locks it
#'
#' # Try setting model after lock (should fail)
#' result_locked <- set_conversation_model(conv_set_model_id, "gpt-4o")
#' print(paste("Model set after lock successful:", result_locked))
#' print(paste("Model after locked attempt:", get_conversation_model(conv_set_model_id))) # Unchanged
#'
#' # Clean up
#' delete_conversation(conv_set_model_id)
#' reset_history_manager()
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
  # Ensure available_openai_models is accessible or define it locally for the example
  available_models_local <- c("gpt-4o", "gpt-4o-mini", "gpt-4.1", "o1", "o3-mini") # Example list
  if (exists("available_openai_models")) {
    available_models_local <- available_openai_models
  }

  if (!model_name %in% available_models_local) {
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_temp_id <- create_new_conversation()
#' initial_temp <- get_conversation_data(conv_temp_id)$temperature
#' print(paste("Initial temperature:", initial_temp))
#'
#' # Set a valid temperature
#' result_valid <- set_conversation_temperature(conv_temp_id, 0.85)
#' print(paste("Valid set successful:", result_valid))
#' print(paste("Temp after valid set:", get_conversation_data(conv_temp_id)$temperature))
#'
#' # Set an invalid temperature (outside 0-1)
#' result_invalid <- set_conversation_temperature(conv_temp_id, 1.5)
#' print(paste("Invalid set successful:", result_invalid))
#' print(paste("Temp after invalid set:", get_conversation_data(conv_temp_id)$temperature)) # Unchanged
#'
#' # Try on non-existent ID
#' result_bad_id <- set_conversation_temperature("bad_id", 0.5)
#' print(paste("Set on bad ID successful:", result_bad_id))
#'
#' # Clean up
#' delete_conversation(conv_temp_id)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_sys_id <- create_new_conversation()
#' initial_sys_msg <- get_conversation_data(conv_sys_id)$system_message
#' print(paste("Initial system message:", initial_sys_msg))
#'
#' # Set a valid system message
#' new_message = "You are an expert R programmer. Respond only with code."
#' result_valid <- set_conversation_system_message(conv_sys_id, new_message)
#' print(paste("Valid set successful:", result_valid))
#' print(paste("System message after set:", get_conversation_data(conv_sys_id)$system_message))
#'
#' # Try setting an invalid message (e.g., not a string)
#' result_invalid <- set_conversation_system_message(conv_sys_id, list("not a string"))
#' print(paste("Invalid set successful:", result_invalid))
#' # Check message after invalid attempt (break the long line)
#' final_msg_after_invalid <- get_conversation_data(conv_sys_id)$system_message
#' print(paste("System message after invalid:", final_msg_after_invalid)) # Unchanged
#'
#' # Clean up
#' delete_conversation(conv_sys_id)
#' reset_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_gethist_id <- create_new_conversation(activate = TRUE)
#'
#' # Get history for new conversation (should be empty list or NULL depending on implementation)
#' print("Initial history by ID:")
#' print(get_conversation_history(conv_gethist_id)) # Likely list()
#'
#' # Add messages
#' add_message_to_active_history("user", "Hi there")
#' add_message_to_active_history("assistant", "Hello")
#'
#' # Get history again
#' print("History after messages:")
#' print(get_conversation_history(conv_gethist_id))
#'
#' # Get history for non-existent ID
#' print("History for non-existent:")
#' print(get_conversation_history("bad_id")) # Should be NULL
#'
#' # Clean up
#' delete_conversation(conv_gethist_id)
#' reset_history_manager()
get_conversation_history <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]]$history %||% list())
}

#' Gets the list of attachments for the conversation with the given ID
#' @param id Conversation ID.
#' @return List of attachments or NULL if conversation doesn't exist.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_attach_id <- create_new_conversation(activate = TRUE)
#'
#' # Get attachments for new conversation (empty list)
#' print("Initial attachments by ID:")
#' print(get_conversation_attachments(conv_attach_id)) # list()
#'
#' # Add an attachment
#' add_attachment_to_active_conversation("file1.txt", "File content here")
#'
#' # Get attachments again
#' print("Attachments after adding:")
#' print(get_conversation_attachments(conv_attach_id))
#'
#' # Get attachments for non-existent ID
#' print("Attachments for non-existent:")
#' print(get_conversation_attachments("bad_id")) # NULL
#'
#' # Clean up
#' delete_conversation(conv_attach_id)
#' reset_history_manager()
get_conversation_attachments <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]]$attachments %||% list())
}

#' Gets the full conversation data object for the given ID
#' @param id Conversation ID.
#' @return List with conversation data or NULL if it doesn't exist.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_getdata_id <- create_new_conversation(title = "Data Test")
#' set_conversation_temperature(conv_getdata_id, 0.9)
#'
#' # Get conversation data by ID
#' data_obj <- get_conversation_data(conv_getdata_id)
#' if (!is.null(data_obj)) {
#'   print("Conversation data object:")
#'   print(str(data_obj))
#' }
#'
#' # Get data for non-existent ID
#' print(paste("Data for non-existent:", is.null(get_conversation_data("bad_id")))) # TRUE
#'
#' # Clean up
#' delete_conversation(conv_getdata_id)
#' reset_history_manager()
get_conversation_data <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]])
}



#' Resets the entire state of the history manager
#' @return Invisible NULL.
#' @export
#' @examples
#' # Setup: Initialize and add some data
#' reset_history_manager() # Ensure clean start
#' initialize_history_manager()
#' conv_reset_id <- create_new_conversation(activate = TRUE)
#' add_message_to_active_history("user", "Message before reset")
#' print(paste("Conversations before reset:", length(get_all_conversation_ids())))
#' print(paste("Active ID before reset:", get_active_conversation_id()))
#'
#' # Reset the manager
#' reset_history_manager()
#'
#' # Verify state after reset
#' print(paste("Conversations after reset:", length(get_all_conversation_ids()))) # 0
#' print(paste("Active ID after reset:", is.null(get_active_conversation_id()))) # TRUE
#'
#' # Note: After reset, you might need to initialize again if needed
#' # initialize_history_manager()
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_addattach_id <- create_new_conversation(activate = TRUE)
#'
#' # Add a first attachment
#' result1 <- add_attachment_to_active_conversation("report.txt", "Summary of findings.")
#' print(paste("Added first attachment:", result1))
#' print("Attachments after first add:")
#' print(get_active_conversation_attachments())
#'
#' # Add a second, different attachment
#' result2 <- add_attachment_to_active_conversation("code.R", "x <- function(y) { y + 1 }")
#' print(paste("Added second attachment:", result2))
#' print("Attachments after second add:")
#' print(get_active_conversation_attachments())
#'
#' # Try adding an attachment with the same name (should fail)
#' result3 <- add_attachment_to_active_conversation("report.txt", "Updated summary.")
#' print(paste("Added duplicate name attachment:", result3))
#' print("Attachments after duplicate attempt:")
#' print(get_active_conversation_attachments()) # Should be unchanged from previous step
#'
#' # Try adding when no conversation is active
#' set_active_conversation(NULL)
#' result4 <- add_attachment_to_active_conversation("another.txt", "Content")
#' print(paste("Added attachment when none active:", result4))
#'
#' # Clean up
#' reset_history_manager() # Also deletes the conversation
#' # delete_conversation(conv_addattach_id) # Not needed after reset
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
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_getactiveattach_id <- create_new_conversation(activate = TRUE)
#'
#' # Get attachments when none added
#' print("Attachments initially:")
#' print(get_active_conversation_attachments()) # list()
#'
#' # Add some attachments
#' add_attachment_to_active_conversation("data.csv", "col1,col2\n1,2")
#' add_attachment_to_active_conversation("notes.txt", "Reminder")
#'
#' # Get attachments again
#' print("Attachments after adding:")
#' attachments_list <- get_active_conversation_attachments()
#' print(attachments_list)
#' print(paste("Number of attachments:", length(attachments_list)))
#'
#' # Deactivate and check (should be empty list)
#' set_active_conversation(NULL)
#' print("Attachments when none active:")
#' print(get_active_conversation_attachments()) # list()
#'
#' # Clean up
#' reset_history_manager()
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
