# history_manager.R

# Environment to store history state
.history_env <- new.env(parent = emptyenv())
.history_env$conversations <- list()
.history_env$active_conversation_id <- NULL
.history_env$conversation_counter <- 0

# Helper function to check verbose option
.is_verbose <- function() {
  getOption("PacketLLM.verbose", default = FALSE)
}

#' Generates a unique conversation ID
#' @return Character string. A unique identifier for a conversation, combining
#'         a counter and the system time.
#' @noRd
generate_conversation_id <- function() {
  .history_env$conversation_counter <- .history_env$conversation_counter + 1
  paste0("conv_", .history_env$conversation_counter, "_", as.integer(Sys.time()))
}

#' Initializes the history manager
#'
#' Clears the current history state (all conversations and settings) and creates
#' a single new, empty conversation, setting it as the active conversation.
#' Optionally prints a message to the console in interactive sessions.
#'
#' @return Character string. The ID of the first, automatically created conversation
#'         after initialization.
#' @export
#' @examples
#' # Initialize the manager. A message might appear in the console if run interactively.
#' first_conv_id <- initialize_history_manager()
#' print(paste("First conversation ID:", first_conv_id))
#'
#' # Verify initialization
#' active_id_after_init <- get_active_conversation_id() # Should be first_conv_id
#' print(paste("Active ID after init:", active_id_after_init))
#' all_ids_after_init <- get_all_conversation_ids() # Should have 1 element
#' print(paste("Total conversations after init:", length(all_ids_after_init)))
#'
#' # Clean up (reset state for other examples if needed)
#' reset_history_manager()
initialize_history_manager <- function() {
  # Message only in interactive sessions - CRAN compliance
  if (interactive()) {
    message("Initializing history manager...")
  }
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  # Default title
  first_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE, title = "New Conversation")
  # Verbose message only if option set
  if (.is_verbose()) message(paste("History manager initialized. Created first conversation:", first_id))
  # Set the first conversation as active *after* creating it
  set_active_conversation(first_id)
  # Verbose message only if option set
  if (.is_verbose()) message(paste("Set active conversation to:", first_id))
  return(first_id)
}


#' Creates a new conversation
#'
#' Adds a new, empty conversation structure to the internal history store.
#' Optionally sets the new conversation as the active one.
#'
#' @param activate Logical. Should the new conversation be set as active immediately? (Default: `FALSE`).
#' @param add_initial_settings Logical. Should default settings (model, temperature,
#'        system message) be added to the conversation structure? (Default: `TRUE`).
#' @param title Character string or `NULL`. An initial title for the conversation.
#'        If `NULL` (default), a title is generated based on the creation time.
#' @return Character string. The unique ID assigned to the newly created conversation.
#' @export
#' @examples
#' # Ensure manager is initialized (or reset)
#' reset_history_manager()
#' initialize_history_manager() # Creates one initial conversation
#' initial_active_id <- get_active_conversation_id()
#'
#' # Create a new conversation without activating it
#' conv1_id <- create_new_conversation(activate = FALSE, title = "My First Topic")
#' print(paste("Created conv1 ID:", conv1_id))
#' current_active_id <- get_active_conversation_id() # Should still be the initial one
#' print(paste("Active ID:", current_active_id))
#'
#' # Create another conversation and activate it immediately
#' conv2_id <- create_new_conversation(activate = TRUE, title = "My Second Topic")
#' print(paste("Created conv2 ID:", conv2_id))
#' current_active_id_2 <- get_active_conversation_id() # Should be conv2_id now
#' print(paste("Active ID:", current_active_id_2))
#'
#' # Check total conversations
#' total_convs <- length(get_all_conversation_ids())
#' print(paste("Total conversations:", total_convs))
#'
#' # Clean up by resetting (which deletes all)
#' reset_history_manager()
create_new_conversation <- function(activate = FALSE, add_initial_settings = TRUE, title = NULL) {
  conv_id <- generate_conversation_id()
  if (is.null(title)) {
    # Default title format
    title <- paste("Conversation", format(Sys.time(), "%H:%M:%S"))
  }
  # Verbose message only if option set
  if (.is_verbose()) message(paste("Creating new conversation with ID:", conv_id, "and title:", title))

  # Determine default model safely
  default_model <- "gpt-4o" # Fallback
  if (exists("available_openai_models", where = "package:PacketLLM", inherits = FALSE)) {
    available_models_pkg <- get("available_openai_models", envir = asNamespace("PacketLLM"))
    if (length(available_models_pkg) > 0) {
      default_model <- available_models_pkg[1]
    }
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
    # Set system message field, not history
    new_conv$system_message <- default_system_message
    if (.is_verbose()) message(paste("Added default settings (SysMsg, Temp) and model (", default_model, ") to conversation", conv_id))
  } else {
    new_conv$system_message <- "" # Empty if not adding settings
    if (.is_verbose()) message(paste("Set default model (", default_model, ") for conversation", conv_id, " (without initial settings)"))
  }

  .history_env$conversations[[conv_id]] <- new_conv

  if (activate) {
    set_active_conversation(conv_id)
  }
  return(conv_id)
}

#' Checks if the conversation has started (model locked)
#'
#' Determines if the model for a given conversation is locked, which typically
#' occurs after the first assistant message has been added. Once locked, the
#' model for the conversation usually cannot be changed.
#'
#' @param id Character string. The ID of the conversation to check.
#' @return Logical. `TRUE` if the model for the conversation is locked,
#'         `FALSE` otherwise or if the conversation with the specified `id`
#'         does not exist.
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
#' # Add an assistant message (using the internal function locks the model)
#' add_message_to_active_history(role = "assistant", content = "Assistant reply")
#'
#' # Check status after assistant message (should be TRUE)
#' print(paste("Locked after assistant msg:", is_conversation_started(conv_lock_id)))
#'
#' # Check non-existent conversation
#' print(paste("Locked for non-existent:", is_conversation_started("conv_non_existent"))) # FALSE
#'
#' # Clean up
#' reset_history_manager()
is_conversation_started <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    # warning("Attempting to check lock status for non-existent conversation: ", id) # Keep warning optional
    return(FALSE)
  }
  # Access the model_locked flag safely using %||%
  return(.history_env$conversations[[id]]$model_locked %||% FALSE)
}


#' Adds a message to the active conversation's history
#'
#' Appends a message with the specified role and content to the history list
#' of the currently active conversation. Handles automatic title generation on
#' the first user message and locks the conversation model upon adding the
#' first assistant message.
#'
#' @param role Character string. The role of the message author, must be one of
#'        "user", "assistant", or "system".
#' @param content Character string. The content of the message.
#' @return A list indicating the result of the operation. Possible structures:
#'         - `list(type = "title_set", new_title = "...")`: If this was the first
#'           user message and the title was automatically set.
#'         - `list(type = "assistant_locked_model")`: If this was the first assistant
#'           message, causing the model to be locked.
#'         - `list(type = "message_added")`: If a message was added without
#'           triggering title setting or model locking.
#'         - `list(type = "error", message = "...")`: If an error occurred (e.g.,
#'           no active conversation, invalid role, conversation vanished).
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
  if (!is.character(content) || length(content) != 1) {
    warning("Invalid message content: must be a single character string.")
    return(list(type = "error", message = "Invalid message content."))
  }

  # Check if conversation exists just before modification
  if (!active_id %in% names(.history_env$conversations)) {
    warning("Active conversation (ID: ", active_id, ") does not exist when adding message.")
    return(list(type = "error", message = "Active conversation does not exist."))
  }

  # Reference conversation directly for modification
  conv <- .history_env$conversations[[active_id]]

  return_value <- list(type = "message_added") # Default return value
  new_message <- list(role = role, content = content)

  # Check if the model should be locked (first assistant response)
  # Use safe access with %||% in case model_locked is NULL initially
  should_lock_model <- role == "assistant" && !(conv$model_locked %||% FALSE)

  # Add message to history (modify the referenced list directly)
  conv$history <- c(conv$history %||% list(), list(new_message))
  if (.is_verbose()) message(paste("Added", role, "message to", active_id))

  # Lock the model if needed (modify the referenced list directly)
  if (should_lock_model) {
    conv$model_locked <- TRUE
    if (.is_verbose()) message(paste("Model for conversation", active_id, "has been locked."))
    # Ensure return value reflects locking if no title was set
    if (return_value$type == "message_added") {
      return_value <- list(type = "assistant_locked_model")
    }
  }

  # --- Title setting logic ---
  # Recalculate user message count based on updated history
  conv_history_updated <- conv$history %||% list()
  user_message_count <- sum(sapply(conv_history_updated, function(m) !is.null(m$role) && m$role == "user"))
  is_first_user_message_ever <- role == "user" && user_message_count == 1

  if (is_first_user_message_ever && nzchar(trimws(content))) {

    # ---TITLE LOGIC START ---
    title_content_base <- content # Start with the full content
    attachment_marker <- "\n\n<strong>Attached:</strong>"
    attachment_marker_start <- "<strong>Attached:</strong>"

    # Scenario 2: Text AND Attachments
    if (grepl(attachment_marker, title_content_base, fixed = TRUE)) {
      title_content_processed <- trimws(strsplit(title_content_base, attachment_marker, fixed = TRUE)[[1]][1])
      if (.is_verbose()) message("Title generation: Detected text and attachments. Using only text part for title.")
      # Scenario 1: Attachments ONLY
    } else if (startsWith(title_content_base, attachment_marker_start)) {
      title_content_processed <- gsub("<strong>|</strong>", "", title_content_base, fixed = FALSE) # Use fixed=FALSE for regex |
      if (.is_verbose()) message("Title generation: Detected attachments only. Using message content without bold tags for title.")
      # Default Scenario: Regular text message
    } else {
      title_content_processed <- title_content_base
      if (.is_verbose()) message("Title generation: Detected regular text message.")
    }

    # Use the processed content for title generation
    words <- strsplit(trimws(title_content_processed), "\\s+")[[1]]
    words <- words[nzchar(words)] # Remove empty strings

    # Existing truncation logic
    max_words <- 5
    max_chars <- 40
    new_title_base <- paste(head(words, max_words), collapse = " ")

    # Apply truncation based on word count first, then char count
    if (length(words) > max_words) {
      new_title <- paste0(new_title_base, "...")
    } else {
      new_title <- new_title_base
    }
    # Apply char limit if necessary (even after word truncation)
    if (nchar(new_title) > max_chars) {
      new_title <- paste0(substr(new_title, 1, max_chars - 3), "...")
    }

    # Handle cases where content was only whitespace or became empty after processing
    if (!nzchar(trimws(new_title)) || new_title == "...") {
      new_title <- paste("Chat", format(Sys.time(), "%M%S")) # Use "Chat" instead of "Conversation"
    }

    # Update title in the referenced list
    conv$title <- new_title
    if (.is_verbose()) message(paste("Set title for", active_id, "to:", new_title))
    # Set specific return value for title setting
    return_value <- list(type = "title_set", new_title = new_title)
  }
  # --- End of title logic ---

  # Assign the modified conversation back to the environment
  .history_env$conversations[[active_id]] <- conv

  return(return_value)
}


#' Deletes the conversation with the given ID
#'
#' Removes the specified conversation from the internal history store. If the
#' deleted conversation was the active one, the active conversation ID is reset
#' to `NULL`.
#'
#' @param id Character string. The ID of the conversation to delete.
#' @return Logical. `TRUE` if the conversation was found and successfully deleted.
#'         `FALSE` if no conversation with the specified `id` existed.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_del_id1 <- create_new_conversation(activate = FALSE, title = "To Delete")
#' conv_del_id2 <- create_new_conversation(activate = TRUE, title = "To Keep Active")
#' all_ids_before_del <- get_all_conversation_ids()
#' print(paste("Initial conversations:", paste(all_ids_before_del, collapse=", ")))
#' print(paste("Initial active ID:", get_active_conversation_id()))
#'
#' # Delete the non-active conversation
#' deleted1 <- delete_conversation(conv_del_id1)
#' print(paste("Deleted conv_del_id1:", deleted1))
#' all_ids_after_del1 <- get_all_conversation_ids()
#' print(paste("Conversations after delete 1:", paste(all_ids_after_del1, collapse=", ")))
#' print(paste("Active ID after delete 1:", get_active_conversation_id())) # Should be unchanged
#'
#' # Delete the active conversation
#' deleted2 <- delete_conversation(conv_del_id2)
#' print(paste("Deleted conv_del_id2:", deleted2))
#' all_ids_after_del2 <- get_all_conversation_ids() # Should be empty now
#' # MODIFIED LINE (was too long)
#' print(paste("Conversations after delete 2:", paste(all_ids_after_del2, collapse=", ")))
#' print(paste("Active ID after delete 2:", get_active_conversation_id())) # Should be NULL
#'
#' # Try deleting a non-existent conversation
#' deleted3 <- delete_conversation("conv_non_existent")
#' print(paste("Deleted non-existent:", deleted3)) # FALSE
#'
#' # Clean up
#' reset_history_manager()
delete_conversation <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    # warning("Attempting to delete non-existent conversation with ID: ", id) # Optional warning
    return(FALSE)
  }
  # Verbose message only if option set
  if (.is_verbose()) message(paste("Deleting conversation with ID:", id))
  .history_env$conversations[[id]] <- NULL

  current_active_id <- .history_env$active_conversation_id
  # Check if the one being deleted is the active one
  if (!is.null(current_active_id) && current_active_id == id) {
    if (.is_verbose()) message("Deleted the active conversation. Resetting active_conversation_id.")
    .history_env$active_conversation_id <- NULL
  }
  return(TRUE)
}

#' Sets the active conversation
#'
#' Designates a conversation, specified by its ID, as the currently active one.
#' Setting `id` to `NULL` deactivates any currently active conversation.
#'
#' @param id Character string (the ID of the conversation to activate) or `NULL`.
#' @return Invisible `NULL` (`invisible(NULL)`). This function is called for its
#'         side effect of changing the active conversation state. It produces a
#'         warning if attempting to activate a non-existent conversation ID.
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
#' active_after_invalid_set <- get_active_conversation_id() # Still conv_set_id2
#' print(paste("Active ID after set non-existent:", active_after_invalid_set))
#'
#' # Deactivate by setting to NULL
#' set_active_conversation(NULL)
#' print(paste("Active ID after set NULL:", get_active_conversation_id())) # NULL
#'
#' # Clean up
#' reset_history_manager()
set_active_conversation <- function(id) {
  if (is.null(id)) {
    # Only message if there *was* an active conversation and verbose
    if (!is.null(.history_env$active_conversation_id) && .is_verbose()) {
      message("Deactivated the active conversation.")
    }
    .history_env$active_conversation_id <- NULL
    return(invisible(NULL))
  }

  # Check if the provided ID is valid before setting
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set a non-existent conversation as active: ", id)
    # Do not change the active_conversation_id if the target doesn't exist
    return(invisible(NULL))
  }

  # Set if it's different from the current active ID or if none is active
  if (is.null(.history_env$active_conversation_id) || .history_env$active_conversation_id != id) {
    # Verbose message only if option set
    if (.is_verbose()) message(paste("Set active conversation to ID:", id))
    .history_env$active_conversation_id <- id
  } else {
    # Message if trying to set the already active one (optional and verbose)
    # if (.is_verbose()) message(paste("Conversation", id, "is already active."))
  }
  return(invisible(NULL))
}

#' Gets the ID of the active conversation
#'
#' Retrieves the identifier of the conversation currently marked as active.
#'
#' @return Character string or `NULL`. The ID of the currently active conversation,
#'         or `NULL` if no conversation is active or if the previously active
#'         conversation ID points to a conversation that no longer exists.
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
#' reset_history_manager()
get_active_conversation_id <- function() {
  active_id <- .history_env$active_conversation_id
  # Validate that the active_id still points to an existing conversation
  if (!is.null(active_id) && !active_id %in% names(.history_env$conversations)) {
    warning(paste("Active ID", active_id, "points to a non-existent conversation. Resetting active ID to NULL."))
    .history_env$active_conversation_id <- NULL
    return(NULL)
  }
  return(active_id)
}

#' Gets the full object of the active conversation
#'
#' Retrieves the complete data structure (a list) associated with the currently
#' active conversation.
#'
#' @return List or `NULL`. A list containing all data for the active conversation
#'         (id, title, history, attachments, settings, etc.), or `NULL` if no
#'         conversation is currently active.
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
#' reset_history_manager()
get_active_conversation <- function() {
  active_id <- get_active_conversation_id() # Use the validated active ID
  if (is.null(active_id)) {
    return(NULL)
  }
  # ID is guaranteed to exist here due to check in get_active_conversation_id()
  return(.history_env$conversations[[active_id]])
}

#' Gets the chat history for the active conversation
#'
#' Retrieves the list of messages associated with the currently active conversation.
#'
#' @return List. A list containing the message history (each element is a list
#'         with `role` and `content`) for the currently active conversation.
#'         Returns an empty list (`list()`) if no conversation is active or if
#'         the active conversation has no history yet.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_hist_id <- create_new_conversation(activate = TRUE)
#'
#' # Get history when empty
#' print("Initial history:")
#' print(get_active_chat_history()) # list()
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
#' print(get_active_chat_history()) # list()
#'
#' # Clean up
#' reset_history_manager()
get_active_chat_history <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) {
    return(list())
  }
  # Use safe access with %||%
  return(active_conv$history %||% list())
}

#' Gets the title of the conversation with the given ID
#'
#' Retrieves the title associated with a specific conversation ID.
#'
#' @param id Character string. The ID of the conversation whose title is requested.
#' @return Character string or `NULL`. The title of the conversation associated
#'         with the specified `id`. Returns `NULL` if the conversation does not exist.
#'         Returns a placeholder string like \samp{[No Title - ID: ...]} if the title
#'         field happens to be `NULL` internally (should not normally occur).
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_title_id1 <- create_new_conversation(title = "Specific Title")
#' conv_title_id2 <- create_new_conversation() # Default title generated
#'
#' # Get title by ID
#' print(paste("Title for ID1:", get_conversation_title(conv_title_id1)))
#' print(paste("Title for ID2:", get_conversation_title(conv_title_id2)))
#'
#' # Get title for non-existent ID
#' print(paste("Title for non-existent:", get_conversation_title("bad_id"))) # NULL
#'
#' # Clean up
#' reset_history_manager()
get_conversation_title <- function(id) {
  if (!id %in% names(.history_env$conversations)) {
    return(NULL)
  }
  # Access the title safely using %||%
  # Use a generic placeholder if title is somehow NULL
  # MODIFIED RETURN SECTION (used \samp{})
  return(.history_env$conversations[[id]]$title %||% paste("[Untitled Conversation - ID:", id, "]"))
}

#' Gets a list of IDs of all existing conversations
#'
#' Retrieves the unique identifiers for all conversations currently stored in the manager.
#'
#' @return Character vector. A vector containing the unique IDs of all currently
#'         stored conversations. Returns an empty character vector (`character(0)`)
#'         if no conversations exist.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' initial_ids <- get_all_conversation_ids() # Should be character(0)
#' # MODIFIED LINE (was too long)
#' print(paste("IDs initially:", paste(initial_ids, collapse=",")))
#'
#' # Create conversations
#' conv_all_id1 <- create_new_conversation()
#' conv_all_id2 <- create_new_conversation()
#'
#' # Get all IDs
#' all_ids <- get_all_conversation_ids()
#' print(paste("IDs after creation:", paste(all_ids, collapse=",")))
#' print(paste("Number of conversations:", length(all_ids))) # 2
#'
#' # Delete one and check again
#' delete_conversation(conv_all_id1)
#' ids_after_del <- get_all_conversation_ids() # Only ID2
#' print(paste("IDs after deletion:", paste(ids_after_del, collapse=",")))
#'
#' # Clean up
#' reset_history_manager()
get_all_conversation_ids <- function() {
  # names() returns character(0) if the list is empty
  return(names(.history_env$conversations))
}

#' Gets the model name for the conversation with the given ID
#'
#' Retrieves the name of the language model assigned to a specific conversation.
#'
#' @param id Character string. The ID of the conversation.
#' @return Character string or `NULL`. The name of the OpenAI model assigned to the
#'         conversation with the specified `id`. Returns `NULL` if the conversation
#'         does not exist. Returns a fallback model name if the model field happens
#'         to be `NULL` internally.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_model_id <- create_new_conversation() # Uses default model from initialization
#'
#' # Get the model for the conversation
#' model_name <- get_conversation_model(conv_model_id)
#' print(paste("Model for conversation:", model_name))
#'
#' # Check a non-existent conversation
#' print(paste("Model for non-existent:", get_conversation_model("bad_id"))) # NULL
#'
#' # Clean up
#' reset_history_manager()
get_conversation_model <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  # Access the model safely using %||%
  return(.history_env$conversations[[id]]$model %||% "gpt-4o") # Provide a fallback
}

#' Sets the model for the conversation, if it hasn't started
#'
#' Assigns a specified OpenAI language model to a conversation, but only if the
#' conversation exists and has not yet "started" (i.e., no assistant messages
#' have been added, `is_conversation_started(id)` is `FALSE`). The model name
#' must be one of the available models listed in `available_openai_models`.
#'
#' @param id Character string. The ID of the conversation.
#' @param model_name Character string. The name of the new model (must be one of
#'        `PacketLLM::available_openai_models`).
#' @return Logical. `TRUE` if the model was successfully set for the conversation.
#'         `FALSE` if the conversation does not exist, the conversation has already
#'         started (model is locked), or the `model_name` is not valid/available.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_set_model_id <- create_new_conversation(activate = TRUE)
#' print(paste("Initial model:", get_conversation_model(conv_set_model_id)))
#'
#' # Set a new valid model (use a known available model)
#' # Ensure the model exists in PacketLLM::available_openai_models
#' target_model <- "gpt-4o-mini" # Assuming this is usually available
#' if (target_model %in% PacketLLM::available_openai_models) {
#'   result_set <- set_conversation_model(conv_set_model_id, target_model)
#'   print(paste("Model set successful:", result_set))
#'   print(paste("Model after set:", get_conversation_model(conv_set_model_id)))
#' } else {
#'    message(paste("Skipping set model example: Target model", target_model, "not in list."))
#' }
#'
#' # Try setting an invalid model name
#' result_invalid <- set_conversation_model(conv_set_model_id, "invalid-model-name")
#' print(paste("Invalid model set successful:", result_invalid)) # FALSE
#' model_after_invalid <- get_conversation_model(conv_set_model_id) # Unchanged
#' print(paste("Model after invalid set:", model_after_invalid))
#'
#' # Simulate conversation start by adding an assistant message
#' add_message_to_active_history("user", "Lock question")
#' add_message_to_active_history("assistant", "Lock answer") # This locks it
#'
#' # Try setting model after lock (should fail)
#' result_locked <- set_conversation_model(conv_set_model_id, "gpt-4o") # Try setting back
#' print(paste("Model set after lock successful:", result_locked)) # FALSE
#' model_after_locked <- get_conversation_model(conv_set_model_id) # Unchanged
#' print(paste("Model after locked attempt:", model_after_locked))
#'
#' # Clean up
#' reset_history_manager()
set_conversation_model <- function(id, model_name) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set model for non-existent conversation: ", id)
    return(FALSE)
  }
  if (is_conversation_started(id)) {
    warning("Cannot change model - conversation has already started for ID: ", id, call. = FALSE)
    return(FALSE)
  }

  # Validate model_name type
  if (!is.character(model_name) || length(model_name) != 1) {
    warning("Invalid model_name: must be a single character string.")
    return(FALSE)
  }

  # Safely get available models from package namespace
  available_models_pkg <- character(0)
  if (exists("available_openai_models", where = "package:PacketLLM", inherits = FALSE)) {
    available_models_pkg <- get("available_openai_models", envir = asNamespace("PacketLLM"))
  } else {
    warning("Could not retrieve available_openai_models from PacketLLM namespace.")
    # Use a minimal fallback list for basic check if namespace access fails
    available_models_pkg <- c("gpt-4o", "gpt-4o-mini", "gpt-4.1", "o1", "o3-mini")
  }

  if (!model_name %in% available_models_pkg) {
    warning(paste("Attempting to set unavailable model:", model_name, "for conversation", id,
                  ". Available models are:", paste(available_models_pkg, collapse=", ")))
    return(FALSE)
  }

  # Set the model (modifying the list in the environment)
  .history_env$conversations[[id]]$model <- model_name
  # Verbose message only if option set
  if (.is_verbose()) message(paste("Set model for conversation", id, "to:", model_name))
  return(TRUE)
}


#' Sets the temperature for the conversation with the given ID
#'
#' Updates the temperature setting (controls creativity/randomness of responses)
#' for a specific conversation.
#'
#' @param id Character string. The ID of the conversation.
#' @param temperature Numeric. The new temperature value, must be a single number
#'        between 0 and 1 (inclusive).
#' @return Logical. `TRUE` if the temperature was successfully updated. `FALSE` if
#'         the conversation does not exist or if the provided `temperature` value
#'         is invalid (not a single number between 0 and 1).
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_temp_id <- create_new_conversation()
#' initial_temp <- get_conversation_data(conv_temp_id)$temperature
#' print(paste("Initial temperature:", initial_temp)) # Default temp
#'
#' # Set a valid temperature
#' result_valid <- set_conversation_temperature(conv_temp_id, 0.85)
#' print(paste("Valid set successful:", result_valid)) # TRUE
#' temp_after_valid <- get_conversation_data(conv_temp_id)$temperature # 0.85
#' print(paste("Temp after valid set:", temp_after_valid))
#'
#' # Set an invalid temperature (outside 0-1)
#' result_invalid <- set_conversation_temperature(conv_temp_id, 1.5)
#' print(paste("Invalid set successful:", result_invalid)) # FALSE
#' # MODIFIED LINE (was too long)
#' temp_after_invalid <- get_conversation_data(conv_temp_id)$temperature # Unchanged (0.85)
#' print(paste("Temp after invalid set:", temp_after_invalid))
#'
#' # Set an invalid temperature (wrong type)
#' result_invalid_type <- set_conversation_temperature(conv_temp_id, "high")
#' print(paste("Invalid type set successful:", result_invalid_type)) # FALSE
#'
#' # Try on non-existent ID
#' result_bad_id <- set_conversation_temperature("bad_id", 0.5)
#' print(paste("Set on bad ID successful:", result_bad_id)) # FALSE
#'
#' # Clean up
#' reset_history_manager()
set_conversation_temperature <- function(id, temperature) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set temperature for non-existent conversation: ", id)
    return(FALSE)
  }
  # More robust check for temperature validity
  if (!is.numeric(temperature) || length(temperature) != 1 || is.na(temperature) || temperature < 0 || temperature > 1) {
    warning(paste("Invalid temperature value:", temperature, ". Must be a single non-NA number between 0 and 1."))
    return(FALSE)
  }
  # Set temperature
  .history_env$conversations[[id]]$temperature <- temperature
  # No message needed for simple setting
  return(TRUE)
}

#' Sets the system message for the conversation with the given ID
#'
#' Updates the system message (instructions provided to the language model)
#' for a specific conversation.
#'
#' @param id Character string. The ID of the conversation.
#' @param message Character string. The new system message content (must be a single string).
#' @return Logical. `TRUE` if the system message was successfully updated. `FALSE`
#'         if the conversation does not exist or if the provided `message` is not a
#'         single character string.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_sys_id <- create_new_conversation()
#' initial_sys_msg <- get_conversation_data(conv_sys_id)$system_message
#' print(paste("Initial system message:", initial_sys_msg)) # Default message
#'
#' # Set a valid system message
#' new_message <- "You are an expert R programmer. Respond only with code."
#' result_valid <- set_conversation_system_message(conv_sys_id, new_message)
#' print(paste("Valid set successful:", result_valid)) # TRUE
#' msg_after_set <- get_conversation_data(conv_sys_id)$system_message
#' print(paste("System message after set:", msg_after_set))
#'
#' # Try setting an invalid message (e.g., not a single string)
#' result_invalid <- set_conversation_system_message(conv_sys_id, list("not a string"))
#' print(paste("Invalid set successful:", result_invalid)) # FALSE
#'
#' # Try setting an invalid message (vector of strings)
#' result_invalid_vec <- set_conversation_system_message(conv_sys_id, c("Line 1", "Line 2"))
#' print(paste("Invalid vector set successful:", result_invalid_vec)) # FALSE
#'
#' # Check message after invalid attempts
#' final_msg_after_invalid <- get_conversation_data(conv_sys_id)$system_message # Unchanged
#' print(paste("System message after invalid attempts:", final_msg_after_invalid))
#'
#' # Clean up
#' reset_history_manager()
set_conversation_system_message <- function(id, message) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set system message for non-existent conversation: ", id)
    return(FALSE)
  }
  # More robust check for message validity
  if (!is.character(message) || length(message) != 1 || is.na(message)) {
    warning("Invalid system message format. Must be a single, non-NA character string.")
    return(FALSE)
  }
  # Set message
  .history_env$conversations[[id]]$system_message <- message
  # No message needed for simple setting
  return(TRUE)
}

#' Gets the chat history for the conversation with the given ID
#'
#' Retrieves the list of messages associated with a specific conversation ID.
#'
#' @param id Character string. The ID of the conversation.
#' @return List or `NULL`. A list containing the message history (each element is a
#'         list with `role` and `content`) for the conversation specified by `id`.
#'         Returns `NULL` if the conversation does not exist. Returns an empty list
#'         (`list()`) if the conversation exists but has no history yet.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_gethist_id <- create_new_conversation(activate = TRUE)
#'
#' # Get history for new conversation
#' print("Initial history by ID:")
#' print(get_conversation_history(conv_gethist_id)) # list()
#'
#' # Add messages using the exported function
#' add_message_to_active_history("user", "Hi there")
#' add_message_to_active_history("assistant", "Hello")
#'
#' # Get history again
#' print("History after messages:")
#' print(get_conversation_history(conv_gethist_id))
#'
#' # Get history for non-existent ID
#' print("History for non-existent:")
#' print(get_conversation_history("bad_id")) # NULL
#'
#' # Clean up
#' reset_history_manager()
get_conversation_history <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  # Safe access using %||%
  return(.history_env$conversations[[id]]$history %||% list())
}

#' Gets the list of attachments for the conversation with the given ID
#'
#' Retrieves the list of attachments (files provided as context) associated with
#' a specific conversation ID.
#'
#' @param id Character string. The ID of the conversation.
#' @return List or `NULL`. A list where each element is itself a list containing
#'         `name` (character) and `content` (character) for an attachment
#'         associated with the conversation specified by `id`. Returns `NULL` if
#'         the conversation does not exist. Returns an empty list (`list()`) if
#'         the conversation exists but has no attachments.
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
#' # Add an attachment using the exported function
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
#' reset_history_manager()
get_conversation_attachments <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  # Safe access using %||%
  return(.history_env$conversations[[id]]$attachments %||% list())
}

#' Gets the full conversation data object for the given ID
#'
#' Retrieves the complete data structure (a list) associated with a specific
#' conversation ID, including its history, attachments, settings, etc.
#'
#' @param id Character string. The ID of the conversation.
#' @return List or `NULL`. A list containing all stored data associated with the
#'         conversation specified by `id`. Returns `NULL` if the conversation
#'         does not exist.
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
#' print("Data for non-existent:")
#' print(get_conversation_data("bad_id")) # NULL
#'
#' # Clean up
#' reset_history_manager()
get_conversation_data <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  return(.history_env$conversations[[id]])
}



#' Resets the entire state of the history manager
#'
#' Clears all stored conversations, resets the active conversation ID to `NULL`,
#' and resets the internal conversation counter used for generating IDs.
#' Effectively returns the manager to its initial empty state. Optionally prints a message
#' to the console in interactive sessions.
#'
#' @return Invisible `NULL` (`invisible(NULL)`). Called for its side effect of
#'         clearing the history state.
#' @export
#' @examples
#' # Setup: Initialize and add some data
#' reset_history_manager() # Ensure clean start
#' initialize_history_manager()
#' conv_reset_id <- create_new_conversation(activate = TRUE)
#' add_message_to_active_history("user", "Message before reset")
#' # MODIFIED LINE (was too long)
#' conv_count_before <- length(get_all_conversation_ids()) # Should be 2 initially
#' print(paste("Conversations before reset:", conv_count_before))
#' print(paste("Active ID before reset:", get_active_conversation_id())) # ID of conv_reset_id
#'
#' # Reset the manager. A message might appear in the console if run interactively.
#' reset_history_manager()
#'
#' # Verify state after reset
#' print(paste("Conversations after reset:", length(get_all_conversation_ids()))) # 0
#' print(paste("Active ID after reset:", get_active_conversation_id())) # NULL
#'
#' # Note: After reset, you might need to initialize again if needed for subsequent operations
#' # initialize_history_manager()
reset_history_manager <- function() {
  # Message only in interactive sessions - CRAN compliance
  if (interactive()) {
    message("Resetting history manager...")
  }
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  invisible(NULL)
}

#' Adds an attachment to the active conversation
#'
#' Associates a file's name and content with the currently active conversation.
#' This function prevents adding attachments with duplicate names to the same
#' conversation.
#'
#' @param name Character string. The name of the attachment file (e.g., "script.R").
#' @param content Character string. The content of the file as a single string.
#' @return Logical. `TRUE` if the attachment (consisting of `name` and `content`)
#'         was successfully added to the active conversation's attachment list.
#'         `FALSE` if no conversation is active, the active conversation doesn't exist
#'         anymore, or if an attachment with the same `name` already exists in the
#'         active conversation.
#' @export
#' @examples
#' # Setup
#' reset_history_manager()
#' conv_addattach_id <- create_new_conversation(activate = TRUE)
#'
#' # Add a first attachment
#' result1 <- add_attachment_to_active_conversation("report.txt", "Summary of findings.")
#' print(paste("Added first attachment:", result1)) # TRUE
#' print("Attachments after first add:")
#' print(get_active_conversation_attachments())
#'
#' # Add a second, different attachment
#' result2 <- add_attachment_to_active_conversation("code.R", "x <- function(y) { y + 1 }")
#' print(paste("Added second attachment:", result2)) # TRUE
#' print("Attachments after second add:")
#' print(get_active_conversation_attachments())
#'
#' # Try adding an attachment with the same name (should fail)
#' result3 <- add_attachment_to_active_conversation("report.txt", "Updated summary.")
#' print(paste("Added duplicate name attachment:", result3)) # FALSE
#' print("Attachments after duplicate attempt:")
#' print(get_active_conversation_attachments()) # Should be unchanged
#'
#' # Try adding when no conversation is active
#' set_active_conversation(NULL)
#' result4 <- add_attachment_to_active_conversation("another.txt", "Content")
#' print(paste("Added attachment when none active:", result4)) # FALSE
#'
#' # Clean up
#' reset_history_manager()
add_attachment_to_active_conversation <- function(name, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    warning("Cannot add attachment, no active conversation.")
    return(FALSE)
  }
  # Validate inputs
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    warning("Invalid attachment name: must be a single non-empty character string.")
    return(FALSE)
  }
  if (!is.character(content) || length(content) != 1) { # Allow empty content? Yes.
    warning("Invalid attachment content: must be a single character string.")
    return(FALSE)
  }

  # Re-check existence just before modification
  if (!active_id %in% names(.history_env$conversations)) {
    warning(paste("Active conversation (ID:", active_id, ") does not exist when adding attachment."))
    return(FALSE)
  }

  # Use direct reference for modification
  conv <- .history_env$conversations[[active_id]]
  conv_attachments <- conv$attachments %||% list()

  # Check if a file with this name already exists
  if (any(sapply(conv_attachments, function(att) !is.null(att$name) && att$name == name))) {
    # Use warning instead of message for failed action
    warning("File named '", name, "' already exists in conversation ", active_id, ". Not adding again.", call. = FALSE)
    return(FALSE) # Return FALSE so UI knows it wasn't added
  }

  new_attachment <- list(name = name, content = content)
  # Modify the referenced list
  conv$attachments <- c(conv_attachments, list(new_attachment))
  # Assign back to the environment
  .history_env$conversations[[active_id]] <- conv
  # Verbose message only if option set
  if (.is_verbose()) message(paste("Added attachment", name, "to conversation", active_id))

  return(TRUE)
}

#' Gets the list of attachments for the active conversation
#'
#' Retrieves the list of attachments (files provided as context) associated with
#' the currently active conversation.
#'
#' @return List. A list where each element is a list containing `name` (character)
#'         and `content` (character) for an attachment associated with the currently
#'         active conversation. Returns an empty list (`list()`) if no conversation
#'         is active or if the active conversation has no attachments.
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
#' print(paste("Number of attachments:", length(attachments_list))) # 2
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
  # Safe access using %||%
  return(active_conv$attachments %||% list())
}

# Helper %||%
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x # Using the more robust version
}
