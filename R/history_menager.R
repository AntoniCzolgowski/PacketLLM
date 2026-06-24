# In-memory state
.history_env <- new.env(parent = emptyenv())
.history_env$conversations <- list()
.history_env$active_conversation_id <- NULL
.history_env$conversation_counter <- 0
.history_env$persistence_enabled <- FALSE

# Verbose flag
.is_verbose <- function() getOption("PacketLLM.verbose", default = FALSE)

#' Generate unique conversation ID
#' @noRd
generate_conversation_id <- function() {
  .history_env$conversation_counter <- .history_env$conversation_counter + 1
  paste0("conv_", .history_env$conversation_counter, "_", as.integer(Sys.time()))
}

#' Initialize the history manager
#'
#' Initializes state and creates or restores a conversation, then activates it.
#' @param persist Logical. Restore and save local gadget history?
#' @return Character: ID of the created conversation.
#' @export
initialize_history_manager <- function(persist = FALSE) {
  if (interactive()) message("Initializing history manager...")
  .history_env$persistence_enabled <- isTRUE(persist)

  if (isTRUE(persist)) {
    if (length(.history_env$conversations) > 0) {
      active_id <- get_active_conversation_id() %||% names(.history_env$conversations)[1]
      set_active_conversation(active_id)
      return(active_id)
    }

    restored_id <- load_history_manager()
    if (!is.null(restored_id)) {
      return(restored_id)
    }
  }

  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  first_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE, title = "New Conversation")
  if (.is_verbose()) message(paste("History manager initialized. Created:", first_id))
  set_active_conversation(first_id)
  if (.is_verbose()) message(paste("Set active conversation to:", first_id))
  first_id
}

history_storage_path <- function() {
  override <- getOption("PacketLLM.history_path", default = NULL)
  if (!is.null(override) && nzchar(override)) {
    override_dir <- dirname(override)
    if (!dir.exists(override_dir)) {
      dir.create(override_dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(override)
  }

  data_dir <- tools::R_user_dir("PacketLLM", "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(data_dir, "conversation-history.rds")
}

save_history_manager <- function() {
  if (!isTRUE(.history_env$persistence_enabled)) {
    return(invisible(FALSE))
  }

  state <- list(
    conversations = .history_env$conversations,
    active_conversation_id = .history_env$active_conversation_id,
    conversation_counter = .history_env$conversation_counter
  )

  tryCatch({
    saveRDS(state, history_storage_path())
    invisible(TRUE)
  }, error = function(e) {
    warning("Could not save PacketLLM history: ", e$message, call. = FALSE)
    invisible(FALSE)
  })
}

load_history_manager <- function() {
  path <- history_storage_path()
  if (!file.exists(path)) {
    return(NULL)
  }

  state <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(state) || !is.list(state$conversations) || length(state$conversations) == 0) {
    return(NULL)
  }

  .history_env$conversations <- state$conversations
  .history_env$conversation_counter <- state$conversation_counter %||% length(state$conversations)
  active_id <- state$active_conversation_id
  if (is.null(active_id) || !active_id %in% names(.history_env$conversations)) {
    active_id <- names(.history_env$conversations)[1]
  }
  .history_env$active_conversation_id <- active_id
  active_id
}

save_history_if_enabled <- function() {
  if (isTRUE(.history_env$persistence_enabled)) {
    save_history_manager()
  }
  invisible(NULL)
}

#' Create a new conversation
#'
#' @param activate Logical. Activate immediately?
#' @param add_initial_settings Logical. Add default model and system message?
#' @param title Optional title; if NULL, a time-based title is used.
#' @return Character: conversation ID.
#' @export
create_new_conversation <- function(activate = FALSE, add_initial_settings = TRUE, title = NULL) {
  conv_id <- generate_conversation_id()
  if (is.null(title)) title <- paste("Conversation", format(Sys.time(), "%H:%M:%S"))
  if (.is_verbose()) message(paste("Creating conversation:", conv_id, "title:", title))

  defaults <- default_model_settings()
  default_model <- defaults$model

  default_system_message <- ""

  new_conv <- list(
    id = conv_id,
    title = title,
    history = list(),
    attachments = list(),
    created_at = Sys.time(),
    system_message = "",
    model = default_model,
    model_locked = FALSE,
    reasoning_effort = defaults$reasoning_effort,
    verbosity = defaults$verbosity,
    max_output_tokens = defaults$max_output_tokens,
    assistant_behavior = defaults$assistant_behavior,
    custom_instruction = defaults$custom_instruction,
    context_mode = defaults$context_mode
  )

  if (add_initial_settings) {
    new_conv$system_message <- default_system_message
    if (.is_verbose()) message(paste("Added defaults (SysMsg) and model (", default_model, ") to", conv_id))
  } else {
    new_conv$system_message <- ""
    if (.is_verbose()) message(paste("Set default model (", default_model, ") for", conv_id, "(no initial settings)"))
  }

  .history_env$conversations[[conv_id]] <- new_conv
  if (activate) set_active_conversation(conv_id)
  save_history_if_enabled()
  conv_id
}

#' Has the conversation started (model locked)?
#' @param id Conversation ID.
#' @return Logical.
#' @export
is_conversation_started <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(FALSE)
  .history_env$conversations[[id]]$model_locked %||% FALSE
}

#' Add a message to the active conversation
#'
#' Locks the model on the first assistant message. Sets title on the first user message.
#' @param role 'user'|'assistant'|'system'
#' @param content Message content
#' @return Result list (type + extra fields) or error list.
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
  if (!is.character(content) || length(content) != 1) {
    warning("Invalid message content: must be a single character string.")
    return(list(type = "error", message = "Invalid message content."))
  }
  if (!active_id %in% names(.history_env$conversations)) {
    warning("Active conversation (ID: ", active_id, ") does not exist when adding message.")
    return(list(type = "error", message = "Active conversation does not exist."))
  }

  conv <- .history_env$conversations[[active_id]]

  return_value <- list(type = "message_added")
  new_message <- list(role = role, content = content)
  should_lock_model <- role == "assistant" && !(conv$model_locked %||% FALSE)

  conv$history <- c(conv$history %||% list(), list(new_message))
  if (.is_verbose()) message(paste("Added", role, "message to", active_id))

  if (should_lock_model) {
    conv$model_locked <- TRUE
    if (.is_verbose()) message(paste("Model for", active_id, "has been locked."))
    if (return_value$type == "message_added") return_value <- list(type = "assistant_locked_model")
  }

  conv_history_updated <- conv$history %||% list()
  user_message_count <- sum(vapply(conv_history_updated, function(m) !is.null(m$role) && m$role == "user", logical(1)))
  is_first_user_message_ever <- role == "user" && user_message_count == 1

  if (is_first_user_message_ever && nzchar(trimws(content))) {
    title_content_base <- content
    attachment_marker <- "\n\n<strong>Attached:</strong>"
    attachment_marker_start <- "<strong>Attached:</strong>"

    if (grepl(attachment_marker, title_content_base, fixed = TRUE)) {
      title_content_processed <- trimws(strsplit(title_content_base, attachment_marker, fixed = TRUE)[[1]][1])
    } else if (startsWith(title_content_base, attachment_marker_start)) {
      title_content_processed <- gsub("<strong>|</strong>", "", title_content_base)
    } else {
      title_content_processed <- title_content_base
    }

    words <- strsplit(trimws(title_content_processed), "\\s+")[[1]]
    words <- words[nzchar(words)]
    max_words <- 5
    max_chars <- 40
    new_title_base <- paste(head(words, max_words), collapse = " ")
    new_title <- if (length(words) > max_words) paste0(new_title_base, "...") else new_title_base
    if (nchar(new_title) > max_chars) new_title <- paste0(substr(new_title, 1, max_chars - 3), "...")

    if (!nzchar(trimws(new_title)) || new_title == "...") new_title <- paste("Chat", format(Sys.time(), "%M%S"))

    conv$title <- new_title
    if (.is_verbose()) message(paste("Set title for", active_id, "to:", new_title))
    return_value <- list(type = "title_set", new_title = new_title)
  }

  .history_env$conversations[[active_id]] <- conv
  save_history_if_enabled()
  return(return_value)
}

#' Delete a conversation
#' @param id Conversation ID.
#' @return TRUE if deleted, FALSE otherwise.
#' @export
delete_conversation <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(FALSE)
  if (.is_verbose()) message(paste("Deleting conversation:", id))
  .history_env$conversations[[id]] <- NULL

  current_active_id <- .history_env$active_conversation_id
  if (!is.null(current_active_id) && current_active_id == id) {
    if (.is_verbose()) message("Deleted the active conversation. Resetting active_conversation_id.")
    .history_env$active_conversation_id <- NULL
  }
  save_history_if_enabled()
  TRUE
}

#' Set the active conversation
#' @param id Conversation ID or NULL.
#' @return Invisible NULL.
#' @export
set_active_conversation <- function(id) {
  if (is.null(id)) {
    if (!is.null(.history_env$active_conversation_id) && .is_verbose()) message("Deactivated the active conversation.")
    .history_env$active_conversation_id <- NULL
    return(invisible(NULL))
  }
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set a non-existent conversation as active: ", id)
    return(invisible(NULL))
  }
  if (is.null(.history_env$active_conversation_id) || .history_env$active_conversation_id != id) {
    if (.is_verbose()) message(paste("Set active conversation to ID:", id))
    .history_env$active_conversation_id <- id
    save_history_if_enabled()
  }
  invisible(NULL)
}

#' Get active conversation ID
#' @return Character or NULL.
#' @export
get_active_conversation_id <- function() {
  active_id <- .history_env$active_conversation_id
  if (!is.null(active_id) && !active_id %in% names(.history_env$conversations)) {
    warning(paste("Active ID", active_id, "points to a non-existent conversation. Resetting to NULL."))
    .history_env$active_conversation_id <- NULL
    return(NULL)
  }
  active_id
}

#' Get active conversation object
#' @return List or NULL.
#' @export
get_active_conversation <- function() {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) return(NULL)
  .history_env$conversations[[active_id]]
}

#' Get active chat history
#' @return List of messages (possibly empty).
#' @export
get_active_chat_history <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) return(list())
  active_conv$history %||% list()
}

#' Get conversation title by ID
#' @param id Conversation ID.
#' @return Character or NULL.
#' @export
get_conversation_title <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  .history_env$conversations[[id]]$title %||% paste("[Untitled Conversation - ID:", id, "]")
}

#' Get all conversation IDs
#' @return Character vector.
#' @export
get_all_conversation_ids <- function() names(.history_env$conversations)

#' Get model for conversation
#' @param id Conversation ID.
#' @return Character or NULL.
#' @export
get_conversation_model <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  .history_env$conversations[[id]]$model %||% default_model_settings()$model
}

#' Set model for conversation (if not started)
#' @param id Conversation ID.
#' @param model_name Model name (must be in available_openai_models).
#' @return Logical.
#' @export
set_conversation_model <- function(id, model_name) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set model for non-existent conversation: ", id)
    return(FALSE)
  }
  if (is_conversation_started(id)) {
    warning("Cannot change model - conversation has already started for ID: ", id, call. = FALSE)
    return(FALSE)
  }
  if (!is.character(model_name) || length(model_name) != 1) {
    warning("Invalid model_name: must be a single character string.")
    return(FALSE)
  }

  available_models_pkg <- character(0)
  if (exists("available_openai_models", where = "package:PacketLLM", inherits = FALSE)) {
    available_models_pkg <- get("available_openai_models", envir = asNamespace("PacketLLM"))
  } else {
    warning("Could not retrieve available_openai_models from PacketLLM namespace.")
    available_models_pkg <- default_model_settings()$model
  }

  if (!model_name %in% available_models_pkg) {
    warning(paste("Attempting to set unavailable model:", model_name, "for conversation", id,
                  ". Available models are:", paste(available_models_pkg, collapse = ", ")))
    return(FALSE)
  }

  .history_env$conversations[[id]]$model <- model_name
  if (.is_verbose()) message(paste("Set model for", id, "to:", model_name))
  save_history_if_enabled()
  TRUE
}

#' Set system message for conversation
#' @param id Conversation ID.
#' @param message Single string system message.
#' @return Logical.
#' @export
set_conversation_system_message <- function(id, message) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set system message for non-existent conversation: ", id)
    return(FALSE)
  }
  if (!is.character(message) || length(message) != 1 || is.na(message)) {
    warning("Invalid system message format. Must be a single, non-NA character string.")
    return(FALSE)
  }
  .history_env$conversations[[id]]$system_message <- message
  save_history_if_enabled()
  TRUE
}

set_conversation_generation_settings <- function(id,
                                                 reasoning_effort = NULL,
                                                 verbosity = NULL,
                                                 max_output_tokens = NULL,
                                                 assistant_behavior = NULL,
                                                 custom_instruction = NULL,
                                                 context_mode = NULL) {
  if (!id %in% names(.history_env$conversations)) {
    warning("Attempting to set settings for non-existent conversation: ", id)
    return(FALSE)
  }

  conv <- .history_env$conversations[[id]]
  if (!is.null(reasoning_effort)) {
    if (!reasoning_effort %in% valid_reasoning_efforts()) return(FALSE)
    conv$reasoning_effort <- reasoning_effort
  }
  if (!is.null(verbosity)) {
    if (!verbosity %in% valid_verbosity_levels()) return(FALSE)
    conv$verbosity <- verbosity
  }
  if (!is.null(max_output_tokens)) {
    max_output_tokens <- suppressWarnings(as.integer(max_output_tokens))
    conv$max_output_tokens <- if (is.na(max_output_tokens) || max_output_tokens <= 0) NA_integer_ else max_output_tokens
  }
  if (!is.null(assistant_behavior)) {
    if (!assistant_behavior %in% assistant_behavior_choices()) return(FALSE)
    conv$assistant_behavior <- assistant_behavior
  }
  if (!is.null(custom_instruction)) {
    if (!is.character(custom_instruction) || length(custom_instruction) != 1 || is.na(custom_instruction)) return(FALSE)
    conv$custom_instruction <- custom_instruction
  }
  if (!is.null(context_mode)) {
    conv$context_mode <- normalize_context_mode(context_mode)
  }

  .history_env$conversations[[id]] <- conv
  save_history_if_enabled()
  TRUE
}

#' Get conversation history by ID
#' @param id Conversation ID.
#' @return List or NULL.
#' @export
get_conversation_history <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  .history_env$conversations[[id]]$history %||% list()
}

#' Get attachments by ID
#' @param id Conversation ID.
#' @return List or NULL.
#' @export
get_conversation_attachments <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  .history_env$conversations[[id]]$attachments %||% list()
}

#' Get conversation data by ID
#' @param id Conversation ID.
#' @return List or NULL.
#' @export
get_conversation_data <- function(id) {
  if (!id %in% names(.history_env$conversations)) return(NULL)
  .history_env$conversations[[id]]
}

#' Reset the history manager
#' @param clear_persistent Logical. Delete saved local gadget history too?
#' @return Invisible NULL.
#' @export
reset_history_manager <- function(clear_persistent = FALSE) {
  if (interactive()) message("Resetting history manager...")
  .history_env$conversations <- list()
  .history_env$active_conversation_id <- NULL
  .history_env$conversation_counter <- 0
  .history_env$persistence_enabled <- FALSE
  if (isTRUE(clear_persistent)) {
    path <- history_storage_path()
    if (file.exists(path)) unlink(path)
  }
  invisible(NULL)
}

#' Add attachment to active conversation
#' @param name File name.
#' @param content File content as string.
#' @return Logical.
#' @export
add_attachment_to_active_conversation <- function(name, content) {
  active_id <- get_active_conversation_id()
  if (is.null(active_id)) {
    warning("Cannot add attachment, no active conversation.")
    return(FALSE)
  }
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    warning("Invalid attachment name: must be a single non-empty character string.")
    return(FALSE)
  }
  if (!is.character(content) || length(content) != 1) {
    warning("Invalid attachment content: must be a single character string.")
    return(FALSE)
  }
  if (!active_id %in% names(.history_env$conversations)) {
    warning(paste("Active conversation (ID:", active_id, ") does not exist when adding attachment."))
    return(FALSE)
  }

  conv <- .history_env$conversations[[active_id]]
  conv_attachments <- conv$attachments %||% list()

  if (any(vapply(conv_attachments, function(att) !is.null(att$name) && att$name == name, logical(1)))) {
    warning("File named '", name, "' already exists in conversation ", active_id, ". Not adding again.", call. = FALSE)
    return(FALSE)
  }

  new_attachment <- list(name = name, content = content)
  conv$attachments <- c(conv_attachments, list(new_attachment))
  .history_env$conversations[[active_id]] <- conv
  if (.is_verbose()) message(paste("Added attachment", name, "to conversation", active_id))
  save_history_if_enabled()
  TRUE
}

#' Get attachments for active conversation
#' @return List (possibly empty).
#' @export
get_active_conversation_attachments <- function() {
  active_conv <- get_active_conversation()
  if (is.null(active_conv)) return(list())
  active_conv$attachments %||% list()
}

# Helper %||%
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
