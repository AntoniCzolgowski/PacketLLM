# gadget_rendering_helpers.R

render_chat_history_ui <- function(history, conv_id = NULL, context = NULL) {
  renderUI({
    history_to_display <- if (length(history) > 0 && !is.null(history[[1]]$role) && history[[1]]$role == "system") {
      tail(history, -1)
    } else {
      history
    }

    if (is.null(history_to_display) || length(history_to_display) == 0) {
      return(tags$div(class = "packet-empty-state", "Start the conversation."))
    }

    tagList(lapply(history_to_display, function(msg) {
      render_message_card(msg, conv_id = conv_id, context = context)
    }))
  })
}

render_message_card <- function(msg, conv_id = NULL, context = NULL) {
  content_display <- msg$content %||% "[NO CONTENT]"
  role_display <- msg$role %||% "unknown"

  if (identical(role_display, "system") &&
      grepl("^(Error|Blad|Future execution error|API Error)", content_display, ignore.case = TRUE)) {
    return(tags$div(
      class = "packet-message packet-message-system",
      tags$div(class = "packet-message-label", "System"),
      tags$div(class = "packet-message-body packet-prewrap", content_display)
    ))
  }
  if (identical(role_display, "system")) {
    return(NULL)
  }

  message_class <- if (identical(role_display, "user")) "packet-message-user" else "packet-message-assistant"
  label <- if (identical(role_display, "user")) "You" else "PacketLLM"
  blocks <- if (identical(role_display, "assistant")) split_response_blocks(content_display) else list(list(type = "text", content = content_display))
  action_state <- editor_action_state(context)

  tags$div(
    class = paste("packet-message", message_class),
    tags$div(class = "packet-message-label", label),
    tags$div(
      class = "packet-message-body",
      tagList(lapply(blocks, function(block) {
        if (identical(block$type, "code") && identical(tolower(block$lang %||% ""), "packetllm-change")) {
          render_change_card(block$content, conv_id, action_state)
        } else if (identical(block$type, "code")) {
          render_code_card(block$content, block$lang, conv_id, action_state)
        } else {
          tags$div(class = "packet-text-block packet-prewrap", block$content)
        }
      })),
      if (identical(role_display, "assistant")) {
        tags$div(
          class = "packet-response-actions",
          packet_action_button("Copy", "copy", content_display, conv_id, enabled = TRUE)
        )
      }
    )
  )
}

split_response_blocks <- function(text) {
  lines <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) {
    return(list(list(type = "text", content = "")))
  }

  blocks <- list()
  current <- character(0)
  in_code <- FALSE
  code_lang <- ""
  code_lines <- character(0)

  flush_text <- function() {
    if (length(current) > 0) {
      blocks[[length(blocks) + 1]] <<- list(type = "text", content = paste(current, collapse = "\n"))
      current <<- character(0)
    }
  }

  for (line in lines) {
    if (!in_code && grepl("^```", line)) {
      flush_text()
      in_code <- TRUE
      code_lang <- trimws(sub("^```", "", line))
      code_lines <- character(0)
    } else if (in_code && grepl("^```\\s*$", line)) {
      blocks[[length(blocks) + 1]] <- list(type = "code", lang = code_lang, content = paste(code_lines, collapse = "\n"))
      in_code <- FALSE
      code_lang <- ""
      code_lines <- character(0)
    } else if (in_code) {
      code_lines <- c(code_lines, line)
    } else {
      current <- c(current, line)
    }
  }

  if (in_code) {
    blocks[[length(blocks) + 1]] <- list(type = "code", lang = code_lang, content = paste(code_lines, collapse = "\n"))
  } else {
    flush_text()
  }

  if (length(blocks) == 0) {
    list(list(type = "text", content = text %||% ""))
  } else {
    blocks
  }
}

render_code_card <- function(code, lang = "", conv_id = NULL, action_state = list()) {
  lang_label <- if (nzchar(lang %||% "")) lang else "code"
  tags$div(
    class = "packet-code-card",
    tags$div(
      class = "packet-code-card-head",
      tags$span(class = "packet-code-lang", lang_label),
      tags$div(
        class = "packet-inline-actions",
        packet_action_button("Copy", "copy", code, conv_id, enabled = TRUE),
        packet_action_button("Insert", "insert", code, conv_id, enabled = isTRUE(action_state$can_insert))
      )
    ),
    tags$pre(class = "packet-code-block", tags$code(code))
  )
}

render_change_card <- function(content, conv_id = NULL, action_state = list()) {
  change <- parse_packetllm_change(content)
  if (is.null(change)) {
    return(render_code_card(content, "packetllm-change", conv_id, action_state))
  }

  tags$div(
    class = "packet-change-card",
    tags$div(
      class = "packet-change-head",
      tags$div(
        tags$div(class = "packet-change-kicker", "Change"),
        tags$div(class = "packet-change-file", change$file %||% "Active file")
      ),
      tags$span(class = "packet-change-target", change$target %||% "Captured selection")
    ),
    tags$div(class = "packet-change-section", tags$div(class = "packet-change-label", "Before"), tags$pre(class = "packet-code-block", tags$code(change$before))),
    tags$div(class = "packet-change-section", tags$div(class = "packet-change-label", "After"), tags$pre(class = "packet-code-block", tags$code(change$after))),
    tags$div(
      class = "packet-response-actions",
      packet_action_button("Copy after", "copy", change$after, conv_id, enabled = TRUE),
      packet_action_button("Replace", "replace", change$after, conv_id, enabled = isTRUE(action_state$can_replace))
    )
  )
}

parse_packetllm_change <- function(content) {
  lines <- strsplit(content %||% "", "\n", fixed = TRUE)[[1]]
  file_line <- grep("^File:\\s*", lines, ignore.case = TRUE)
  target_line <- grep("^Target:\\s*", lines, ignore.case = TRUE)
  before_line <- grep("^Before:\\s*$", lines, ignore.case = TRUE)
  after_line <- grep("^After:\\s*$", lines, ignore.case = TRUE)

  if (length(before_line) == 0 || length(after_line) == 0 || before_line[1] >= after_line[1]) {
    return(NULL)
  }

  before <- if ((before_line[1] + 1) <= (after_line[1] - 1)) {
    lines[(before_line[1] + 1):(after_line[1] - 1)]
  } else {
    character(0)
  }
  after <- if ((after_line[1] + 1) <= length(lines)) {
    lines[(after_line[1] + 1):length(lines)]
  } else {
    character(0)
  }
  before <- strip_fence_lines(before)
  after <- strip_fence_lines(after)

  list(
    file = if (length(file_line) > 0) trimws(sub("^File:\\s*", "", lines[file_line[1]], ignore.case = TRUE)) else NULL,
    target = if (length(target_line) > 0) trimws(sub("^Target:\\s*", "", lines[target_line[1]], ignore.case = TRUE)) else NULL,
    before = paste(before, collapse = "\n"),
    after = paste(after, collapse = "\n")
  )
}

strip_fence_lines <- function(lines) {
  if (length(lines) >= 2 && grepl("^```", lines[1]) && grepl("^```\\s*$", lines[length(lines)])) {
    return(lines[2:(length(lines) - 1)])
  }
  lines
}

packet_action_button <- function(label, action, text, conv_id = NULL, enabled = TRUE) {
  attrs <- list(
    type = "button",
    class = paste("packet-action-btn", paste0("packet-action-", action)),
    `data-action` = action,
    `data-conv-id` = conv_id %||% "",
    `data-text` = text %||% ""
  )
  if (!isTRUE(enabled)) {
    attrs$disabled <- "disabled"
    attrs$title <- "Refresh context to enable this action."
  }
  do.call(tags$button, c(attrs, list(label)))
}

render_staged_attachments_list_ui <- function(staged_files) {
  renderUI({
    if (is.null(staged_files) || length(staged_files) == 0 || all(staged_files == "")) {
      tags$span(class = "packet-attachment-empty", "No files")
    } else {
      valid_files <- staged_files[nzchar(staged_files)]
      tagList(lapply(valid_files, function(file_name) {
        tags$span(class = "packet-attachment-pill", title = file_name, file_name)
      }))
    }
  })
}

# Helper %||%
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
