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
  label_class <- if (identical(role_display, "assistant")) {
    "packet-message-label packet-message-label-assistant"
  } else {
    "packet-message-label"
  }
  blocks <- if (identical(role_display, "assistant")) split_response_blocks(content_display) else list(list(type = "text", content = content_display))
  action_state <- editor_action_state(context)

  tags$div(
    class = paste("packet-message", message_class),
    tags$div(class = label_class, label),
    tags$div(
      class = "packet-message-body",
      tagList(lapply(blocks, function(block) {
        if (identical(block$type, "code") && identical(tolower(block$lang %||% ""), "packetllm-change")) {
          render_change_card(block$content, conv_id, action_state)
        } else if (identical(block$type, "code")) {
          render_code_card(block$content, block$lang, conv_id, action_state)
        } else {
          render_markdown_text_block(block$content)
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

render_markdown_text_block <- function(text) {
  shiny::withMathJax(tags$div(class = "packet-text-block packet-markdown", render_markdown_blocks(text)))
}

render_markdown_blocks <- function(text) {
  lines <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]]
  blocks <- list()
  i <- 1

  while (i <= length(lines)) {
    line <- lines[i]
    if (!nzchar(trimws(line))) {
      i <- i + 1
      next
    }

    heading <- regexec("^(#{1,6})\\s+(.+)$", line, perl = TRUE)
    heading_match <- regmatches(line, heading)[[1]]
    if (length(heading_match) == 3) {
      level <- nchar(heading_match[2])
      blocks[[length(blocks) + 1]] <- do.call(tags[[paste0("h", level)]], list(inline_markdown_elements(heading_match[3])))
      i <- i + 1
      next
    }

    if (i < length(lines) && is_table_separator(lines[i + 1]) && grepl("\\|", line, fixed = TRUE)) {
      table_lines <- line
      i <- i + 2
      while (i <= length(lines) && grepl("\\|", lines[i], fixed = TRUE) && nzchar(trimws(lines[i]))) {
        table_lines <- c(table_lines, lines[i])
        i <- i + 1
      }
      blocks[[length(blocks) + 1]] <- render_markdown_table(table_lines)
      next
    }

    if (grepl("^\\s*[-*+]\\s+", line)) {
      items <- character(0)
      while (i <= length(lines) && grepl("^\\s*[-*+]\\s+", lines[i])) {
        items <- c(items, sub("^\\s*[-*+]\\s+", "", lines[i]))
        i <- i + 1
      }
      blocks[[length(blocks) + 1]] <- tags$ul(class = "packet-md-list", lapply(items, function(item) tags$li(inline_markdown_elements(item))))
      next
    }

    if (grepl("^\\s*[0-9]+\\.\\s+", line)) {
      items <- character(0)
      while (i <= length(lines) && grepl("^\\s*[0-9]+\\.\\s+", lines[i])) {
        items <- c(items, sub("^\\s*[0-9]+\\.\\s+", "", lines[i]))
        i <- i + 1
      }
      blocks[[length(blocks) + 1]] <- tags$ol(class = "packet-md-list", lapply(items, function(item) tags$li(inline_markdown_elements(item))))
      next
    }

    if (grepl("^\\s*>\\s?", line)) {
      quote_lines <- character(0)
      while (i <= length(lines) && grepl("^\\s*>\\s?", lines[i])) {
        quote_lines <- c(quote_lines, sub("^\\s*>\\s?", "", lines[i]))
        i <- i + 1
      }
      blocks[[length(blocks) + 1]] <- tags$blockquote(class = "packet-md-quote", inline_markdown_elements(paste(quote_lines, collapse = " ")))
      next
    }

    paragraph <- line
    i <- i + 1
    while (i <= length(lines) && nzchar(trimws(lines[i])) && !is_markdown_block_start(lines, i)) {
      paragraph <- paste(paragraph, lines[i])
      i <- i + 1
    }
    blocks[[length(blocks) + 1]] <- tags$p(class = "packet-md-p", inline_markdown_elements(paragraph))
  }

  if (length(blocks) == 0) {
    return(tags$p(class = "packet-md-p", ""))
  }
  tagList(blocks)
}

is_markdown_block_start <- function(lines, i) {
  if (i > length(lines)) return(FALSE)
  line <- lines[i]
  grepl("^(#{1,6})\\s+(.+)$", line, perl = TRUE) ||
    grepl("^\\s*[-*+]\\s+", line) ||
    grepl("^\\s*[0-9]+\\.\\s+", line) ||
    grepl("^\\s*>\\s?", line) ||
    (i < length(lines) && is_table_separator(lines[i + 1]) && grepl("\\|", line, fixed = TRUE))
}

is_table_separator <- function(line) {
  grepl("^\\s*\\|?\\s*:?-{3,}:?\\s*(\\|\\s*:?-{3,}:?\\s*)+\\|?\\s*$", line)
}

render_markdown_table <- function(table_lines) {
  header <- split_table_row(table_lines[1])
  body <- lapply(table_lines[-1], split_table_row)
  tags$table(
    class = "packet-md-table",
    tags$thead(tags$tr(lapply(header, function(cell) tags$th(inline_markdown_elements(cell))))),
    tags$tbody(lapply(body, function(row) tags$tr(lapply(row, function(cell) tags$td(inline_markdown_elements(cell))))))
  )
}

split_table_row <- function(line) {
  line <- trimws(line)
  line <- sub("^\\|", "", line)
  line <- sub("\\|$", "", line)
  trimws(strsplit(line, "|", fixed = TRUE)[[1]])
}

inline_markdown_elements <- function(text) {
  text <- text %||% ""
  patterns <- list(
    code = "`([^`]+)`",
    link = "\\[([^\\]]+)\\]\\((https?://[^\\s)]+|mailto:[^\\s)]+)\\)",
    bold = "\\*\\*([^*]+)\\*\\*",
    italic = "\\*([^*]+)\\*"
  )
  out <- list()
  rest <- text

  while (nzchar(rest)) {
    matches <- lapply(patterns, function(pattern) regexpr(pattern, rest, perl = TRUE))
    starts <- vapply(matches, function(match) if (match[1] < 0) Inf else as.integer(match[1]), numeric(1))
    if (all(is.infinite(starts))) {
      out[[length(out) + 1]] <- rest
      break
    }

    chosen_name <- names(which.min(starts))
    chosen <- matches[[chosen_name]]
    start <- as.integer(chosen[1])
    len <- attr(chosen, "match.length")
    if (start > 1) {
      out[[length(out) + 1]] <- substr(rest, 1, start - 1)
    }

    match_text <- substr(rest, start, start + len - 1)
    groups <- regmatches(match_text, regexec(patterns[[chosen_name]], match_text, perl = TRUE))[[1]]
    out[[length(out) + 1]] <- switch(
      chosen_name,
      code = tags$code(class = "packet-inline-code", groups[2]),
      link = tags$a(href = groups[3], target = "_blank", rel = "noopener noreferrer", groups[2]),
      bold = tags$strong(inline_markdown_elements(groups[2])),
      italic = tags$em(inline_markdown_elements(groups[2])),
      match_text
    )
    rest <- substr(rest, start + len, nchar(rest))
  }

  tagList(out)
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
        packet_action_button("Insert", "insert", code, conv_id, enabled = isTRUE(action_state$can_insert)),
        packet_action_button("Replace", "replace", code, conv_id, enabled = isTRUE(action_state$can_replace))
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
