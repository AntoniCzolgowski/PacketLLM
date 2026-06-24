#' Run the PacketLLM gadget
#'
#' Launches PacketLLM as an RStudio/Shiny gadget. The gadget can use RStudio
#' editor context when available, while still running outside RStudio with
#' reduced functionality.
#'
#' @export
#' @return Value passed to shiny::stopApp() (typically NULL).
#' @import shiny
#' @import future promises httr pdftools readtext tools
#' @importFrom shinyjs useShinyjs runjs enable disable
#' @importFrom utils tail head
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' run_llm_chat_app()
#' }
run_llm_chat_app <- function() {
  ui <- fluidPage(
    title = "PacketLLM",
    useShinyjs(),
    tags$head(
      packetllm_client_script(),
      packetllm_styles()
    ),
    tags$div(
      class = "packet-shell",
      tags$header(
        class = "packet-topbar",
        tags$div(
          tags$div(class = "packet-brand", "PacketLLM"),
          tags$div(class = "packet-subtitle", "AI assistant for RStudio")
        ),
        tags$div(
          class = "packet-topbar-actions",
          actionButton("advanced_settings_btn", "Settings", class = "packet-topbar-btn"),
          actionButton("new_chat_btn", "New chat", class = "packet-topbar-btn packet-topbar-primary"),
          actionButton("close_app_btn", "Close", class = "packet-topbar-btn")
        )
      ),
      tags$main(
        class = "packet-main",
        tabsetPanel(id = "chatTabs", type = "tabs")
      )
    ),
    tags$div(style = "display: none;", fileInput("hidden_file_input", NULL, multiple = TRUE))
  )

  server <- function(input, output, session) {
    initial_conv_id <- initialize_history_manager(persist = TRUE)
    active_conv_id_rv <- reactiveVal(NULL)
    open_tab_ids_rv <- reactiveVal(character(0))
    first_tab_created <- reactiveVal(FALSE)
    processing_state <- reactiveValues()
    file_upload_context_rv <- reactiveVal(NULL)
    staged_attachments_rv <- reactiveVal(list())
    context_state_rv <- reactiveVal(list())
    pending_replace_rv <- reactiveVal(NULL)

    get_context_for_tab <- function(conv_id) {
      context_state_rv()[[conv_id]]
    }

    set_context_for_tab <- function(conv_id, context) {
      current <- context_state_rv()
      current[[conv_id]] <- context
      context_state_rv(current)
    }

    render_context_status <- function(conv_id) {
      ns <- NS(conv_id)
      output[[ns("context_status_output")]] <- renderUI({
        context <- get_context_for_tab(conv_id)
        label <- if (is.null(context)) "Context not loaded" else context$label
        tags$span(
          class = if (!is.null(context) && isTRUE(context$available)) "packet-context-ok" else "packet-context-muted",
          label
        )
      })
    }

    render_chat_for_tab <- function(conv_id) {
      ns <- NS(conv_id)
      output[[ns("chat_history_output")]] <- render_chat_history_ui(
        get_conversation_history(conv_id) %||% list(),
        conv_id = conv_id,
        context = get_context_for_tab(conv_id)
      )
    }

    capture_context_for_tab <- function(conv_id) {
      ns <- NS(conv_id)
      mode <- isolate(input[[ns("context_mode")]]) %||% get_conversation_data(conv_id)$context_mode %||% "auto"
      context <- capture_rstudio_context(mode = mode)
      set_context_for_tab(conv_id, context)
      set_conversation_generation_settings(conv_id, context_mode = mode)
      render_context_status(conv_id)
      render_chat_for_tab(conv_id)
      invisible(context)
    }

    update_send_button_state <- function(conv_id) {
      if (!conv_id %in% open_tab_ids_rv()) return()
      selector <- paste0("#", NS(conv_id)("send_query_btn"))
      if (can_send_message(conv_id)) shinyjs::enable(selector = selector) else shinyjs::disable(selector = selector)
    }

    can_send_message <- function(conv_id) {
      if (is.null(conv_id) || !conv_id %in% open_tab_ids_rv()) return(FALSE)
      if (isTRUE(processing_state[[conv_id]])) return(FALSE)
      input_id <- NS(conv_id)("user_message_input")
      input_val <- isolate(input[[input_id]])
      has_text <- nzchar(trimws(input_val %||% ""))
      has_staged_files <- length(isolate(staged_attachments_rv()[[conv_id]]) %||% list()) > 0
      has_text || has_staged_files
    }

    create_and_append_new_tab <- function(conv_id, select_tab = TRUE) {
      conv_title <- get_conversation_title(conv_id) %||% "Conversation"
      processing_state[[conv_id]] <- FALSE
      current_staged <- staged_attachments_rv()
      current_staged[[conv_id]] <- list()
      staged_attachments_rv(current_staged)

      insertTab(
        inputId = "chatTabs",
        tabPanel(
          title = span(
            conv_title,
            actionButton(
              paste0("close_tab_", conv_id),
              "x",
              class = "packet-close-tab",
              onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('close_tab_request', '%s', {priority: 'event'})", conv_id)
            )
          ),
          value = conv_id,
          create_tab_content_ui(conv_id)
        ),
        select = select_tab
      )
      open_tab_ids_rv(c(open_tab_ids_rv(), conv_id))

      ns <- NS(conv_id)
      output[[ns("staged_files_list_output")]] <- render_staged_attachments_list_ui(list())
      render_context_status(conv_id)
      render_chat_for_tab(conv_id)
      capture_context_for_tab(conv_id)
      update_send_button_state(conv_id)

      observeEvent(input[[ns("user_message_input")]], {
        update_send_button_state(conv_id)
      }, ignoreNULL = FALSE, ignoreInit = TRUE, autoDestroy = TRUE)

      observeEvent(input[[ns("context_mode")]], {
        capture_context_for_tab(conv_id)
      }, ignoreNULL = FALSE, ignoreInit = TRUE, autoDestroy = TRUE)

      observeEvent(input[[ns("refresh_context_btn")]], {
        capture_context_for_tab(conv_id)
        showNotification("Context refreshed.", type = "message", duration = 2)
      }, ignoreInit = TRUE, autoDestroy = TRUE)
    }

    observeEvent(TRUE, {
      req(!first_tab_created())
      conversation_ids <- get_all_conversation_ids()
      if (length(conversation_ids) == 0) {
        conversation_ids <- initialize_history_manager(persist = TRUE)
      }
      for (conv_id in conversation_ids) {
        create_and_append_new_tab(conv_id, select_tab = identical(conv_id, initial_conv_id))
      }
      first_tab_created(TRUE)
      set_active_conversation(initial_conv_id)
      active_conv_id_rv(initial_conv_id)
    }, once = TRUE, ignoreInit = FALSE)

    observeEvent(input$chatTabs, {
      req(first_tab_created(), input$chatTabs)
      current_tab_id <- input$chatTabs
      if (current_tab_id %in% open_tab_ids_rv()) {
        set_active_conversation(current_tab_id)
        active_conv_id_rv(current_tab_id)
        render_chat_for_tab(current_tab_id)
        update_send_button_state(current_tab_id)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$new_chat_btn, {
      new_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE)
      create_and_append_new_tab(new_id, select_tab = TRUE)
      showNotification("Started a new chat.", type = "message", duration = 2)
    })

    observeEvent(input$dynamic_send_click, {
      req(input$dynamic_send_click)
      conv_id <- input$dynamic_send_click$convId
      req(conv_id %in% open_tab_ids_rv())
      if (!can_send_message(conv_id)) return()

      processing_state[[conv_id]] <- TRUE
      update_send_button_state(conv_id)

      input_id <- NS(conv_id)("user_message_input")
      user_text <- trimws(isolate(input[[input_id]]) %||% "")
      staged_files_to_send <- isolate(staged_attachments_rv()[[conv_id]]) %||% list()
      final_content <- build_user_message_content(user_text, staged_files_to_send)

      original_active <- get_active_conversation_id()
      set_active_conversation(conv_id)
      add_result <- add_message_to_active_history(role = "user", content = final_content)
      if (is.list(add_result) && identical(add_result$type, "error")) {
        processing_state[[conv_id]] <- FALSE
        update_send_button_state(conv_id)
        set_active_conversation(original_active)
        showNotification(add_result$message, type = "error", duration = 5)
        return()
      }

      updateTextAreaInput(session, input_id, value = "")
      current_staged <- staged_attachments_rv()
      current_staged[[conv_id]] <- list()
      staged_attachments_rv(current_staged)
      output[[NS(conv_id)("staged_files_list_output")]] <- render_staged_attachments_list_ui(list())
      render_chat_for_tab(conv_id)
      update_tab_title_if_needed(conv_id, add_result)

      request_snapshot <- get_conversation_data(conv_id)
      request_context <- get_context_for_tab(conv_id)
      notification_id <- paste0("api_call_indicator_", conv_id)
      showNotification("Thinking...", id = notification_id, duration = NULL, type = "message")

      future({
        prepared <- prepare_api_messages(
          conversation_history = request_snapshot$history %||% list(),
          attachments = request_snapshot$attachments %||% list(),
          conversation_system_message = request_snapshot$system_message %||% "",
          conversation_model = request_snapshot$model,
          context = request_context,
          assistant_behavior = request_snapshot$assistant_behavior %||% "default",
          custom_instruction = request_snapshot$custom_instruction %||% "",
          verbose = getOption("PacketLLM.verbose", default = FALSE)
        )
        response_text <- call_openai_chat(
          prepared$messages,
          model = request_snapshot$model,
          reasoning_effort = request_snapshot$reasoning_effort %||% "medium",
          verbosity = request_snapshot$verbosity %||% "low",
          max_output_tokens = request_snapshot$max_output_tokens %||% NA_integer_
        )
        list(response = response_text, conv_id = conv_id)
      }) %...>% (function(result) {
        response_conv_id <- result$conv_id
        removeNotification(paste0("api_call_indicator_", response_conv_id))
        processing_state[[response_conv_id]] <- FALSE
        update_send_button_state(response_conv_id)

        if (!response_conv_id %in% get_all_conversation_ids()) return()
        set_active_conversation(response_conv_id)
        response_text <- result$response %||% "No response text was returned."
        add_message_to_active_history(role = "assistant", content = response_text)
        render_chat_for_tab(response_conv_id)
        scroll_active_chat()
        showNotification("Response received.", type = "message", duration = 2)
      }) %...!% (function(error) {
        removeNotification(notification_id)
        processing_state[[conv_id]] <- FALSE
        update_send_button_state(conv_id)
        set_active_conversation(conv_id)
        add_message_to_active_history(role = "system", content = paste("API Error:", error$message))
        render_chat_for_tab(conv_id)
        showNotification(paste("API error:", error$message), type = "error", duration = 8)
      })

      set_active_conversation(original_active)
      NULL
    })

    observeEvent(input$dynamic_add_file_click, {
      req(input$dynamic_add_file_click)
      conv_id <- input$dynamic_add_file_click$convId
      req(conv_id %in% open_tab_ids_rv())
      if (isTRUE(processing_state[[conv_id]])) {
        showNotification("Please wait for the current response.", type = "warning", duration = 3)
        return()
      }
      file_upload_context_rv(conv_id)
    })

    observeEvent(input$hidden_file_input, {
      conv_id <- isolate(file_upload_context_rv())
      file_upload_context_rv(NULL)
      req(conv_id, conv_id %in% open_tab_ids_rv(), input$hidden_file_input)

      original_active <- get_active_conversation_id()
      set_active_conversation(conv_id)
      files_info <- input$hidden_file_input
      added <- character(0)
      failed <- 0

      for (i in seq_len(nrow(files_info))) {
        file_info <- files_info[i, ]
        file_content <- tryCatch(read_file_content(file_info$datapath), error = function(e) {
          warning("Error reading ", file_info$name, ": ", e$message)
          NULL
        })
        if (!is.null(file_content) && add_attachment_to_active_conversation(file_info$name, file_content)) {
          added <- c(added, file_info$name)
        } else {
          failed <- failed + 1
        }
      }

      if (length(added) > 0) {
        current_staged <- staged_attachments_rv()
        current_staged[[conv_id]] <- unique(c(current_staged[[conv_id]] %||% list(), added))
        staged_attachments_rv(current_staged)
        output[[NS(conv_id)("staged_files_list_output")]] <- render_staged_attachments_list_ui(current_staged[[conv_id]])
        update_send_button_state(conv_id)
        showNotification(paste("Attached", length(added), "file(s)."), type = "message", duration = 2)
      }
      if (failed > 0) {
        showNotification(paste("Could not attach", failed, "file(s)."), type = "warning", duration = 4)
      }
      set_active_conversation(original_active)
      session$sendCustomMessage(type = "resetFileInput", message = list(id = "hidden_file_input"))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$packet_response_action, {
      action <- input$packet_response_action
      conv_id <- action$convId
      text <- action$text %||% ""
      req(conv_id, conv_id %in% open_tab_ids_rv(), nzchar(text))

      context <- get_context_for_tab(conv_id)
      if (identical(action$action, "replace")) {
        validation <- validate_replacement_target(context)
        if (!isTRUE(validation$ok)) {
          showNotification(validation$message, type = "warning", duration = 4)
          return()
        }
        pending_replace_rv(list(conv_id = conv_id, text = text, context = context))
        showModal(replace_preview_modal(text, context))
        return()
      }

      result <- switch(
        action$action,
        insert = insert_text_into_rstudio(text, context),
        list(ok = FALSE, message = "Unsupported action.")
      )

      showNotification(result$message, type = if (isTRUE(result$ok)) "message" else "warning", duration = 4)
      if (isTRUE(result$ok)) {
        capture_context_for_tab(conv_id)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$confirm_replace_selection, {
      pending <- pending_replace_rv()
      req(pending)

      result <- replace_selection_in_rstudio(pending$text, pending$context)
      removeModal()
      pending_replace_rv(NULL)
      showNotification(result$message, type = if (isTRUE(result$ok)) "message" else "warning", duration = 4)
      if (isTRUE(result$ok) && pending$conv_id %in% open_tab_ids_rv()) {
        capture_context_for_tab(pending$conv_id)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$close_tab_request, {
      tab_id <- input$close_tab_request
      if (!tab_id %in% open_tab_ids_rv()) return()
      open_tabs <- open_tab_ids_rv()
      if (length(open_tabs) <= 1) {
        showNotification("Cannot close the last chat.", type = "warning", duration = 3)
        return()
      }
      processing_state[[tab_id]] <- NULL
      open_tab_ids_rv(setdiff(open_tabs, tab_id))
      removeTab(inputId = "chatTabs", target = tab_id)
      delete_conversation(tab_id)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$advanced_settings_btn, {
      active_id <- active_conv_id_rv()
      req(active_id, active_id %in% get_all_conversation_ids())
      current_conv <- get_conversation_data(active_id)
      model_is_locked <- is_conversation_started(active_id)

      showModal(modalDialog(
        title = paste("Settings:", current_conv$title %||% "Conversation"),
        create_advanced_settings_modal_ui(
          model_value = current_conv$model %||% default_model_settings()$model,
          behavior_value = current_conv$assistant_behavior %||% "default",
          custom_instruction = current_conv$custom_instruction %||% "",
          reasoning_value = current_conv$reasoning_effort %||% "medium",
          verbosity_value = current_conv$verbosity %||% "low",
          max_output_value = current_conv$max_output_tokens %||% NA_integer_
        ),
        footer = tagList(modalButton("Cancel"), actionButton("save_advanced_settings", "Save")),
        easyClose = TRUE,
        size = "m"
      ))

      observe({
        if (model_is_locked) {
          shinyjs::disable("modal_model")
          shinyjs::runjs("$('#model_locked_message').show();")
        }
      })
    })

    observeEvent(input$save_advanced_settings, {
      active_id <- active_conv_id_rv()
      req(active_id, active_id %in% get_all_conversation_ids())
      model_is_locked <- is_conversation_started(active_id)

      if (!model_is_locked) {
        selected_model <- isolate(input$modal_model)
        if (!selected_model %in% available_openai_models || !set_conversation_model(active_id, selected_model)) {
          showNotification("Could not save the selected model.", type = "error", duration = 4)
          return()
        }
      }

      ok <- set_conversation_generation_settings(
        active_id,
        reasoning_effort = isolate(input$modal_reasoning_effort),
        verbosity = isolate(input$modal_verbosity),
        max_output_tokens = isolate(input$modal_max_output_tokens),
        assistant_behavior = isolate(input$modal_assistant_behavior),
        custom_instruction = isolate(input$modal_custom_instruction)
      )
      if (!ok) {
        showNotification("Could not save settings.", type = "error", duration = 4)
        return()
      }
      removeModal()
      render_chat_for_tab(active_id)
      showNotification("Settings saved.", type = "message", duration = 2)
    })

    observeEvent(input$close_app_btn, {
      save_history_manager()
      shiny::stopApp()
    })

    session$onSessionEnded(function() {
      save_history_manager()
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 600))
}

#' Launch PacketLLM from the RStudio Addins menu
#'
#' @return Value passed to `run_llm_chat_app()`.
#' @export
packetllm_addin <- function() {
  run_llm_chat_app()
}

build_user_message_content <- function(user_text, staged_files) {
  if (length(staged_files) == 0) {
    return(user_text)
  }
  attachment_list <- paste0("- ", staged_files, collapse = "\n")
  if (nzchar(user_text)) {
    paste0(user_text, "\n\nAttached files:\n", attachment_list)
  } else {
    paste0("Attached files:\n", attachment_list)
  }
}

update_tab_title_if_needed <- function(conv_id, add_result) {
  if (!is.list(add_result) || !identical(add_result$type, "title_set")) {
    return(invisible(NULL))
  }
  title <- add_result$new_title %||% "Conversation"
  escaped_title <- gsub("'", "\\\\'", gsub("\"", "\\\\\"", title))
  shinyjs::runjs(sprintf(
    "$('#chatTabs a[data-value=\"%s\"] > span').contents().filter(function(){ return this.nodeType == 3; }).first().replaceWith('%s');",
    conv_id,
    escaped_title
  ))
  invisible(NULL)
}

scroll_active_chat <- function() {
  shinyjs::runjs("
    var activePane = $('#chatTabs .tab-pane.active');
    var el = activePane.find('.packet-chat-history');
    if (el.length > 0) {
      setTimeout(function() { el.scrollTop(el[0].scrollHeight); }, 80);
    }
  ")
}

replace_preview_modal <- function(replacement_text, context) {
  target_label <- if (!is.null(context$path) && nzchar(context$path)) {
    basename(context$path)
  } else {
    "active editor"
  }

  modalDialog(
    title = "Preview replacement",
    tags$div(
      class = "packet-replace-preview",
      tags$div(
        class = "packet-replace-target",
        tags$span(class = "packet-change-kicker", "Target"),
        tags$span(class = "packet-change-file", target_label)
      ),
      tags$div(
        class = "packet-change-section",
        tags$div(class = "packet-change-label", "Current selection"),
        tags$pre(class = "packet-code-block", tags$code(context$selection_text %||% ""))
      ),
      tags$div(
        class = "packet-change-section",
        tags$div(class = "packet-change-label", "Replacement"),
        tags$pre(class = "packet-code-block", tags$code(replacement_text %||% ""))
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_replace_selection", "Replace selection", class = "packet-primary-btn")
    ),
    easyClose = TRUE,
    size = "l"
  )
}

packetllm_client_script <- function() {
  tags$script(HTML("
    function packetSend(inputId, value) {
      Shiny.setInputValue(inputId, value, { priority: 'event' });
    }
    $(document).on('click', '.send-btn-class', function() {
      packetSend('dynamic_send_click', { convId: $(this).data('conv-id'), timestamp: Date.now() });
    });
    $(document).on('click', '.add-file-btn-class', function() {
      if ($(this).is(':disabled')) return;
      packetSend('dynamic_add_file_click', { convId: $(this).data('conv-id'), timestamp: Date.now() });
      $('#hidden_file_input').val(null).click();
    });
    $(document).on('click', '.packet-action-copy', function() {
      var text = $(this).attr('data-text') || '';
      if (navigator.clipboard && navigator.clipboard.writeText) {
        navigator.clipboard.writeText(text);
      }
      $(this).text('Copied');
      var btn = $(this);
      setTimeout(function(){ btn.text(btn.data('original-label') || 'Copy'); }, 1200);
    });
    $(document).on('mouseenter', '.packet-action-copy', function() {
      if (!$(this).data('original-label')) $(this).data('original-label', $(this).text());
    });
    $(document).on('click', '.packet-action-insert, .packet-action-replace', function() {
      if ($(this).is(':disabled')) return;
      packetSend('packet_response_action', {
        action: $(this).attr('data-action'),
        convId: $(this).attr('data-conv-id'),
        text: $(this).attr('data-text') || '',
        timestamp: Date.now()
      });
    });
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('resetFileInput', function(message) {
        var el = $('#' + message.id);
        if (el.length > 0) el.val(null);
      });
    });
  "))
}

packetllm_styles <- function() {
  tags$style(HTML("
    :root {
      --packet-bg: #f7f8fa;
      --packet-panel: #ffffff;
      --packet-text: #1f2933;
      --packet-muted: #667085;
      --packet-border: #d9dee7;
      --packet-soft: #eef2f6;
      --packet-accent: #2563a8;
      --packet-accent-dark: #1f4f86;
      --packet-danger: #b42318;
    }
    body {
      background: var(--packet-bg);
      color: var(--packet-text);
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      margin: 0;
      padding: 0;
    }
    .container-fluid { padding-left: 0; padding-right: 0; }
    .packet-shell { min-height: 100vh; display: flex; flex-direction: column; }
    .packet-topbar {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 12px;
      padding: 10px 14px;
      background: var(--packet-panel);
      border-bottom: 1px solid var(--packet-border);
      position: sticky;
      top: 0;
      z-index: 20;
    }
    .packet-brand { font-weight: 650; font-size: 16px; line-height: 1.2; }
    .packet-subtitle { color: var(--packet-muted); font-size: 12px; margin-top: 1px; }
    .packet-topbar-actions { display: flex; gap: 6px; flex-wrap: wrap; justify-content: flex-end; }
    .packet-topbar-btn, .packet-primary-btn, .packet-secondary-btn, .packet-action-btn, .packet-icon-btn {
      border: 1px solid var(--packet-border);
      background: var(--packet-panel);
      color: var(--packet-text);
      border-radius: 6px;
      font-size: 13px;
      line-height: 1;
      padding: 8px 10px;
      cursor: pointer;
    }
    .packet-topbar-primary, .packet-primary-btn {
      background: var(--packet-accent);
      border-color: var(--packet-accent);
      color: #fff;
    }
    .packet-primary-btn:hover, .packet-topbar-primary:hover { background: var(--packet-accent-dark); color: #fff; }
    .packet-main { flex: 1; padding: 10px 14px 0; min-width: 0; }
    .nav-tabs { border-bottom-color: var(--packet-border); }
    .nav-tabs > li > a { color: var(--packet-muted); padding: 8px 10px; border-radius: 6px 6px 0 0; }
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
      color: var(--packet-text);
      border-color: var(--packet-border) var(--packet-border) transparent;
      background: var(--packet-panel);
    }
    .tab-content > .tab-pane {
      background: var(--packet-panel);
      border: 1px solid var(--packet-border);
      border-top: none;
      min-height: calc(100vh - 112px);
      display: none;
      flex-direction: column;
    }
    .tab-content > .tab-pane.active { display: flex; }
    .packet-close-tab {
      border: none;
      background: transparent;
      color: var(--packet-muted);
      font-size: 11px;
      margin-left: 6px;
      padding: 0 2px;
    }
    .packet-context-row {
      display: flex;
      align-items: center;
      gap: 10px;
      padding: 10px 12px;
      border-bottom: 1px solid var(--packet-border);
      background: #fbfcfd;
    }
    .packet-context-controls { display: flex; align-items: center; gap: 6px; flex-shrink: 0; }
    .packet-context-controls .form-group { margin: 0; }
    .packet-context-controls select { height: 32px; font-size: 13px; padding: 4px 8px; }
    .packet-icon-btn { padding: 8px 9px; color: var(--packet-muted); }
    .packet-context-status { color: var(--packet-muted); font-size: 13px; min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
    .packet-context-ok { color: #344054; }
    .packet-context-muted { color: var(--packet-muted); }
    .packet-chat-history {
      flex: 1;
      overflow-y: auto;
      padding: 14px;
      min-height: 48vh;
      background: var(--packet-panel);
    }
    .packet-empty-state {
      color: var(--packet-muted);
      border: 1px dashed var(--packet-border);
      border-radius: 8px;
      padding: 18px;
      text-align: center;
      background: #fbfcfd;
    }
    .packet-message {
      max-width: 980px;
      margin: 0 auto 14px;
      border: 1px solid var(--packet-border);
      border-radius: 8px;
      background: var(--packet-panel);
      overflow: hidden;
    }
    .packet-message-user { background: #fbfcfd; }
    .packet-message-system { border-color: #f3b4ae; background: #fff7f6; color: var(--packet-danger); }
    .packet-message-label {
      border-bottom: 1px solid var(--packet-border);
      background: #fbfcfd;
      padding: 7px 10px;
      font-size: 12px;
      font-weight: 650;
      color: var(--packet-muted);
    }
    .packet-message-body { padding: 10px; }
    .packet-prewrap { white-space: pre-wrap; overflow-wrap: anywhere; }
    .packet-text-block { margin-bottom: 8px; line-height: 1.45; }
    .packet-markdown h1, .packet-markdown h2, .packet-markdown h3,
    .packet-markdown h4, .packet-markdown h5, .packet-markdown h6 {
      margin: 10px 0 6px;
      font-weight: 650;
      line-height: 1.25;
      letter-spacing: 0;
    }
    .packet-markdown h1 { font-size: 20px; }
    .packet-markdown h2 { font-size: 18px; }
    .packet-markdown h3 { font-size: 16px; }
    .packet-markdown h4, .packet-markdown h5, .packet-markdown h6 { font-size: 14px; }
    .packet-md-p { margin: 0 0 9px; }
    .packet-md-list { margin: 0 0 10px 20px; padding-left: 16px; }
    .packet-md-list li { margin: 3px 0; }
    .packet-md-quote {
      margin: 8px 0;
      padding: 8px 10px;
      border-left: 3px solid var(--packet-border);
      color: var(--packet-muted);
      background: #f8fafc;
    }
    .packet-inline-code {
      font-family: Consolas, 'Liberation Mono', Menlo, monospace;
      font-size: 0.92em;
      background: var(--packet-soft);
      color: #182230;
      border: 1px solid var(--packet-border);
      border-radius: 4px;
      padding: 1px 4px;
    }
    .packet-md-table {
      width: 100%;
      border-collapse: collapse;
      margin: 10px 0;
      font-size: 13px;
      overflow: hidden;
    }
    .packet-md-table th, .packet-md-table td {
      border: 1px solid var(--packet-border);
      padding: 6px 8px;
      vertical-align: top;
    }
    .packet-md-table th { background: #f8fafc; font-weight: 650; }
    .packet-markdown .MathJax, .packet-markdown mjx-container { overflow-x: auto; overflow-y: hidden; max-width: 100%; }
    .packet-code-card, .packet-change-card {
      border: 1px solid var(--packet-border);
      border-radius: 8px;
      margin: 10px 0;
      background: #fcfcfd;
      overflow: hidden;
    }
    .packet-code-card-head, .packet-change-head {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
      padding: 8px 10px;
      border-bottom: 1px solid var(--packet-border);
      background: #f8fafc;
    }
    .packet-code-lang, .packet-change-file {
      font-family: Consolas, 'Liberation Mono', Menlo, monospace;
      font-size: 12px;
      color: #344054;
    }
    .packet-change-kicker, .packet-change-label {
      font-size: 11px;
      text-transform: uppercase;
      letter-spacing: 0;
      color: var(--packet-muted);
      margin-bottom: 2px;
    }
    .packet-change-target {
      color: var(--packet-muted);
      font-size: 12px;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      max-width: 45%;
    }
    .packet-change-section { padding: 10px; border-bottom: 1px solid var(--packet-border); }
    .packet-replace-preview {
      border: 1px solid var(--packet-border);
      border-radius: 8px;
      overflow: hidden;
      background: #fcfcfd;
    }
    .packet-replace-target {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
      padding: 8px 10px;
      border-bottom: 1px solid var(--packet-border);
      background: #f8fafc;
    }
    .modal-dialog .packet-code-block {
      max-height: 260px;
      overflow: auto;
    }
    .packet-code-block {
      margin: 0;
      padding: 10px;
      overflow-x: auto;
      background: #0f172a;
      color: #e5e7eb;
      font-size: 12.5px;
      line-height: 1.45;
      border-radius: 0;
    }
    .packet-code-block code {
      white-space: pre;
      font-family: Consolas, 'Liberation Mono', Menlo, monospace;
      color: inherit;
      background: transparent;
      padding: 0;
    }
    .packet-inline-actions, .packet-response-actions { display: flex; gap: 6px; flex-wrap: wrap; }
    .packet-response-actions { margin-top: 8px; }
    .packet-action-btn { padding: 6px 8px; font-size: 12px; }
    .packet-action-btn:disabled {
      opacity: 0.48;
      cursor: not-allowed;
    }
    .packet-composer {
      position: sticky;
      bottom: 0;
      border-top: 1px solid var(--packet-border);
      background: #fbfcfd;
      padding: 10px 12px;
      z-index: 10;
    }
    .packet-attachments-row {
      display: flex;
      align-items: center;
      gap: 8px;
      margin-bottom: 8px;
      min-width: 0;
    }
    .packet-attachments {
      display: flex;
      align-items: center;
      gap: 6px;
      min-width: 0;
      overflow-x: auto;
      color: var(--packet-muted);
      font-size: 12px;
    }
    .packet-attachment-pill {
      border: 1px solid var(--packet-border);
      background: var(--packet-panel);
      border-radius: 999px;
      padding: 4px 8px;
      white-space: nowrap;
      max-width: 220px;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    .packet-input-row { display: flex; gap: 8px; align-items: stretch; }
    .packet-input-row .form-group { flex: 1; margin: 0; min-width: 0; }
    .packet-input-row textarea {
      min-height: 58px;
      resize: vertical;
      border-color: var(--packet-border);
      border-radius: 8px;
      box-shadow: none;
    }
    .packet-input-row .packet-primary-btn { min-width: 74px; }
    .packet-settings-group { margin-bottom: 12px; }
    .packet-settings-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; }
    @media (max-width: 700px) {
      .packet-topbar { align-items: flex-start; flex-direction: column; }
      .packet-topbar-actions { justify-content: flex-start; width: 100%; }
      .packet-main { padding: 6px 6px 0; }
      .tab-content > .tab-pane { min-height: calc(100vh - 150px); }
      .packet-context-row { align-items: flex-start; flex-direction: column; }
      .packet-context-status { white-space: normal; }
      .packet-input-row { flex-direction: column; }
      .packet-input-row .packet-primary-btn { width: 100%; }
      .packet-change-head { align-items: flex-start; flex-direction: column; }
      .packet-change-target { max-width: 100%; white-space: normal; }
      .packet-settings-grid { grid-template-columns: 1fr; }
    }
  "))
}

# Helper %||%
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
