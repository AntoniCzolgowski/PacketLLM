# llm_chat_app.R

#' Run the LLM Chat Application in RStudio Window
#'
#' Launches the Shiny application as a Gadget in the RStudio Viewer pane,
#' from where it can be opened in a separate window ("Show in new window").
#' The application allows interaction with LLM models, managing conversations,
#' attachments, and settings, without blocking the RStudio console when opened in a new window.
#'
#' @export
#' @return Returns the value passed to \code{shiny::stopApp()} when the application is closed (typically \code{NULL}). The primary purpose is the side effect of launching the interactive application.
#' @import shiny
#' @importFrom shinyjs useShinyjs runjs enable disable show hide
#' @import future promises httr pdftools readtext tools
#' @importFrom utils tail head
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#' # This function launches an interactive Shiny gadget.
#' # It should be run in an interactive R session, preferably within RStudio.
#'
#' # Ensure necessary setup like the OpenAI API key environment variable
#' # might be needed for the application's full functionality once launched.
#' # Example: Sys.setenv(OPENAI_API_KEY = "your_actual_openai_api_key")
#'
#' # Launch the application
#' run_llm_chat_app()
#'
#' # The application will open in the RStudio Viewer or a separate window.
#' # Interaction happens within the app's UI.
#' # To stop the app, close its window or click the 'X' button in the app's UI.
#' }
run_llm_chat_app <- function() {

  # --- Helper function for verbose checking (same as in history_manager) ---
  .is_verbose <- function() getOption("PacketLLM.verbose", default = FALSE)

  # --- UI Definition (unchanged from your provided code) ---
  ui <- fluidPage(
    title = "LLM Chat",
    useShinyjs(),
    tags$head(
      tags$script(HTML("
        function sendClickToServer(inputId, convId) {
          // console.log('JS: Sending click for ' + inputId + ' from conv ' + convId); // Removed JS console log
          Shiny.setInputValue(inputId, { convId: convId, timestamp: Date.now() }, { priority: 'event' });
        }
        $(document).on('click', '.send-btn-class', function() {
          var convId = $(this).data('conv-id');
          sendClickToServer('dynamic_send_click', convId);
        });
        $(document).on('click', '.add-file-btn-class', function() {
          if ($(this).is(':disabled')) {
            // console.log('JS: Add file button clicked but disabled.'); // Removed JS console log
            return;
          }
          var convId = $(this).data('conv-id');
          Shiny.setInputValue('dynamic_add_file_click', { convId: convId, timestamp: Date.now() }, { priority: 'event' });
          $('#hidden_file_input').val(null).click();
          // console.log('JS: Clicked hidden file input for conv ' + convId); // Removed JS console log
        });
        $(document).on('shiny:connected', function() {
          Shiny.addCustomMessageHandler('resetFileInput', function(message) {
             var el = $('#' + message.id);
            if (el.length > 0) {
               el.val(null);
               // console.log('JS: Reset value of file input #' + message.id); // Removed JS console log
            } else {
               console.warn('JS Reset: Element not found: #' + message.id); // Keep JS warning
            }
          });
        });
      ")),
      # CSS styles (unchanged)
      tags$style(HTML("
        body { padding-top: 50px; font-family: sans-serif; }
        .navbar.navbar-default.navbar-fixed-top { border-width: 0 0 1px; min-height: 40px; margin-bottom: 10px; background-color: #ffffff; border-color: #ddd; padding-left: 0; padding-right: 0; }
        .navbar.navbar-default.navbar-fixed-top .container-fluid { display: flex !important; align-items: center !important; flex-wrap: wrap !important; padding-left: 15px !important; padding-right: 15px !important; width: 100%; box-sizing: border-box; }
        .navbar.navbar-default.navbar-fixed-top .navbar-header { float: none !important; flex-shrink: 0 !important; margin: 0 !important; padding: 0 !important; margin-right: 15px !important; }
        .navbar.navbar-default.navbar-fixed-top .navbar-brand { padding-left: 0 !important; padding-right: 0 !important; padding-top: 10px !important; padding-bottom: 10px !important; margin: 0 !important; height: auto !important; font-size: 16px !important; line-height: 20px; display: block !important; }
        .navbar.navbar-default.navbar-fixed-top .navbar-right { float: none !important; flex-shrink: 0 !important; margin: 0 !important; padding: 0 !important; margin-left: auto !important; display: flex !important; align-items: center !important; }
        .navbar.navbar-default.navbar-fixed-top .navbar-right .btn { margin-left: 5px !important; margin-top: 0 !important; margin-bottom: 0 !important; }
        #hidden_file_input { display: none; }
        .close-tab-btn { font-size: 10px; line-height: 1; vertical-align: middle; margin-top: -2px; margin-left: 6px; padding: 1px 4px; }
        .nav-tabs > li > a { padding: 8px 10px; }
        .nav-tabs > li > a .close-tab-btn { visibility: hidden; }
        .nav-tabs > li:hover > a .close-tab-btn { visibility: visible; }
        .nav-tabs > li.active > a .close-tab-btn { visibility: visible; }
        .btn.disabled, .btn[disabled] { cursor: not-allowed; opacity: 0.65; }
        .tab-content > .tab-pane { padding: 15px; border: 1px solid #ddd; border-top: none; min-height: 450px; }
        .chat-history-container-class { height: 55vh; overflow-y: auto; border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; background-color: #f9f9f9; }
        .input-action-row { display: flex; align-items: stretch; gap: 10px; }
        .input-action-row .add-file-btn-class { flex-shrink: 0; height: 56px; width: 56px; font-size: 24px; padding: 0; line-height: 56px; text-align: center; }
        .input-action-row .attachments-container { flex-shrink: 0; flex-basis: 200px; min-width: 100px; max-width: 30%; min-height: 56px; max-height: 80px; overflow-y: auto; border: 1px dashed #ddd; padding: 5px; background-color: #fff; align-self: center; box-sizing: border-box; }
        .input-action-row textarea.form-control { flex-grow: 1; height: 56px; resize: none; margin: 0; box-sizing: border-box; }
        .input-action-row .send-btn-class { flex-shrink: 0; height: 56px; }
        .attachments-input-row, .message-input-row { display: none !important; }
        .message-input-col, .send-button-col { display: none !important; }
        .form-group { margin-bottom: 0px; }
      "))
    ),
    tags$nav(class = "navbar navbar-default navbar-fixed-top",
             tags$div(class = "container-fluid",
                      tags$div(class = "navbar-header",
                               tags$span(class = "navbar-brand", "LLM Chat") # brand
                      ),
                      tags$div(class = "navbar-right",
                               # button labels
                               actionButton("advanced_settings_btn", "Settings", class = "btn-default btn-sm"),
                               actionButton("new_chat_btn", "New Conversation", class = "btn-primary btn-sm"),
                               # <<< NEW BUTTON ADDED HERE >>>
                               actionButton("close_app_btn", "X", class = "btn btn-danger btn-sm")
                      )
             )
    ),
    div(style = "padding-left: 15px; padding-right: 15px;",
        tabsetPanel(id = "chatTabs", type = "tabs")
    ),
    tags$div(style = "display: none;",
             fileInput("hidden_file_input", NULL, multiple = TRUE)
    )
  )

  # --- Server Definition (REDUCED MESSAGES/NOTIFICATIONS) ---
  server <- function(input, output, session) {

    # --- Application State Initialization ---
    initial_conv_id <- initialize_history_manager() # Message handled inside if interactive()
    active_conv_id_rv <- reactiveVal(NULL)
    open_tab_ids_rv <- reactiveVal(character(0))
    first_tab_created <- reactiveVal(FALSE)
    processing_state <- reactiveValues()
    file_upload_context_rv <- reactiveVal(NULL)
    staged_attachments_rv <- reactiveVal(list())

    # --- UI/Logic Helper Functions ---
    create_and_append_new_tab <- function(conv_id, select_tab = TRUE) {
      conv_title <- get_conversation_title(conv_id) %||% paste("ID:", conv_id)
      processing_state[[conv_id]] <- FALSE
      current_staged <- staged_attachments_rv()
      current_staged[[conv_id]] <- list()
      staged_attachments_rv(current_staged)
      tab_content <- tryCatch({ create_tab_content_ui(conv_id) }, error = function(e) {
        shiny::tagList( shiny::h4("Error loading UI for conversation"), shiny::verbatimTextOutput(paste0("error_ui_", conv_id)) )
        output[[paste0("error_ui_", conv_id)]] <- shiny::renderPrint(e)
      })
      insertTab( inputId = "chatTabs", tabPanel(
        title = span(conv_title, actionButton(paste0("close_tab_", conv_id), "x", class = "btn-xs btn-danger close-tab-btn", onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('close_tab_request', '%s', {priority: 'event'})", conv_id))),
        value = conv_id, tab_content ), select = select_tab )
      open_tab_ids_rv(c(open_tab_ids_rv(), conv_id))
      input_id <- NS(conv_id)("user_message_input")
      observeEvent(input[[input_id]], { if(conv_id %in% open_tab_ids_rv()) { active_id <- active_conv_id_rv(); if (!is.null(active_id) && active_id == conv_id) { update_send_button_state(conv_id) } }
      }, ignoreNULL = FALSE, ignoreInit = TRUE, once = FALSE, autoDestroy = TRUE)
    }
    can_send_message <- function(conv_id) {
      if (is.null(conv_id) || !conv_id %in% open_tab_ids_rv()) return(FALSE)
      if (isTRUE(processing_state[[conv_id]])) return(FALSE)
      input_id <- NS(conv_id)("user_message_input")
      input_val <- isolate(input[[input_id]])
      has_text <- nzchar(trimws(input_val %||% ""))
      has_staged_files <- length(isolate(staged_attachments_rv()[[conv_id]]) %||% list()) > 0
      conv_model <- get_conversation_model(conv_id)
      if (is.null(conv_model)) { warning(paste("No model found for conversation ID:", conv_id, "in can_send_message.")); return(FALSE) }
      # simplified_models_list access should be safe as it's defined in the package
      if (conv_model %in% simplified_models_list) {
        return(has_text)
      } else {
        return(has_text || has_staged_files)
      }
    }
    update_send_button_state <- function(conv_id) { req(conv_id); if(conv_id %in% open_tab_ids_rv()) { button_id_selector <- paste0("#", NS(conv_id)("send_query_btn")); can_send <- can_send_message(conv_id); if (can_send) { shinyjs::enable(selector = button_id_selector) } else { shinyjs::disable(selector = button_id_selector) } } }
    update_add_file_button_state <- function(conv_id) { req(conv_id); current_model <- get_conversation_model(conv_id); if(is.null(current_model)) return(); button_id_selector <- paste0("#", NS(conv_id)("add_file_btn")); if (current_model %in% simplified_models_list) { shinyjs::disable(selector = button_id_selector) } else { shinyjs::enable(selector = button_id_selector) } }

    # --- First Tab Initialization ---
    observeEvent(TRUE, {
      req(!first_tab_created())
      # Removed message("SERVER: Initialization - Creating first tab...")
      create_and_append_new_tab(initial_conv_id, select_tab = TRUE)
      first_tab_created(TRUE)
      set_active_conversation(initial_conv_id)
      active_conv_id_rv(initial_conv_id)
      ns <- NS(initial_conv_id)
      output[[ns("chat_history_output")]] <- render_chat_history_ui(get_active_chat_history())
      output[[ns("staged_files_list_output")]] <- render_staged_attachments_list_ui(list())
      update_send_button_state(initial_conv_id)
      update_add_file_button_state(initial_conv_id)
    }, once = TRUE, ignoreInit = FALSE)

    # --- Main Observer for Active Tab Change ---
    observeEvent(input$chatTabs, {
      req(first_tab_created(), input$chatTabs)
      current_tab_id <- input$chatTabs
      if (current_tab_id %in% open_tab_ids_rv() && (is.null(active_conv_id_rv()) || current_tab_id != active_conv_id_rv())) {
        # Removed message("SERVER: Switched active tab...")
        set_active_conversation(current_tab_id)
        active_conv_id_rv(current_tab_id)
        active_history <- get_conversation_history(current_tab_id)
        staged_files_for_tab <- isolate(staged_attachments_rv()[[current_tab_id]]) %||% list()
        if(is.null(active_history)){
          if (!current_tab_id %in% get_all_conversation_ids()) {
            warning(paste("SERVER: No data for conversation", current_tab_id, "- likely deleted."))
            return()
          }
          active_history <- list()
        }
        ns <- NS(current_tab_id)
        output[[ns("chat_history_output")]] <- render_chat_history_ui(active_history)
        output[[ns("staged_files_list_output")]] <- render_staged_attachments_list_ui(staged_files_for_tab)
        session$sendCustomMessage(type = "resetFileInput", message = list(id = "hidden_file_input"))
        update_send_button_state(current_tab_id)
        update_add_file_button_state(current_tab_id)
      } else if (!current_tab_id %in% open_tab_ids_rv() && length(open_tab_ids_rv()) > 0) {
        warning(paste("SERVER: Selected tab", current_tab_id, "is no longer open. Attempting to switch to the first available."))
        first_available_tab <- open_tab_ids_rv()[1]
        if (!is.null(first_available_tab)) { updateTabsetPanel(session, "chatTabs", selected = first_available_tab) }
        else { active_conv_id_rv(NULL); set_active_conversation(NULL) } # Removed message
      } else if (length(open_tab_ids_rv()) == 0) {
        # Removed message("SERVER: No open tabs.")
        active_conv_id_rv(NULL)
        set_active_conversation(NULL)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # --- Button and Action Handling ---
    # 1. New Conversation
    observeEvent(input$new_chat_btn, {
      # Removed message("SERVER: Clicked 'New Conversation'")
      new_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE)
      create_and_append_new_tab(new_id, select_tab = TRUE)
      showNotification("Started a new conversation in a new tab.", type = "message", duration = 3)
    })

    # 2. Send Query
    observeEvent(input$dynamic_send_click, {
      req(input$dynamic_send_click)
      conv_id <- input$dynamic_send_click$convId
      req(conv_id %in% open_tab_ids_rv())
      # Removed message("SERVER: Received Send click...")

      if (!can_send_message(conv_id)) return()

      processing_state[[conv_id]] <- TRUE
      update_send_button_state(conv_id)

      input_id <- NS(conv_id)("user_message_input")
      user_text <- trimws(isolate(input[[input_id]]) %||% "")
      staged_files_to_send <- isolate(staged_attachments_rv()[[conv_id]]) %||% list()

      # Removed message("SERVER (...): Preparing to send...")

      final_content <- user_text
      if (length(staged_files_to_send) > 0) {
        attachment_list_str <- paste("-", staged_files_to_send, collapse = "\n")
        if (nzchar(final_content)) {
          final_content <- paste0(final_content, "\n\n<strong>Attached:</strong>\n", attachment_list_str)
        } else {
          final_content <- paste0("<strong>Attached:</strong>\n", attachment_list_str)
        }
      }

      add_result <- NULL
      add_error <- FALSE
      original_active_conv_id_before_add <- get_active_conversation_id()
      set_active_conversation(conv_id)

      if (nzchar(trimws(final_content))) {
        tryCatch({
          add_result <- add_message_to_active_history(role = "user", content = final_content)
          if (is.list(add_result) && !is.null(add_result$type) && add_result$type == "error") { stop(add_result$message) }
        }, error = function(e) {
          showNotification(paste("Error adding message:", e$message), type = "error", duration=5)
          # Keep message associated with warning/error
          message(paste("SERVER (", conv_id, ") Error in add_message_to_active_history:", e$message))
          set_active_conversation(original_active_conv_id_before_add)
          processing_state[[conv_id]] <- FALSE
          update_send_button_state(conv_id)
          add_error <<- TRUE
        })
        if(add_error) return()

        # --- CLEANUP AFTER SENDING ---
        updateTextAreaInput(session, input_id, value = "")
        current_staged <- staged_attachments_rv()
        current_staged[[conv_id]] <- list()
        staged_attachments_rv(current_staged)
        ns_render <- NS(conv_id)
        output[[ns_render("staged_files_list_output")]] <- render_staged_attachments_list_ui(list())
        # --- END CLEANUP ---

        if (is.list(add_result) && !is.null(add_result$type) && add_result$type == "title_set") {
          final_title <- add_result$new_title %||% "Conversation..."
          escaped_title <- gsub("'", "\\'", gsub("\"", "\\\"", final_title))
          shinyjs::runjs(sprintf( "$('#chatTabs a[data-value=\"%s\"] > span').contents().filter(function(){ return this.nodeType == 3; }).first().replaceWith('%s');", conv_id, escaped_title ))
          # Removed message("SERVER (...): UI title updated...")
        }
      } else {
        # Removed message("SERVER (...): Nothing to send...")
        set_active_conversation(original_active_conv_id_before_add)
        processing_state[[conv_id]] <- FALSE
        update_send_button_state(conv_id)
        return()
      }


      ns_render <- NS(conv_id)
      if (!conv_id %in% get_all_conversation_ids()) {
        # Keep message associated with warning/error condition
        message(paste("SERVER (", conv_id, "): Conversation disappeared after adding message. Aborting."))
        set_active_conversation(original_active_conv_id_before_add)
        processing_state[[conv_id]] <- FALSE
        update_send_button_state(conv_id)
        return()
      }
      current_history <- get_conversation_history(conv_id)
      if(is.null(current_history)) current_history <- list()
      output[[ns_render("chat_history_output")]] <- render_chat_history_ui(current_history)

      chat_history_container_selector <- ".chat-history-container-class"
      if(isolate(active_conv_id_rv()) == conv_id) {
        js_code <- sprintf(
          "var activeTabPane = $('#chatTabs .tab-pane.active');
           var el = activeTabPane.find('%s');
           if (el.length > 0 && el.is(':visible')) {
             setTimeout(function() { el.scrollTop(el[0].scrollHeight); }, 100);
           }",
          chat_history_container_selector
        )
        shinyjs::runjs(js_code)
      }

      notification_id <- paste0("api_call_indicator_", conv_id)
      showNotification("Processing request...", id = notification_id, duration = NULL, type = "message")
      request_conv_id_main <- conv_id
      future({
        set_active_conversation(request_conv_id_main)
        if (!request_conv_id_main %in% get_all_conversation_ids()) { warning(paste("FUTURE (Main -", request_conv_id_main, ") Conversation does not exist before API call.")); stop("Conversation was deleted.") }
        response_text <- tryCatch({ get_assistant_response() }, error = function(e) { error_message <- paste("API Error in future:", e$message); warning(paste("FUTURE (Main -", request_conv_id_main, ")", error_message)); return(error_message) })
        list(response = response_text, conv_id = request_conv_id_main)
      }) %...>% (function(result) {
        response_conv_id <- result$conv_id
        assistant_response_content <- result$response
        if(!response_conv_id %in% open_tab_ids_rv()) { removeNotification(paste0("api_call_indicator_", response_conv_id)); processing_state[[response_conv_id]] <- FALSE; return() } # Removed message
        processing_state[[response_conv_id]] <- FALSE
        update_send_button_state(response_conv_id)
        if (!response_conv_id %in% get_all_conversation_ids()) { removeNotification(paste0("api_call_indicator_", response_conv_id)); return() } # Removed message
        ns_resp <- NS(response_conv_id)
        updated_history <- get_conversation_history(response_conv_id)
        if(is.null(updated_history)) updated_history <- list()
        output[[ns_resp("chat_history_output")]] <- render_chat_history_ui(updated_history)
        if (isolate(active_conv_id_rv()) == response_conv_id) {
          js_code <- sprintf(
            "var activeTabPane = $('#chatTabs .tab-pane.active');
              var el = activeTabPane.find('%s');
              if (el.length > 0 && el.is(':visible')) {
                setTimeout(function() { el.scrollTop(el[0].scrollHeight); }, 100);
              }",
            chat_history_container_selector
          )
          shinyjs::runjs(js_code)
        }
        removeNotification(paste0("api_call_indicator_", response_conv_id))
        conv_title_for_notif <- get_conversation_title(response_conv_id) %||% response_conv_id
        if (is.character(assistant_response_content) && grepl("^(Error|Blad)(:| API:| execution error:| processing| wykonania Future:| przetwarzania)", assistant_response_content, ignore.case = TRUE)) { showNotification(paste("Processing error for:", conv_title_for_notif), type = "error", duration = 5) } else { showNotification(paste("Received response for:", conv_title_for_notif), type = "message", duration = 3) }
      }) %...!% (function(error) {
        error_conv_id_main <- request_conv_id_main
        # Keep message associated with warning/error
        message(paste("FUTURE-ERROR (Main -", error_conv_id_main, ") Future error: ", error$message))
        if(!error_conv_id_main %in% open_tab_ids_rv()) { removeNotification(paste0("api_call_indicator_", error_conv_id_main)); processing_state[[error_conv_id_main]] <- FALSE; return() } # Removed message
        processing_state[[error_conv_id_main]] <- FALSE
        update_send_button_state(error_conv_id_main)
        removeNotification(paste0("api_call_indicator_", error_conv_id_main))
        showNotification(paste("Asynchronous error:", error$message), type="error", duration=10)
        tryCatch({
          set_active_conversation(error_conv_id_main)
          if (error_conv_id_main %in% get_all_conversation_ids()) {
            add_message_to_active_history(role = "system", content = paste("Future execution error:", error$message))
            ns_err <- NS(error_conv_id_main)
            err_history <- get_conversation_history(error_conv_id_main)
            if(is.null(err_history)) err_history <- list()
            output[[ns_err("chat_history_output")]] <- render_chat_history_ui(err_history)
            if(isolate(active_conv_id_rv()) == error_conv_id_main) {
              js_code <- sprintf(
                "var activeTabPane = $('#chatTabs .tab-pane.active');
                  var el = activeTabPane.find('%s');
                  if (el.length > 0 && el.is(':visible')) {
                    setTimeout(function() { el.scrollTop(el[0].scrollHeight); }, 100);
                  }",
                chat_history_container_selector
              )
              shinyjs::runjs(js_code)
            }
          }
        }, error = function(e2){ warning(paste("FUTURE-ERROR (Main -", error_conv_id_main, ") Failed to save future error:", e2$message)) })
      })

      set_active_conversation(original_active_conv_id_before_add)
      NULL
    })


    # 3. Add File (+)
    observeEvent(input$dynamic_add_file_click, {
      req(input$dynamic_add_file_click)
      conv_id <- input$dynamic_add_file_click$convId
      req(conv_id %in% open_tab_ids_rv())
      current_model <- get_conversation_model(conv_id)
      if (is.null(current_model)){ return() }
      if (current_model %in% simplified_models_list) { showNotification(paste("Model", current_model, "does not support attachments."), type="warning", duration=4); return() }
      if (isTRUE(processing_state[[conv_id]])) { showNotification("Please wait for the current response to complete.", type="warning", duration=4); return() }
      # Removed message("SERVER: Clicked Add File...")
      file_upload_context_rv(conv_id)
    })

    # 4. Process Selected Files
    observeEvent(input$hidden_file_input, {
      conv_id_for_upload <- isolate(file_upload_context_rv())
      file_upload_context_rv(NULL)
      req(conv_id_for_upload, conv_id_for_upload %in% open_tab_ids_rv(), input$hidden_file_input)

      # Removed message("SERVER (...): Processing files.")
      current_model <- get_conversation_model(conv_id_for_upload)
      if (is.null(current_model)) return()
      if (current_model %in% simplified_models_list) { warning("Attempting to process file for a simplified model."); session$sendCustomMessage(type = "resetFileInput", message = list(id = "hidden_file_input")); return() }
      if (isTRUE(processing_state[[conv_id_for_upload]])) { # Removed message here
        return()
      }

      files_info_from_input <- input$hidden_file_input
      files_added_to_staging_success <- 0
      files_failed <- 0
      newly_staged_filenames <- character(0)

      original_active_conv_before_upload <- get_active_conversation_id()
      tryCatch({
        set_active_conversation(conv_id_for_upload)

        for (i in seq_len(nrow(files_info_from_input))) {
          file_info <- files_info_from_input[i, ]
          file_name <- file_info$name
          file_path <- file_info$datapath
          # Removed message("SERVER (...): Reading:", file_name))

          file_content <- tryCatch({ read_file_content(file_path) }, error = function(e_read) {
            warning(paste("(", conv_id_for_upload, ") Error reading", file_name, ":", e_read$message))
            showNotification(paste("Error reading", file_name), type="error", duration=5)
            return(NULL)
          })

          if (!is.null(file_content)) {
            added_persistently <- add_attachment_to_active_conversation(name = file_name, content = file_content)
            if (added_persistently) {
              newly_staged_filenames <- c(newly_staged_filenames, file_name)
              files_added_to_staging_success <- files_added_to_staging_success + 1
            } else {
              files_failed <- files_failed + 1
              showNotification(paste("File", file_name, "already exists or an error occurred adding it."), type="warning", duration=4)
            }
          } else {
            files_failed <- files_failed + 1
          }
        }

        if (length(newly_staged_filenames) > 0) {
          current_staged <- staged_attachments_rv()
          existing_staged <- current_staged[[conv_id_for_upload]] %||% list()
          combined_staged <- unique(c(existing_staged, newly_staged_filenames))
          current_staged[[conv_id_for_upload]] <- combined_staged
          staged_attachments_rv(current_staged)

          if (conv_id_for_upload %in% get_all_conversation_ids()) {
            ns_files <- NS(conv_id_for_upload)
            output[[ns_files("staged_files_list_output")]] <- render_staged_attachments_list_ui(combined_staged)
            update_send_button_state(conv_id_for_upload)
          } else {
            # Keep message associated with warning/error condition
            message(paste("Conversation", conv_id_for_upload, "does not exist after processing files."))
          }
        }

        conv_title_for_notif <- get_conversation_title(conv_id_for_upload) %||% conv_id_for_upload
        if (files_added_to_staging_success > 0) showNotification(paste("Added", files_added_to_staging_success, "file(s) to staging for:", conv_title_for_notif), type="message", duration=3)
        if (files_failed > 0) showNotification(paste("Failed to add/read", files_failed, "file(s)."), type="warning", duration=5)

      }, error = function(e) {
        error_msg <- paste("Error processing files:", e$message)
        # Keep message associated with warning/error
        warning(paste("(", conv_id_for_upload, ")", error_msg))
        message(paste("(", conv_id_for_upload, ")", error_msg)) # Also log error to console
        showNotification(error_msg, type = "error", duration = 8)
      }, finally = {
        update_send_button_state(conv_id_for_upload)
        update_add_file_button_state(conv_id_for_upload)
        set_active_conversation(original_active_conv_before_upload)
        session$sendCustomMessage(type = "resetFileInput", message = list(id = "hidden_file_input"))
        # Removed message("SERVER (...): Finished processing files...")
      })
      NULL
    }, ignoreNULL = TRUE, ignoreInit = TRUE)


    # 5. Close Tab
    observeEvent(input$close_tab_request, {
      req(input$close_tab_request)
      tab_id_to_close <- input$close_tab_request
      # Removed message("SERVER: Request to close tab...")
      if(!tab_id_to_close %in% open_tab_ids_rv()){ return() }
      open_tabs <- open_tab_ids_rv()
      if (length(open_tabs) <= 1) { showNotification("Cannot close the last tab.", type = "warning", duration = 3); return() }

      processing_state[[tab_id_to_close]] <- NULL
      current_staged <- staged_attachments_rv()
      current_staged[[tab_id_to_close]] <- NULL
      staged_attachments_rv(current_staged)

      open_tab_ids_rv(setdiff(open_tabs, tab_id_to_close))
      removeTab(inputId = "chatTabs", target = tab_id_to_close)
      delete_conversation(tab_id_to_close)

      current_active_id <- active_conv_id_rv()
      if (!is.null(current_active_id) && current_active_id == tab_id_to_close) {
        active_conv_id_rv(NULL)
        # Removed message("SERVER: The active tab was closed.")
        remaining_tabs <- open_tab_ids_rv()
        if (length(remaining_tabs) == 0) {
          set_active_conversation(NULL)
        }
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # --- "Settings" Modal Logic ---
    observeEvent(input$advanced_settings_btn, {
      active_id <- active_conv_id_rv(); req(active_id, active_id %in% get_all_conversation_ids());
      # Removed message("SERVER: Opening settings...")
      current_conv <- get_conversation_data(active_id);
      if(is.null(current_conv)){ showNotification("Error retrieving conversation data.", type="error"); return() };
      current_model <- current_conv$model %||% "gpt-4o"; current_temp <- current_conv$temperature %||% 0.5; current_sys_msg <- current_conv$system_message %||% "";
      model_is_locked <- is_conversation_started(active_id);
      showModal(modalDialog(
        title = paste("Settings for:", current_conv$title %||% active_id),
        create_advanced_settings_modal_ui(
          available_models = available_openai_models,
          model_value = current_model,
          temp_value = current_temp,
          sys_msg_value = current_sys_msg
        ),
        footer = tagList(modalButton("Cancel"), actionButton("save_advanced_settings", "Save Changes")),
        easyClose = TRUE, size = "m"
      ));
      observe({
        selected_model_in_modal <- input$modal_model; req(selected_model_in_modal);
        model_selector_id <- "modal_model"; model_lock_msg_id <- "model_locked_message";
        temp_input_id <- "modal_temp"; temp_msg_id <- "temp_disabled_message";
        sys_msg_input_id <- "modal_system_message"; sys_msg_msg_id <- "sysmsg_disabled_message";
        is_simplified <- selected_model_in_modal %in% simplified_models_list;
        if (model_is_locked) { shinyjs::disable(model_selector_id); shinyjs::show(model_lock_msg_id) } else { shinyjs::enable(model_selector_id); shinyjs::hide(model_lock_msg_id) };
        if (is_simplified) { shinyjs::disable(temp_input_id); shinyjs::show(temp_msg_id); shinyjs::disable(sys_msg_input_id); shinyjs::show(sys_msg_msg_id) } else { shinyjs::enable(temp_input_id); shinyjs::hide(temp_msg_id); shinyjs::enable(sys_msg_input_id); shinyjs::hide(sys_msg_msg_id) }
      })
    })
    observeEvent(input$save_advanced_settings, {
      active_id <- active_conv_id_rv(); req(active_id, active_id %in% get_all_conversation_ids());
      new_model <- isolate(input$modal_model); new_temp <- isolate(input$modal_temp); new_sys_msg <- isolate(input$modal_system_message);
      model_is_locked <- is_conversation_started(active_id); model_saved <- FALSE;
      current_model_for_check <- get_conversation_model(active_id);
      if (!model_is_locked) {
        if (is.null(new_model) || !new_model %in% available_openai_models) { showNotification("Selected model is invalid.", type = "error"); return() };
        model_saved <- set_conversation_model(active_id, new_model);
        if (model_saved) {
          # Removed message("SERVER: Saved model...")
          update_add_file_button_state(active_id); current_model_for_check <- new_model
        } else {
          showNotification("Could not save model.", type = "warning")
        }
      } else {
        if (!is.null(new_model) && !identical(new_model, current_model_for_check)) { warning("Ignoring attempt to change locked model.") };
        model_saved <- TRUE
      };
      if(!model_saved && !model_is_locked) return();
      temp_saved = FALSE; sys_msg_saved = FALSE;
      if (!current_model_for_check %in% simplified_models_list) {
        if (is.null(new_temp) || !is.numeric(new_temp) || new_temp < 0 || new_temp > 1) { showNotification("Invalid temperature value.", type = "error"); return() };
        if (is.null(new_sys_msg) || !is.character(new_sys_msg) || length(new_sys_msg) != 1) { showNotification("Invalid system message.", type = "error"); return() };
        temp_saved <- set_conversation_temperature(active_id, new_temp);
        sys_msg_saved <- set_conversation_system_message(active_id, new_sys_msg);
        # Removed message("SERVER: Saved Temp/SysMsg...")
        if(!temp_saved || !sys_msg_saved) showNotification("Failed to save temp/sys_msg.", type="warning")
      } else {
        # Removed message("SERVER: Model... is simplified...")
        temp_saved = TRUE; sys_msg_saved = TRUE
      };
      if((model_is_locked || model_saved) && temp_saved && sys_msg_saved) {
        showNotification("Settings have been saved.", type = "message");
        removeModal(); update_send_button_state(active_id); update_add_file_button_state(active_id)
      } else {
        if (!model_saved || !temp_saved || !sys_msg_saved) {
          showNotification("An error occurred while saving settings.", type = "error")
        }
      }
    })

    # --- Close Application Button ---
    observeEvent(input$close_app_btn, {
      # Removed message("SERVER: Close Application button clicked.")
      shiny::stopApp()
    })

  } # End of server definition

  # --- Run the Application ---
  # Run as a gadget in the RStudio Viewer
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 600))

}

#1. devtools::document()
#2. devtools::load_all()
#3. run_llm_chat_app()
