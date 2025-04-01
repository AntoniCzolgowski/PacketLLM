# llm_chat_app.R

#' Uruchomienie Aplikacji Czat LLM w Oknie RStudio
#'
#' Uruchamia aplikację Shiny jako Gadget w panelu Viewer RStudio,
#' skąd można ją otworzyć w osobnym oknie ("Show in new window").
#' Aplikacja pozwala na interakcję z modelami LLM, zarządzanie konwersacjami,
#' załącznikami i ustawieniami, nie blokując konsoli RStudio po otwarciu w nowym oknie.
#'
#' @export
#' @import shiny shinyjs future promises httr pdftools readtext tools
#' @importFrom utils tail head
#' @importFrom stats setNames
run_llm_chat_app <- function() {

  # --- Zasoby pakietu ---
  # Zakładamy, że available_openai_models i simplified_models_list są dostępne

  # --- Definicja UI (BEZ ZMIAN) ---
  ui <- fluidPage(
    title = "Czat LLM",
    useShinyjs(),
    tags$head(
      tags$script(HTML("
        function sendClickToServer(inputId, convId) {
          console.log('JS: Sending click for ' + inputId + ' from conv ' + convId);
          Shiny.setInputValue(inputId, { convId: convId, timestamp: Date.now() }, { priority: 'event' });
        }
        $(document).on('click', '.send-btn-class', function() {
          var convId = $(this).data('conv-id');
          sendClickToServer('dynamic_send_click', convId);
        });
        $(document).on('click', '.add-file-btn-class', function() {
          if ($(this).is(':disabled')) {
            console.log('JS: Add file button clicked but disabled.');
            return;
          }
          var convId = $(this).data('conv-id');
          Shiny.setInputValue('dynamic_add_file_click', { convId: convId, timestamp: Date.now() }, { priority: 'event' });
          $('#hidden_file_input').val(null).click();
          console.log('JS: Clicked hidden file input for conv ' + convId);
        });
        $(document).on('shiny:connected', function() {
          Shiny.addCustomMessageHandler('resetFileInput', function(message) {
             var el = $('#' + message.id);
            if (el.length > 0) {
               el.val(null);
               console.log('JS: Reset value of file input #' + message.id);
            } else {
               console.warn('JS Reset: Nie znaleziono elementu: #' + message.id);
            }
          });
        });
      ")),
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
                               tags$span(class = "navbar-brand", "Czat LLM")
                      ),
                      tags$div(class = "navbar-right",
                               actionButton("advanced_settings_btn", "Ustawienia", class = "btn-default btn-sm"),
                               actionButton("new_chat_btn", "Nowa Konwersacja", class = "btn-primary btn-sm")
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

  # --- Definicja Server ---
  server <- function(input, output, session) {

    # --- Inicjalizacja Stanu Aplikacji ---
    initial_conv_id <- initialize_history_manager()
    active_conv_id_rv <- reactiveVal(NULL)
    open_tab_ids_rv <- reactiveVal(character(0))
    first_tab_created <- reactiveVal(FALSE)
    processing_state <- reactiveValues()
    file_upload_context_rv <- reactiveVal(NULL)
    # NOWE: ReactiveVal do przechowywania plików STAGED dla każdej konwersacji
    staged_attachments_rv <- reactiveVal(list()) # Klucz: conv_id, Wartość: wektor nazw plików

    # --- Funkcje Pomocnicze UI/Logiki ---
    create_and_append_new_tab <- function(conv_id, select_tab = TRUE) {
      conv_title <- get_conversation_title(conv_id) %||% paste("ID:", conv_id)
      processing_state[[conv_id]] <- FALSE
      # Inicjalizacja pustej listy staged dla nowej konwersacji
      current_staged <- staged_attachments_rv()
      current_staged[[conv_id]] <- list()
      staged_attachments_rv(current_staged)
      # -----
      tab_content <- tryCatch({ create_tab_content_ui(conv_id) }, error = function(e) {
        shiny::tagList( shiny::h4("Błąd ładowania UI dla konwersacji"), shiny::verbatimTextOutput(paste0("error_ui_", conv_id)) )
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
      # Sprawdź pliki w STAGING area
      has_staged_files <- length(isolate(staged_attachments_rv()[[conv_id]]) %||% list()) > 0
      conv_model <- get_conversation_model(conv_id)
      if (is.null(conv_model)) { warning(paste("Brak modelu dla konwersacji ID:", conv_id, "w can_send_message.")); return(FALSE) }
      if (conv_model %in% simplified_models_list) {
        return(has_text) # Uproszczone modele nie obsługują plików
      } else {
        return(has_text || has_staged_files) # Wyślij jeśli jest tekst LUB pliki w staging
      }
    }
    update_send_button_state <- function(conv_id) { req(conv_id); if(conv_id %in% open_tab_ids_rv()) { button_id_selector <- paste0("#", NS(conv_id)("send_query_btn")); can_send <- can_send_message(conv_id); if (can_send) { shinyjs::enable(selector = button_id_selector) } else { shinyjs::disable(selector = button_id_selector) } } }
    update_add_file_button_state <- function(conv_id) { req(conv_id); current_model <- get_conversation_model(conv_id); if(is.null(current_model)) return(); button_id_selector <- paste0("#", NS(conv_id)("add_file_btn")); if (current_model %in% simplified_models_list) { shinyjs::disable(selector = button_id_selector) } else { shinyjs::enable(selector = button_id_selector) } }

    # --- Inicjalizacja Pierwszej Karty ---
    observeEvent(TRUE, {
      req(!first_tab_created())
      message("SERVER: Inicjalizacja - Tworzę pierwszą kartę dla ID: ", initial_conv_id)
      create_and_append_new_tab(initial_conv_id, select_tab = TRUE)
      first_tab_created(TRUE)
      set_active_conversation(initial_conv_id)
      active_conv_id_rv(initial_conv_id)
      ns <- NS(initial_conv_id)
      output[[ns("chat_history_output")]] <- render_chat_history_ui(get_active_chat_history())
      # Inicjalizacja pustej listy staged files UI
      output[[ns("staged_files_list_output")]] <- render_staged_attachments_list_ui(list())
      update_send_button_state(initial_conv_id)
      update_add_file_button_state(initial_conv_id)
    }, once = TRUE, ignoreInit = FALSE)

    # --- Główny Obserwator Zmiany Aktywnej Karty ---
    observeEvent(input$chatTabs, {
      req(first_tab_created(), input$chatTabs)
      current_tab_id <- input$chatTabs
      if (current_tab_id %in% open_tab_ids_rv() && (is.null(active_conv_id_rv()) || current_tab_id != active_conv_id_rv())) {
        message(paste("SERVER: Zmieniono aktywną kartę na ID:", current_tab_id))
        set_active_conversation(current_tab_id)
        active_conv_id_rv(current_tab_id)
        active_history <- get_conversation_history(current_tab_id)
        # Pobierz STAGED pliki dla nowej karty
        staged_files_for_tab <- isolate(staged_attachments_rv()[[current_tab_id]]) %||% list()
        if(is.null(active_history)){ # Nie sprawdzamy już attachments tutaj
          if (!current_tab_id %in% get_all_conversation_ids()) {
            warning(paste("SERVER: Brak danych dla konwersacji", current_tab_id, "- prawdopodobnie usunięta."))
            return()
          }
          active_history <- list()
        }
        ns <- NS(current_tab_id)
        output[[ns("chat_history_output")]] <- render_chat_history_ui(active_history)
        # Renderuj listę staged files dla tej karty
        output[[ns("staged_files_list_output")]] <- render_staged_attachments_list_ui(staged_files_for_tab)
        session$sendCustomMessage(type = "resetFileInput", message = list(id = "hidden_file_input")) # Czyści globalny input
        update_send_button_state(current_tab_id)
        update_add_file_button_state(current_tab_id)
      } else if (!current_tab_id %in% open_tab_ids_rv() && length(open_tab_ids_rv()) > 0) {
        warning(paste("SERVER: Wybrana karta", current_tab_id, "nie jest już otwarta. Próbuję przełączyć na pierwszą dostępną."))
        first_available_tab <- open_tab_ids_rv()[1]
        if (!is.null(first_available_tab)) { updateTabsetPanel(session, "chatTabs", selected = first_available_tab) }
        else { message("SERVER: Brak otwartych kart po zamknięciu nieaktywnej."); active_conv_id_rv(NULL); set_active_conversation(NULL) }
      } else if (length(open_tab_ids_rv()) == 0) {
        message("SERVER: Brak otwartych kart.")
        active_conv_id_rv(NULL)
        set_active_conversation(NULL)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # --- Obsługa Przycisków i Akcji ---
    # 1. Nowa Konwersacja (Bez zmian)
    observeEvent(input$new_chat_btn, {
      message("SERVER: Kliknięto 'Nowa Konwersacja'")
      new_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE)
      create_and_append_new_tab(new_id, select_tab = TRUE) # To już inicjalizuje staged_attachments dla new_id
      showNotification("Rozpoczęto nową rozmowę w nowej karcie.", type = "message", duration = 3)
    })

    # 2. Wyślij Zapytanie (ZMIENIONE)
    observeEvent(input$dynamic_send_click, {
      req(input$dynamic_send_click)
      conv_id <- input$dynamic_send_click$convId
      req(conv_id %in% open_tab_ids_rv())
      message(paste("SERVER: Otrzymano kliknięcie Wyślij dla conv_id:", conv_id))

      if (!can_send_message(conv_id)) return() # Sprawdza tekst LUB staged files

      processing_state[[conv_id]] <- TRUE
      update_send_button_state(conv_id) # Wyłącz przycisk

      input_id <- NS(conv_id)("user_message_input")
      user_text <- trimws(isolate(input[[input_id]]) %||% "")
      # Pobierz pliki ze STAGING area dla tej konwersacji
      staged_files_to_send <- isolate(staged_attachments_rv()[[conv_id]]) %||% list()

      message(paste("SERVER (", conv_id, "): Przygotowuję wysłanie. Tekst: '", substr(user_text, 1, 50), "...'. Pliki w stagingu:", length(staged_files_to_send)))

      # Przygotuj finalną treść wiadomości
      final_content <- user_text
      if (length(staged_files_to_send) > 0) {
        attachment_list_str <- paste("-", staged_files_to_send, collapse = "\n")
        # Dodaj sekcję "Załączono" tylko jeśli jest tekst LUB same pliki
        if (nzchar(final_content)) {
          # Dodano tagi <strong> wokół "Załączono:"
          final_content <- paste0(final_content, "\n\n<strong>Załączono:</strong>\n", attachment_list_str)
        } else {
          # Dodano tagi <strong> wokół "Załączono:"
          # Jeśli nie ma tekstu, a są pliki, wiadomość użytkownika to tylko informacja o plikach
          final_content <- paste0("<strong>Załączono:</strong>\n", attachment_list_str)
        }
      }

      # Dodaj wiadomość do historii (już sformatowaną)
      add_result <- NULL
      add_error <- FALSE
      original_active_conv_id_before_add <- get_active_conversation_id()
      set_active_conversation(conv_id) # Ustaw jako aktywną na czas dodawania

      # Sprawdź czy jest cokolwiek do wysłania (tekst lub pliki)
      if (nzchar(trimws(final_content))) {
        tryCatch({
          # Używamy add_message_to_active_history bezpośrednio
          add_result <- add_message_to_active_history(role = "user", content = final_content)
          if (is.list(add_result) && !is.null(add_result$type) && add_result$type == "error") { stop(add_result$message) }
        }, error = function(e) {
          showNotification(paste("Błąd dodawania wiadomości:", e$message), type = "error", duration=5)
          message(paste("SERVER (", conv_id, ") Błąd add_message_to_active_history:", e$message))
          set_active_conversation(original_active_conv_id_before_add)
          processing_state[[conv_id]] <- FALSE
          update_send_button_state(conv_id)
          add_error <<- TRUE
        })
        if(add_error) return()

        # --- CZYSZCZENIE PO WYSŁANIU ---
        # 1. Wyczyść pole tekstowe
        updateTextAreaInput(session, input_id, value = "")
        # 2. Wyczyść STAGED attachments dla tej konwersacji
        current_staged <- staged_attachments_rv()
        current_staged[[conv_id]] <- list() # Ustaw na pustą listę
        staged_attachments_rv(current_staged)
        # 3. Zaktualizuj UI listy staged files (powinno pokazać "Brak plików")
        ns_render <- NS(conv_id)
        output[[ns_render("staged_files_list_output")]] <- render_staged_attachments_list_ui(list())
        # --- KONIEC CZYSZCZENIA ---

        # Aktualizacja tytułu (jeśli to pierwsza wiadomość)
        if (is.list(add_result) && !is.null(add_result$type) && add_result$type == "title_set") {
          final_title <- add_result$new_title %||% "Rozmowa..."
          escaped_title <- gsub("'", "\\'", gsub("\"", "\\\"", final_title))
          shinyjs::runjs(sprintf( "$('#chatTabs a[data-value=\"%s\"] > span').contents().filter(function(){ return this.nodeType == 3; }).first().replaceWith('%s');", conv_id, escaped_title ))
          message(paste("SERVER (", conv_id, "): UI zaktualizowane tytułem:", final_title))
        }
      } else {
        # Nic do wysłania (ani tekstu, ani plików w stagingu) - nie powinno się zdarzyć przez can_send_message
        message(paste("SERVER (", conv_id, "): Nic do wysłania (brak tekstu i staged plików). Anulowano."))
        set_active_conversation(original_active_conv_id_before_add)
        processing_state[[conv_id]] <- FALSE
        update_send_button_state(conv_id) # Włącz ponownie, jeśli było zablokowane przez pomyłkę
        return()
      }


      # Aktualizacja historii czatu w UI
      ns_render <- NS(conv_id)
      if (!conv_id %in% get_all_conversation_ids()) {
        message(paste("SERVER (", conv_id, "): Konwersacja zniknęła po dodaniu wiadomości. Przerywam."))
        set_active_conversation(original_active_conv_id_before_add)
        processing_state[[conv_id]] <- FALSE
        update_send_button_state(conv_id)
        return()
      }
      current_history <- get_conversation_history(conv_id)
      if(is.null(current_history)) current_history <- list()
      output[[ns_render("chat_history_output")]] <- render_chat_history_ui(current_history)

      # Przewijanie historii (bez zmian)
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

      # Wywołanie API (bez zmian - używa get_conversation_attachments())
      notification_id <- paste0("api_call_indicator_", conv_id)
      showNotification("Przetwarzanie zapytania...", id = notification_id, duration = NULL, type = "message")
      request_conv_id_main <- conv_id
      future({
        set_active_conversation(request_conv_id_main) # Ustawiamy aktywną dla future
        if (!request_conv_id_main %in% get_all_conversation_ids()) { warning(paste("FUTURE (Main -", request_conv_id_main, ") Konwersacja nie istnieje przed API call.")); stop("Konwersacja została usunięta.") }
        # get_assistant_response użyje TRWAŁYCH załączników z get_conversation_attachments(request_conv_id_main)
        response_text <- tryCatch({ get_assistant_response() }, error = function(e) { error_message <- paste("Błąd API w future:", e$message); warning(paste("FUTURE (Main -", request_conv_id_main, ")", error_message)); return(error_message) })
        list(response = response_text, conv_id = request_conv_id_main)
      }) %...>% (function(result) {
        # Callback po odpowiedzi API (bez zmian logiki przewijania i aktualizacji historii)
        response_conv_id <- result$conv_id
        assistant_response_content <- result$response
        if(!response_conv_id %in% open_tab_ids_rv()) { message(paste("FUTURE-CALLBACK (Main -", response_conv_id, ") Karta zamknięta.")); removeNotification(paste0("api_call_indicator_", response_conv_id)); processing_state[[response_conv_id]] <- FALSE; return() }
        processing_state[[response_conv_id]] <- FALSE
        update_send_button_state(response_conv_id) # Włącz przycisk Wyślij
        if (!response_conv_id %in% get_all_conversation_ids()) { message(paste("FUTURE-CALLBACK (Main -", response_conv_id, ") Konwersacja nie istnieje po API call.")); removeNotification(paste0("api_call_indicator_", response_conv_id)); return() }
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
        if (is.character(assistant_response_content) && grepl("^Błąd(:| API:)", assistant_response_content, ignore.case = TRUE)) { showNotification(paste("Błąd przetwarzania dla:", conv_title_for_notif), type = "error", duration = 5) } else { showNotification(paste("Otrzymano odpowiedź dla:", conv_title_for_notif), type = "message", duration = 3) }
      }) %...!% (function(error) {
        # Callback błędu future (bez zmian)
        error_conv_id_main <- request_conv_id_main
        message(paste("FUTURE-ERROR (Main -", error_conv_id_main, ") Błąd future: ", error$message))
        if(!error_conv_id_main %in% open_tab_ids_rv()) { message(paste("FUTURE-ERROR (Main -", error_conv_id_main, ") Karta zamknięta.")); removeNotification(paste0("api_call_indicator_", error_conv_id_main)); processing_state[[error_conv_id_main]] <- FALSE; return() }
        processing_state[[error_conv_id_main]] <- FALSE
        update_send_button_state(error_conv_id_main) # Włącz przycisk
        removeNotification(paste0("api_call_indicator_", error_conv_id_main))
        showNotification(paste("Błąd asynchroniczny:", error$message), type="error", duration=10)
        tryCatch({
          set_active_conversation(error_conv_id_main)
          if (error_conv_id_main %in% get_all_conversation_ids()) {
            add_message_to_active_history(role = "system", content = paste("Błąd wykonania Future:", error$message))
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
        }, error = function(e2){ warning(paste("FUTURE-ERROR (Main -", error_conv_id_main, ") Nie udało się zapisać błędu future:", e2$message)) })
      })

      # Przywróć pierwotnie aktywną konwersację (jeśli była inna)
      set_active_conversation(original_active_conv_id_before_add)
      NULL
    })


    # 3. Dodaj Plik (+) (kliknięcie) (Bez zmian)
    observeEvent(input$dynamic_add_file_click, {
      req(input$dynamic_add_file_click)
      conv_id <- input$dynamic_add_file_click$convId
      req(conv_id %in% open_tab_ids_rv())
      current_model <- get_conversation_model(conv_id)
      if (is.null(current_model)){ return() }
      if (current_model %in% simplified_models_list) { showNotification(paste("Model", current_model, "nie obsługuje załączników."), type="warning", duration=4); return() }
      if (isTRUE(processing_state[[conv_id]])) { showNotification("Poczekaj na zakończenie odpowiedzi.", type="warning", duration=4); return() }
      message(paste("SERVER: Kliknięto Dodaj Plik dla conv_id:", conv_id, "- Ustawiam kontekst."))
      file_upload_context_rv(conv_id)
    })

    # 4. Przetwarzanie Wybranych Plików (ZMIENIONE)
    observeEvent(input$hidden_file_input, {
      conv_id_for_upload <- isolate(file_upload_context_rv())
      file_upload_context_rv(NULL) # Resetuj kontekst od razu
      req(conv_id_for_upload, conv_id_for_upload %in% open_tab_ids_rv(), input$hidden_file_input)

      message(paste("SERVER (", conv_id_for_upload, "): Przetwarzam pliki."))
      current_model <- get_conversation_model(conv_id_for_upload)
      if (is.null(current_model)) return()
      if (current_model %in% simplified_models_list) { warning("Próba przetworzenia pliku dla modelu uproszczonego."); session$sendCustomMessage(type = "resetFileInput", message = list(id = "hidden_file_input")); return() }
      if (isTRUE(processing_state[[conv_id_for_upload]])) { message(paste("SERVER (", conv_id_for_upload, "): Ignoruję dodawanie pliku - przetwarzanie w toku.")); return() }

      # Nie blokujemy już stanu przetwarzania tutaj, bo to tylko dodawanie do stagingu
      # processing_state[[conv_id_for_upload]] <- TRUE
      # update_send_button_state(conv_id_for_upload) # Stan przycisku zależy teraz od staged files i tekstu
      # update_add_file_button_state(conv_id_for_upload) # Ten może pozostać włączony

      files_info_from_input <- input$hidden_file_input
      files_added_to_staging_success <- 0
      files_failed <- 0
      newly_staged_filenames <- character(0) # Lista nazw plików dodanych w tej operacji

      original_active_conv_before_upload <- get_active_conversation_id()
      tryCatch({
        set_active_conversation(conv_id_for_upload) # Ustaw aktywną na czas operacji

        for (i in seq_len(nrow(files_info_from_input))) {
          file_info <- files_info_from_input[i, ]
          file_name <- file_info$name
          file_path <- file_info$datapath
          message(paste("SERVER (", conv_id_for_upload, "): Odczytuję:", file_name))

          file_content <- tryCatch({ read_file_content(file_path) }, error = function(e_read) {
            warning(paste("(", conv_id_for_upload, ") Błąd odczytu", file_name, ":", e_read$message))
            showNotification(paste("Błąd odczytu", file_name), type="error", duration=5)
            return(NULL)
          })

          if (!is.null(file_content)) {
            # 1. Dodaj do TRWAŁYCH załączników (dla modelu)
            added_persistently <- add_attachment_to_active_conversation(name = file_name, content = file_content)
            if (added_persistently) {
              # 2. Dodaj nazwę do listy STAGED (dla UI i następnej wiadomości)
              newly_staged_filenames <- c(newly_staged_filenames, file_name)
              files_added_to_staging_success <- files_added_to_staging_success + 1
            } else {
              # Nie udało się dodać trwale (np. duplikat), więc nie dodajemy do staged
              files_failed <- files_failed + 1
              showNotification(paste("Plik", file_name, "już istnieje lub wystąpił błąd dodawania."), type="warning", duration=4)
            }
          } else {
            files_failed <- files_failed + 1
          }
        } # Koniec pętli po plikach

        # Po przetworzeniu wszystkich plików, zaktualizuj staged_attachments_rv i UI
        if (length(newly_staged_filenames) > 0) {
          current_staged <- staged_attachments_rv()
          existing_staged <- current_staged[[conv_id_for_upload]] %||% list()
          # Upewnij się, że nie dodajesz duplikatów do listy staged (choć add_attachment... powinien to obsłużyć)
          combined_staged <- unique(c(existing_staged, newly_staged_filenames))
          current_staged[[conv_id_for_upload]] <- combined_staged
          staged_attachments_rv(current_staged)

          # Zaktualizuj UI listy staged files
          if (conv_id_for_upload %in% get_all_conversation_ids()) {
            ns_files <- NS(conv_id_for_upload)
            output[[ns_files("staged_files_list_output")]] <- render_staged_attachments_list_ui(combined_staged)
            update_send_button_state(conv_id_for_upload) # Zaktualizuj stan przycisku Wyślij
          } else {
            message(paste("Konwersacja", conv_id_for_upload, "nie istnieje po przetworzeniu plików."))
          }
        }

        # Notyfikacje
        conv_title_for_notif <- get_conversation_title(conv_id_for_upload) %||% conv_id_for_upload
        if (files_added_to_staging_success > 0) showNotification(paste("Dodano", files_added_to_staging_success, "plik(ów) do przygotowania dla:", conv_title_for_notif), type="message", duration=3)
        if (files_failed > 0) showNotification(paste("Nie udało się dodać/odczytać", files_failed, "plik(ów)."), type="warning", duration=5)

      }, error = function(e) {
        error_msg <- paste("Błąd przetwarzania plików:", e$message)
        warning(paste("(", conv_id_for_upload, ")", error_msg))
        showNotification(error_msg, type = "error", duration = 8)
      }, finally = {
        # Już nie zarządzamy processing_state tutaj
        # processing_state[[conv_id_for_upload]] <- FALSE
        update_send_button_state(conv_id_for_upload) # Upewnij się, że stan przycisku jest aktualny
        update_add_file_button_state(conv_id_for_upload) # Stan tego przycisku zwykle się nie zmienia
        set_active_conversation(original_active_conv_before_upload) # Przywróć aktywną konwersację
        # Zresetuj globalny file input
        session$sendCustomMessage(type = "resetFileInput", message = list(id = "hidden_file_input"))
        message(paste("SERVER (", conv_id_for_upload, "): Zakończono przetwarzanie plików dla stagingu."))
      })
      NULL # Zwróć NULL z observeEvent
    }, ignoreNULL = TRUE, ignoreInit = TRUE)


    # 5. Zamknij Kartę (ZMIENIONE - usuń staged data)
    observeEvent(input$close_tab_request, {
      req(input$close_tab_request)
      tab_id_to_close <- input$close_tab_request
      message(paste("SERVER: Żądanie zamknięcia karty ID:", tab_id_to_close))
      if(!tab_id_to_close %in% open_tab_ids_rv()){ return() }
      open_tabs <- open_tab_ids_rv()
      if (length(open_tabs) <= 1) { showNotification("Nie można zamknąć ostatniej karty.", type = "warning", duration = 3); return() }

      # Usuń dane z reactive values dla tej karty
      processing_state[[tab_id_to_close]] <- NULL
      current_staged <- staged_attachments_rv()
      current_staged[[tab_id_to_close]] <- NULL # Usuń wpis
      staged_attachments_rv(current_staged)

      # Usuń kartę z UI i listy otwartych
      open_tab_ids_rv(setdiff(open_tabs, tab_id_to_close))
      removeTab(inputId = "chatTabs", target = tab_id_to_close)
      # Usuń konwersację z backendu
      delete_conversation(tab_id_to_close)

      # Zarządzanie aktywną kartą (bez zmian)
      current_active_id <- active_conv_id_rv()
      if (!is.null(current_active_id) && current_active_id == tab_id_to_close) {
        active_conv_id_rv(NULL)
        message("SERVER: Aktywna karta została zamknięta.")
        remaining_tabs <- open_tab_ids_rv()
        if (length(remaining_tabs) > 0) {
          # Automatycznie przełącz na pierwszą pozostałą kartę
          # updateTabsetPanel(session, "chatTabs", selected = remaining_tabs[1])
          # Nie robimy tego, observeEvent(input$chatTabs) powinno sobie poradzić
        } else {
          set_active_conversation(NULL) # Brak otwartych kart
        }
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # --- Logika Modala "Ustawienia" (Bez zmian) ---
    observeEvent(input$advanced_settings_btn, { active_id <- active_conv_id_rv(); req(active_id, active_id %in% get_all_conversation_ids()); message(paste("SERVER: Otwieram ustawienia dla:", active_id)); current_conv <- get_conversation_data(active_id); if(is.null(current_conv)){ showNotification("Błąd pobierania danych konwersacji.", type="error"); return() }; current_model <- current_conv$model %||% "gpt-4o"; current_temp <- current_conv$temperature %||% 0.5; current_sys_msg <- current_conv$system_message %||% ""; model_is_locked <- is_conversation_started(active_id); showModal(modalDialog( title = paste("Ustawienia dla:", current_conv$title %||% active_id), create_advanced_settings_modal_ui( available_models = available_openai_models, model_value = current_model, temp_value = current_temp, sys_msg_value = current_sys_msg ), footer = tagList(modalButton("Anuluj"), actionButton("save_advanced_settings", "Zapisz zmiany")), easyClose = TRUE, size = "m" )); observe({ selected_model_in_modal <- input$modal_model; req(selected_model_in_modal); model_selector_id <- "modal_model"; model_lock_msg_id <- "model_locked_message"; temp_input_id <- "modal_temp"; temp_msg_id <- "temp_disabled_message"; sys_msg_input_id <- "modal_system_message"; sys_msg_msg_id <- "sysmsg_disabled_message"; is_simplified <- selected_model_in_modal %in% simplified_models_list; if (model_is_locked) { shinyjs::disable(model_selector_id); shinyjs::show(model_lock_msg_id) } else { shinyjs::enable(model_selector_id); shinyjs::hide(model_lock_msg_id) }; if (is_simplified) { shinyjs::disable(temp_input_id); shinyjs::show(temp_msg_id); shinyjs::disable(sys_msg_input_id); shinyjs::show(sys_msg_msg_id) } else { shinyjs::enable(temp_input_id); shinyjs::hide(temp_msg_id); shinyjs::enable(sys_msg_input_id); shinyjs::hide(sys_msg_msg_id) } }) })
    observeEvent(input$save_advanced_settings, { active_id <- active_conv_id_rv(); req(active_id, active_id %in% get_all_conversation_ids()); new_model <- isolate(input$modal_model); new_temp <- isolate(input$modal_temp); new_sys_msg <- isolate(input$modal_system_message); model_is_locked <- is_conversation_started(active_id); model_saved <- FALSE; current_model_for_check <- get_conversation_model(active_id); if (!model_is_locked) { if (is.null(new_model) || !new_model %in% available_openai_models) { showNotification("Wybrany model jest nieprawidłowy.", type = "error"); return() }; model_saved <- set_conversation_model(active_id, new_model); if (model_saved) { message(paste("SERVER: Zapisano model dla", active_id, "na:", new_model)); update_add_file_button_state(active_id); current_model_for_check <- new_model } else { showNotification("Nie można zapisać modelu.", type = "warning") } } else { if (!is.null(new_model) && !identical(new_model, current_model_for_check)) { warning("Ignoruję próbę zmiany zablokowanego modelu.") }; model_saved <- TRUE }; if(!model_saved && !model_is_locked) return(); temp_saved = FALSE; sys_msg_saved = FALSE; if (!current_model_for_check %in% simplified_models_list) { if (is.null(new_temp) || !is.numeric(new_temp) || new_temp < 0 || new_temp > 1) { showNotification("Nieprawidłowa wartość temperatury.", type = "error"); return() }; if (is.null(new_sys_msg) || !is.character(new_sys_msg) || length(new_sys_msg) != 1) { showNotification("Nieprawidłowy komunikat systemowy.", type = "error"); return() }; temp_saved <- set_conversation_temperature(active_id, new_temp); sys_msg_saved <- set_conversation_system_message(active_id, new_sys_msg); if(temp_saved && sys_msg_saved) message(paste("SERVER: Zapisano Temp/SysMsg dla", active_id)) else showNotification("Nie udało się zapisać temp/sys_msg.", type="warning") } else { message(paste("SERVER: Model", current_model_for_check, "uproszczony. Ignoruję Temp/SysMsg.")); temp_saved = TRUE; sys_msg_saved = TRUE }; if((model_is_locked || model_saved) && temp_saved && sys_msg_saved) { showNotification("Ustawienia zostały zapisane.", type = "message"); removeModal(); update_send_button_state(active_id); update_add_file_button_state(active_id) } else { if (!model_saved || !temp_saved || !sys_msg_saved) { showNotification("Wystąpił błąd podczas zapisywania ustawień.", type = "error") } } })

  } # Koniec definicji server

  # --- Uruchomienie Aplikacji ---
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 600))

}
# --- Komentarze dotyczące użycia (BEZ ZMIAN) ---
# 1. Upewnij się, że wszystkie zależności są w DESCRIPTION (shiny, shinyjs, future, promises, itd.)
# 2. Uruchom `devtools::document()`
# 3. Zbuduj/załaduj pakiet: `devtools::install()` lub `devtools::load_all()`
# 4. Ustaw klucz API OpenAI w .Renviron (jeśli potrzebne).
# 5. Uruchom aplikację: `NazwaTwojegoPakietu::run_llm_chat_app()`
