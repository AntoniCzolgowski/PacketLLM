# chat_addin.R

# ZMIANA: Dodanie source dla nowego pliku
# Zakładając, że devtools::load_all() lub budowanie pakietu sobie z tym poradzi,
# ale dla pewności można dodać.
# source("R/advanced_settings.R") # Jeśli uruchamiasz skrypt bezpośrednio
# W kontekście pakietu, funkcje z advanced_settings.R będą dostępne.

#' Uruchomienie gadżetu czatu LLM z kartami konwersacji
#'
#' Główna funkcja uruchamiająca gadżet Shiny. Pozwala na zarządzanie
#' konwersacjami w kartach oraz zmianę ustawień zaawansowanych (temperatura,
#' komunikat systemowy) dla każdej z nich.
#'
#' @export
#' @import shiny shinyjs miniUI httr pdftools readtext tools future promises
chat_addin <- function() {

  # Definicja interfejsu użytkownika gadżetu z kartami
  ui <- miniPage(
    useShinyjs(),
    gadget_head_tags(),
    gadgetTitleBar("Czat LLM",
                   # ZMIANA: Dodanie przycisku "Zaawansowane" po lewej
                   left = miniTitleBarButton("advanced_settings_btn", "Zaawansowane", primary = FALSE),
                   right = miniTitleBarButton("new_chat_btn", "Nowa Konwersacja", primary = TRUE)
    ),
    miniContentPanel(
      padding = 0,
      tabsetPanel(id = "chatTabs", type = "tabs"),
      tags$div(style = "display: none;",
               fileInput("hidden_file_input", NULL, multiple = TRUE)
      )
    )
  )

  # Definicja logiki serwera gadżetu z kartami
  server <- function(input, output, session) {

    # --- Inicjalizacja Stanu ---
    initial_conv_id <- initialize_history_manager()
    active_conv_id_rv <- reactiveVal(NULL)
    open_tab_ids_rv <- reactiveVal(character(0))
    first_tab_created <- reactiveVal(FALSE)
    processing_state <- reactiveValues()
    file_upload_context_rv <- reactiveVal(NULL)

    # --- Funkcje Pomocnicze (wewnętrzne serwera) ---

    create_and_append_new_tab <- function(conv_id, select_tab = TRUE) {
      conv_title <- get_conversation_title(conv_id) %||% paste("ID:", conv_id)
      processing_state[[conv_id]] <- FALSE

      insertTab(
        inputId = "chatTabs",
        tabPanel(
          title = span(conv_title,
                       actionButton(paste0("close_tab_", conv_id), "x",
                                    class = "btn-xs btn-danger close-tab-btn",
                                    onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('close_tab_request', '%s', {priority: 'event'})", conv_id))
          ),
          value = conv_id,
          create_tab_content_ui(conv_id) # UI z gadget_ui_components.R
        ),
        select = select_tab
      )
      open_tab_ids_rv(c(open_tab_ids_rv(), conv_id))

      input_id <- NS(conv_id)("user_message_input")
      observeEvent(input[[input_id]], {
        if(conv_id %in% open_tab_ids_rv()) {
          active_id <- active_conv_id_rv()
          if (!is.null(active_id) && active_id == conv_id) {
            update_send_button_state(conv_id)
          }
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE, once = FALSE, autoDestroy = TRUE)
    }

    can_send_message <- function(conv_id) {
      # ... (bez zmian) ...
      if (is.null(conv_id) || !conv_id %in% open_tab_ids_rv()) return(FALSE)
      if (isTRUE(processing_state[[conv_id]])) return(FALSE)

      input_id <- NS(conv_id)("user_message_input")
      input_val <- isolate(input[[input_id]])
      has_text <- nzchar(trimws(input_val %||% ""))

      conv_exists <- conv_id %in% names(.history_env$conversations)
      if (!conv_exists) return(FALSE)

      attachments <- .history_env$conversations[[conv_id]]$attachments %||% list()
      conversation_has_attachments <- length(attachments) > 0

      return(has_text || conversation_has_attachments)
    }

    update_send_button_state <- function(conv_id) {
      # ... (bez zmian) ...
      req(conv_id)
      if(conv_id %in% open_tab_ids_rv()) {
        button_id_selector <- paste0("#", NS(conv_id)("send_query_btn"))
        can_send <- can_send_message(conv_id)
        if (can_send) {
          shinyjs::enable(selector = button_id_selector)
        } else {
          shinyjs::disable(selector = button_id_selector)
        }
      }
    }

    # --- Inicjalizacja pierwszej karty ---
    observeEvent(TRUE, {
      # ... (bez zmian) ...
      req(!first_tab_created())
      message("SERVER: Inicjalizacja - Tworzę pierwszą kartę dla ID: ", initial_conv_id)
      create_and_append_new_tab(initial_conv_id, select_tab = TRUE)
      first_tab_created(TRUE)
      active_conv_id_rv(initial_conv_id)
      set_active_conversation(initial_conv_id)

      ns <- NS(initial_conv_id)
      output[[ns("chat_history_output")]] <- render_chat_history_ui(get_active_chat_history())
      output[[ns("attached_files_list_output")]] <- render_attachments_list_ui(get_active_conversation_attachments())
      update_send_button_state(initial_conv_id)
    }, once = TRUE, ignoreInit = FALSE)


    # --- Główny Obserwator Zmiany Aktywnej Karty ---
    observeEvent(input$chatTabs, {
      # ... (bez zmian) ...
      req(first_tab_created(), input$chatTabs)
      current_tab_id <- input$chatTabs
      current_active_id <- active_conv_id_rv()

      if (is.null(current_active_id) || current_tab_id != current_active_id) {
        message(paste("SERVER: Zmieniono aktywną kartę na ID:", current_tab_id))

        if (!current_tab_id %in% open_tab_ids_rv()) {
          warning(paste("SERVER: Wybrana karta", current_tab_id, "nie istnieje. Ignoruję."))
          if (length(open_tab_ids_rv()) > 0) {
            first_available_tab <- open_tab_ids_rv()[1]
            message(paste("SERVER: Próbuję przełączyć na pierwszą dostępną kartę:", first_available_tab))
            updateTabsetPanel(session, "chatTabs", selected = first_available_tab)
          } else {
            message("SERVER: Brak otwartych kart do wybrania.")
            active_conv_id_rv(NULL)
            set_active_conversation(NULL)
          }
          return()
        }

        set_active_conversation(current_tab_id)
        active_conv_id_rv(current_tab_id)

        # Sprawdzenie czy konwersacja istnieje w historii przed próbą dostępu
        active_conv_data <- .history_env$conversations[[current_tab_id]]
        if(is.null(active_conv_data)) {
          warning(paste("SERVER: Brak danych dla konwersacji", current_tab_id, "mimo że jest na liście otwartych kart."))
          # Można by spróbować usunąć tę kartę lub wybrać inną
          return()
        }

        active_history <- active_conv_data$history %||% list()
        active_attachments <- active_conv_data$attachments %||% list()

        ns <- NS(current_tab_id)
        output[[ns("chat_history_output")]] <- render_chat_history_ui(active_history)
        output[[ns("attached_files_list_output")]] <- render_attachments_list_ui(active_attachments)

        session$sendCustomMessage(type = "resetFileInput", message = "hidden_file_input")
        update_send_button_state(current_tab_id)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # --- Pozostałe Observery ---

    # Observer dla 'Nowa Konwersacja'
    observeEvent(input$new_chat_btn, {
      # ... (bez zmian) ...
      message("SERVER: Kliknięto 'Nowa Konwersacja'")
      new_id <- create_new_conversation(activate = FALSE, add_initial_settings = TRUE)
      create_and_append_new_tab(new_id, select_tab = TRUE)
      showNotification("Rozpoczęto nową rozmowę w nowej karcie.", type = "message", duration = 3)
    })

    # Observer dla kliknięć przycisku "Wyślij"
    observeEvent(input$dynamic_send_click, {
      # Logika wysyłania - bez zmian w strukturze, ale wywołanie get_assistant_response() użyje już nowych ustawień
      # ... (kod bez zmian w stosunku do poprzedniej wersji) ...
      req(input$dynamic_send_click)
      conv_id <- input$dynamic_send_click$convId
      req(conv_id %in% open_tab_ids_rv())

      message(paste("SERVER: Otrzymano kliknięcie Wyślij dla conv_id:", conv_id))

      if (!can_send_message(conv_id)) {
        message(paste("SERVER (", conv_id, "): Kliknięto Wyślij, ale can_send_message zwróciło FALSE. Ignoruję."))
        return()
      }

      processing_state[[conv_id]] <- TRUE
      update_send_button_state(conv_id)

      input_id <- NS(conv_id)("user_message_input")
      user_text <- trimws(isolate(input[[input_id]]) %||% "")
      message(paste("SERVER (", conv_id, "): Wysyłam zapytanie. Tekst: '", substr(user_text, 1, 50), "...'"))

      new_title <- NULL
      add_error <- FALSE # Flaga błędu

      if (nzchar(user_text)) {
        original_active_conv <- get_active_conversation_id()
        set_active_conversation(conv_id)
        tryCatch({
          new_title <- add_message_to_active_history(role = "user", content = user_text)
        }, error = function(e) {
          showNotification(paste("Błąd dodawania wiadomości:", e$message), type = "error", duration=5)
          message(paste("SERVER (", conv_id, ") Błąd add_message_to_active_history:", e$message))
          set_active_conversation(original_active_conv)
          processing_state[[conv_id]] <- FALSE
          update_send_button_state(conv_id)
          add_error <<- TRUE
        })
        if(add_error) return()

        set_active_conversation(original_active_conv)
        message(paste("SERVER (", conv_id, "): Clearing input", input_id))
        updateTextAreaInput(session, input_id, value = "")
      } else {
        message(paste("SERVER (", conv_id, "): Wysyłanie zapytania bez nowego tekstu użytkownika."))
      }

      ns_render <- NS(conv_id)
      # Sprawdzenie czy konwersacja nadal istnieje
      if (!conv_id %in% names(.history_env$conversations)) {
        message(paste("SERVER (", conv_id, "): Konwersacja nie istnieje przed odświeżeniem historii. Przerywam."))
        processing_state[[conv_id]] <- FALSE # Zresetuj stan
        return()
      }
      current_history <- .history_env$conversations[[conv_id]]$history %||% list()
      output[[ns_render("chat_history_output")]] <- render_chat_history_ui(current_history)
      shinyjs::runjs(sprintf(
        "var el = $('#%s'); if (el.length && el.is(':visible')) { el.scrollTop(el[0].scrollHeight); }",
        NS(conv_id)("chat-history-container")
      ))

      if (!is.null(new_title)) {
        escaped_title <- gsub("'", "\\'", gsub("\"", "\\\"", new_title))
        shinyjs::runjs(sprintf(
          "$('#chatTabs a[data-value=\"%s\"]').contents().filter(function(){ return this.nodeType == 3; }).first().replaceWith(' %s ');",
          conv_id,
          escaped_title
        ))
      }

      notification_id <- paste0("api_call_indicator_", conv_id)
      showNotification("Przetwarzanie zapytania...", id = notification_id, duration = NULL, type = "message")

      request_conv_id <- conv_id

      future({
        set_active_conversation(request_conv_id)
        if (!request_conv_id %in% names(.history_env$conversations)) {
          warning(paste("(", request_conv_id, ") Konwersacja nie istnieje przed API call. Przerywam future."))
          stop("Konwersacja została usunięta.")
        }
        # Wywołanie get_assistant_response bez argumentu temperatury
        response_text <- tryCatch({
          get_assistant_response() # Użyje temperatury z konwersacji
        }, error = function(e) {
          # ... (obsługa błędu bez zmian) ...
          error_message <- paste("Błąd API/przetwarzania:", e$message)
          warning(paste("(", request_conv_id, ")", error_message))
          tryCatch({
            if (request_conv_id %in% names(.history_env$conversations)) {
              set_active_conversation(request_conv_id)
              add_message_to_active_history(role = "system", content = paste("Błąd:", error_message))
            }
          }, error = function(e2){})
          return(paste("Błąd:", error_message))
        })
        list(response = response_text, conv_id = request_conv_id)
      }) %...>% (function(result) {
        # Sukces Future - bez zmian
        # ... (kod bez zmian) ...
        response_conv_id <- result$conv_id
        assistant_response_content <- result$response

        if(!response_conv_id %in% open_tab_ids_rv()) {
          message(paste("Odpowiedź dla", response_conv_id, "otrzymana, ale karta zamknięta."))
          removeNotification(paste0("api_call_indicator_", response_conv_id))
          return()
        }

        processing_state[[response_conv_id]] <- FALSE

        # Sprawdzenie czy konwersacja istnieje przed aktualizacją UI
        if (!response_conv_id %in% names(.history_env$conversations)) {
          message(paste("Konwersacja", response_conv_id, "nie istnieje po otrzymaniu odpowiedzi API."))
          removeNotification(paste0("api_call_indicator_", response_conv_id))
          return()
        }

        ns_resp <- NS(response_conv_id)
        updated_history <- .history_env$conversations[[response_conv_id]]$history %||% list()
        output[[ns_resp("chat_history_output")]] <- render_chat_history_ui(updated_history)
        shinyjs::runjs(sprintf(
          "var el = $('#%s'); if (el.length && el.is(':visible')) { el.scrollTop(el[0].scrollHeight); }",
          NS(response_conv_id)("chat-history-container")
        ))
        update_send_button_state(response_conv_id)
        removeNotification(paste0("api_call_indicator_", response_conv_id))

        if (grepl("^Błąd:", assistant_response_content, ignore.case = TRUE)) {
          showNotification(paste("Błąd przetwarzania dla:", get_conversation_title(response_conv_id)), type = "error", duration = 5)
        } else {
          showNotification(paste("Otrzymano odpowiedź dla:", get_conversation_title(response_conv_id)), type = "message", duration = 3)
        }

      }) %...!% (function(error) {
        # Błąd Future - bez zmian
        # ... (kod bez zmian) ...
        error_conv_id <- request_conv_id
        if(!error_conv_id %in% open_tab_ids_rv()) {
          message(paste("Błąd future dla", error_conv_id, ", ale karta zamknięta."))
          removeNotification(paste0("api_call_indicator_", error_conv_id))
          return()
        }

        processing_state[[error_conv_id]] <- FALSE
        removeNotification(paste0("api_call_indicator_", error_conv_id))
        showNotification(paste("Błąd asynchroniczny:", error$message), type="error", duration=10)
        warning(paste("(", error_conv_id, ") Błąd future:", error))
        update_send_button_state(error_conv_id)

        tryCatch({
          if (error_conv_id %in% names(.history_env$conversations)) {
            set_active_conversation(error_conv_id)
            add_message_to_active_history(role = "system", content = paste("Błąd future:", error$message))
            ns_err <- NS(error_conv_id)
            err_history <- .history_env$conversations[[error_conv_id]]$history %||% list()
            output[[ns_err("chat_history_output")]] <- render_chat_history_ui(err_history)
          }
        }, error = function(e2){})
      })
      NULL
    })

    # Observer dla kliknięć przycisku "Dodaj plik"
    observeEvent(input$dynamic_add_file_click, {
      # ... (bez zmian) ...
      req(input$dynamic_add_file_click)
      conv_id <- input$dynamic_add_file_click$convId
      req(conv_id %in% open_tab_ids_rv())
      message(paste("SERVER: Otrzymano kliknięcie Dodaj Plik dla conv_id:", conv_id))
      file_upload_context_rv(conv_id)
    })

    # Observer dla globalnego fileInput
    observeEvent(input$hidden_file_input, {
      # ... (bez zmian) ...
      conv_id_for_upload <- file_upload_context_rv()
      file_upload_context_rv(NULL)
      req(conv_id_for_upload, conv_id_for_upload %in% open_tab_ids_rv(), input$hidden_file_input)
      message(paste("SERVER (", conv_id_for_upload, "): Przetwarzam pliki."))

      if (isTRUE(processing_state[[conv_id_for_upload]])) {
        message(paste("SERVER (", conv_id_for_upload, "): Ignoruję dodawanie pliku - przetwarzanie w toku."))
        showNotification("Poczekaj na zakończenie poprzedniej operacji.", type="warning", duration=3)
        session$sendCustomMessage(type = "resetFileInput", message = "hidden_file_input")
        return()
      }

      files_info_from_input <- input$hidden_file_input
      files_added_success <- 0
      files_failed <- 0
      original_active_conv <- get_active_conversation_id()
      processing_state[[conv_id_for_upload]] <- TRUE
      update_send_button_state(conv_id_for_upload)

      tryCatch({
        set_active_conversation(conv_id_for_upload)
        for (i in seq_len(nrow(files_info_from_input))) {
          file_info <- files_info_from_input[i, ]
          file_name <- file_info$name
          file_path <- file_info$datapath
          message(paste("SERVER (", conv_id_for_upload, "): Odczytuję plik:", file_name))
          file_content <- tryCatch({
            read_file_content(file_path)
          }, error = function(e_read) {
            warning(paste("(", conv_id_for_upload, ") Błąd odczytu", file_name, ":", e_read$message))
            showNotification(paste("Błąd odczytu", file_name, ":", e_read$message), type="error", duration=5)
            return(NULL)
          })

          if (!is.null(file_content)) {
            added <- add_attachment_to_active_conversation(name = file_name, content = file_content)
            if (added) {
              files_added_success <- files_added_success + 1
            } else {
              showNotification(paste("Plik", file_name, "już istnieje."), type="warning", duration=3)
              files_failed <- files_failed + 1
            }
          } else {
            files_failed <- files_failed + 1
          }
        }

        # Sprawdzenie czy konwersacja istnieje przed aktualizacją UI
        if (!conv_id_for_upload %in% names(.history_env$conversations)) {
          message(paste("Konwersacja", conv_id_for_upload, "nie istnieje po przetworzeniu plików."))
        } else {
          ns_files <- NS(conv_id_for_upload)
          current_attachments <- get_active_conversation_attachments() # Pobiera dla aktywnej (conv_id_for_upload)
          output[[ns_files("attached_files_list_output")]] <- render_attachments_list_ui(current_attachments)
        }


        if (files_added_success > 0) showNotification(paste("Dodano", files_added_success, "plik(ów) do:", get_conversation_title(conv_id_for_upload)), type="message", duration=3)
        if (files_failed > 0) showNotification(paste("Nie dodano", files_failed, "plik(ów)."), type="warning", duration=5)

      }, error = function(e) {
        error_msg <- paste("Błąd przetwarzania plików:", e$message)
        warning(paste("(", conv_id_for_upload, ")", error_msg))
        showNotification(error_msg, type = "error", duration = 8)
      }, finally = {
        processing_state[[conv_id_for_upload]] <- FALSE
        update_send_button_state(conv_id_for_upload)
        set_active_conversation(original_active_conv)
        session$sendCustomMessage(type = "resetFileInput", message = "hidden_file_input")
        message(paste("SERVER (", conv_id_for_upload, "): Zakończono przetwarzanie plików."))
      })
      NULL
    })


    # Zamykanie karty
    observeEvent(input$close_tab_request, {
      # ... (bez zmian) ...
      req(input$close_tab_request)
      tab_id_to_close <- input$close_tab_request
      message(paste("SERVER: Żądanie zamknięcia karty ID:", tab_id_to_close))

      if(!tab_id_to_close %in% open_tab_ids_rv()){
        warning("SERVER: Próba zamknięcia nieistniejącej karty:", tab_id_to_close)
        return()
      }

      open_tabs <- open_tab_ids_rv()
      if (length(open_tabs) <= 1) {
        showNotification("Nie można zamknąć ostatniej karty.", type = "warning", duration = 3)
        return()
      }

      processing_state[[tab_id_to_close]] <- NULL
      open_tab_ids_rv(setdiff(open_tabs, tab_id_to_close))
      removeTab(inputId = "chatTabs", target = tab_id_to_close)
      delete_conversation(tab_id_to_close)

      current_active_id <- active_conv_id_rv()
      if (!is.null(current_active_id) && current_active_id == tab_id_to_close) {
        active_conv_id_rv(NULL)
        message("SERVER: Aktywna karta została zamknięta.")
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ZMIANA: Observer dla przycisku "Zaawansowane"
    observeEvent(input$advanced_settings_btn, {
      active_id <- active_conv_id_rv()
      req(active_id, active_id %in% names(.history_env$conversations)) # Wymagaj aktywnej i istniejącej konwersacji

      message(paste("SERVER: Otwieram ustawienia zaawansowane dla:", active_id))

      # Pobierz aktualne ustawienia dla aktywnej konwersacji
      current_conv <- .history_env$conversations[[active_id]]
      current_temp <- current_conv$temperature %||% 0.5
      current_sys_msg <- current_conv$system_message %||% ""

      # Pokaż modal
      showModal(modalDialog(
        title = paste("Ustawienia zaawansowane dla:", current_conv$title %||% active_id),
        # Wywołaj funkcję tworzącą UI z advanced_settings.R
        create_advanced_settings_modal_ui(
          temp_value = current_temp,
          sys_msg_value = current_sys_msg,
          temp_input_id = "modal_temp", # Użyj stałych ID dla modala
          msg_input_id = "modal_system_message"
        ),
        footer = tagList(
          modalButton("Anuluj"),
          actionButton("save_advanced_settings", "Zapisz zmiany")
        ),
        easyClose = TRUE,
        size = "m" # Rozmiar modala: s, m, l
      ))
    })

    # ZMIANA: Observer dla przycisku "Zapisz zmiany" w modalu
    observeEvent(input$save_advanced_settings, {
      active_id <- active_conv_id_rv()
      req(active_id, active_id %in% names(.history_env$conversations)) # Wymagaj aktywnej i istniejącej

      # Odczytaj wartości z inputów modala
      new_temp <- input$modal_temp
      new_sys_msg <- input$modal_system_message

      # Sprawdź czy wartości są poprawne (prosta walidacja)
      if (!is.numeric(new_temp) || new_temp < 0 || new_temp > 1) {
        showNotification("Nieprawidłowa wartość temperatury.", type = "error")
        return() # Nie zamykaj modala, nie zapisuj
      }
      if (!is.character(new_sys_msg) || length(new_sys_msg) != 1) {
        showNotification("Nieprawidłowy komunikat systemowy.", type = "error")
        return()
      }

      # Zapisz nowe wartości bezpośrednio w obiekcie konwersacji
      .history_env$conversations[[active_id]]$temperature <- new_temp
      .history_env$conversations[[active_id]]$system_message <- new_sys_msg

      message(paste("SERVER: Zapisano ustawienia zaawansowane dla:", active_id, "- Temp:", new_temp, "- SysMsg:", substr(new_sys_msg, 1, 30), "..."))
      showNotification("Ustawienia zaawansowane zostały zapisane.", type = "message")
      removeModal() # Zamknij modal
    })


  } # Koniec server

  # Uruchomienie gadżetu
  runGadget(ui, server, viewer = dialogViewer("Czat LLM (Karty)", width = 750, height = 680))

}

# Helper %||%
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}



# Komentarze dotyczące użycia i budowania pakietu pozostają bez zmian

# Komentarze dotyczące użycia i budowania pakietu
# Po zapisaniu tych trzech plików (chat_addin.R, gadget_ui_components.R, gadget_rendering_helpers.R)
# oraz pozostałych (openai_api.R, file_utils.R, chat_logic.R, history_manager.R) w folderze R/:
# 1. Upewnij się, że plik DESCRIPTION zawiera w `Imports:` wszystkie potrzebne pakiety:
#    shiny, shinyjs, miniUI, httr, pdftools, readtext, tools, future, promises
# 2. Uruchom `devtools::document()` aby zaktualizować NAMESPACE i dokumentację.
#    Sprawdź, czy w NAMESPACE eksportowana jest tylko funkcja `chat_addin`.
#    Sprawdź, czy NAMESPACE zawiera dyrektywy importujące pakiety z tagów @import.
# 3. Zbuduj i załaduj pakiet: `devtools::install()` lub `devtools::load_all()`.
# 4. Ustaw klucz API OpenAI w .Renviron.
# 5. Uruchom gadżet: `TwojaNazwaPakietu::chat_addin()`
