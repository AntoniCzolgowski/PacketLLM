# ui.R

#' Uruchomienie gadżetu czatu LLM z historią konwersacji
#'
#' Otwiera okno dialogowe Shiny Gadget umożliwiające interakcję z LLM.
#' Pozwala na prowadzenie wielu rozmów w jednej sesji i przełączanie się
#' między nimi za pomocą przycisku "Historia".
#' Dołączanie plików za pomocą przycisku '+'. Przycisk "Wyślij"
#' jest aktywny tylko gdy jest tekst lub załączone pliki.
#' Zamykanie okna za pomocą przycisku 'X' zamyka gadżet.
#'
#' @export
#' @import shiny shinyjs miniUI httr pdftools readtext tools
chat_addin <- function() {

  # Definicja interfejsu użytkownika gadżetu
  ui <- miniPage(
    useShinyjs(), # Aktywuj shinyjs
    tags$div(id = "history-modal-container"),
    gadgetTitleBar("Czat LLM",
                   left = miniTitleBarButton("show_history_btn", "Historia", primary = FALSE),
                   right = miniTitleBarButton("new_chat_btn", "Nowa rozmowa", primary = FALSE)
    ),
    miniContentPanel(
      padding = 10,
      tags$h5(style = "margin-top: 0; margin-bottom: 10px; font-weight: bold; color: #555;",
              textOutput("active_conversation_title")),
      tags$div(
        id = "chat-history-container",
        style = "overflow-y: auto; height: 300px; border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; background-color: #f9f9f9;",
        uiOutput("chat_history_output")
      ),
      fluidRow(
        style = "margin-bottom: 15px; display: flex; align-items: center;",
        column(2,
               style = "padding-right: 0; text-align: center;",
               actionButton("add_file_btn", "+", class = "btn-sm"),
               tags$div(style = "display: none;",
                        fileInput("hidden_file_input", NULL, multiple = TRUE)
               )
        ),
        column(10,
               tags$div(
                 tags$div(
                   id = "conversation-attachments-list-container",
                   style = "min-height: 40px; max-height: 100px; overflow-y: auto; border: 1px dashed #ddd; padding: 5px; margin-top: 5px; background-color: #fff;",
                   uiOutput("attached_files_list_output")
                 )
               )
        )
      ),
      fluidRow(
        column(9,
               textAreaInput("user_message_input",
                             label = NULL,
                             placeholder = "Wpisz wiadomość lub pytanie...",
                             rows = 2,
                             resize = "vertical",
                             width = "100%")
        ),
        column(3,
               style = "padding-top: 5px;",
               actionButton("send_query_btn", "Wyślij", class = "btn-primary", width = "100%")
        )
      )
    )
  ) # Koniec miniPage

  # Definicja logiki serwera gadżetu
  server <- function(input, output, session) {

    # --- Inicjalizacja Stanu ---
    initial_conv_id <- initialize_history_manager()
    active_conv_id_rv <- reactiveVal(initial_conv_id)
    chat_history_rv <- reactiveVal(get_active_chat_history())
    conversation_attachments_rv <- reactiveVal(get_active_conversation_attachments())

    # --- Główny Obserwator Zmian Aktywnej Konwersacji ---
    observeEvent(active_conv_id_rv(), {
      req(active_conv_id_rv())
      current_id <- active_conv_id_rv()
      message(paste("SERVER: Zmieniam aktywną konwersację na ID:", current_id))

      new_history <- get_active_chat_history()
      new_attachments <- get_active_conversation_attachments()

      chat_history_rv(new_history)
      conversation_attachments_rv(new_attachments)

      message(paste("SERVER: Załadowano dla", current_id, "- Historia:", length(new_history), "wiadomości, Załączniki:", length(new_attachments)))

      updateTextAreaInput(session, "user_message_input", value = "")
      session$sendCustomMessage(type = "resetFileInput", message = "hidden_file_input")

    }, ignoreNULL = TRUE)

    # --- Renderowanie UI ---
    output$active_conversation_title <- renderText({
      req(active_conv_id_rv())
      active_conv <- get_active_conversation()
      if (!is.null(active_conv)) {
        paste("Aktywna rozmowa:", active_conv$title %||% "[Brak tytułu]")
      } else {
        "Brak aktywnej rozmowy"
      }
    })

    output$chat_history_output <- renderUI({
      history <- chat_history_rv()
      message("UI RENDER: Historia czatu, wiadomości:", length(history))
      if (is.null(history) || length(history) <= 1) {
        return(tags$p(tags$em("Rozpocznij rozmowę.")))
      }
      formatted_messages <- lapply(history, function(msg) {
        content_display <- msg$content %||% "[BRAK TREŚCI]"
        role_display <- msg$role %||% "unknown"
        tags$div(
          style = paste(
            "margin-bottom: 8px; padding: 5px; border-radius: 5px;",
            if (role_display == "user") "background-color: #e1f5fe; text-align: left; margin-left: 10%; width: 90%;"
            else if (role_display == "assistant") "background-color: #f0f4c3; margin-right: 10%; width: 90%;"
            else "color: #777; font-style: italic; font-size: 0.9em; text-align: center;"
          ),
          if (role_display != "system" && role_display != "unknown") tags$strong(ifelse(role_display == "user", "Ty:", "Asystent:")),
          tags$div(style = "white-space: pre-wrap; word-wrap: break-word;", content_display)
        )
      })
      if (is.null(formatted_messages)) {
        message("Ostrzeżenie: formatted_messages jest NULL w renderUI")
        return(tags$p(tags$em("Błąd podczas formatowania historii.")))
      }
      tagList(formatted_messages)
    })

    output$attached_files_list_output <- renderUI({
      attachments_info <- conversation_attachments_rv()
      message("UI RENDER: Lista załączników, plików:", length(attachments_info))
      if (length(attachments_info) == 0) {
        tags$p(tags$em("Brak plików dołączonych do tej konwersacji."), style = "padding: 5px;")
      } else {
        tagList(
          tags$strong("Pliki w kontekście tej konwersacji:"),
          tags$ul(style = "margin: 0; padding-left: 20px;",
                  lapply(attachments_info, function(att) tags$li(att$name %||% "[Brak nazwy]"))
          )
        )
      }
    })

    # --- Logika przycisku Wyślij ---
    observe({
      has_text <- nzchar(trimws(input$user_message_input))
      conversation_has_attachments <- length(conversation_attachments_rv()) > 0
      can_send <- has_text || conversation_has_attachments
      if (can_send) {
        shinyjs::enable("send_query_btn")
      } else {
        shinyjs::disable("send_query_btn")
      }
    })

    # --- Obsługa Zdarzeń ---

    # Przycisk "Nowa rozmowa"
    observeEvent(input$new_chat_btn, {
      message("SERVER: Kliknięto 'Nowa rozmowa'")
      new_id <- create_new_conversation(activate = TRUE, add_initial_system_msg = TRUE)
      active_conv_id_rv(new_id)
      showNotification("Rozpoczęto nową rozmowę.", type = "message", duration = 3)
    })

    # Przycisk "Historia"
    observeEvent(input$show_history_btn, {
      message("SERVER: Kliknięto 'Historia', generuję modal")
      conv_list <- get_conversation_list_for_display()
      active_id <- active_conv_id_rv()

      if (length(conv_list) == 0) {
        showModal(modalDialog(
          title = "Historia rozmów",
          "Brak zapisanych rozmów w tej sesji.",
          footer = modalButton("Zamknij")
        ))
        return()
      }

      conversation_links <- lapply(conv_list, function(conv) {
        is_active <- conv$id == active_id
        tags$a(
          href = "#",
          class = paste("btn btn-sm history-link", ifelse(is_active, "btn-success", "btn-default")),
          style = paste("display: block; margin-bottom: 5px; text-align: left;", ifelse(is_active, "font-weight: bold;", "")),
          "data-conv-id" = conv$id,
          conv$title %||% "[Brak tytułu]"
        )
      })

      showModal(modalDialog(
        id = "history-modal",
        title = "Historia rozmów (najnowsze u góry)",
        tagList(conversation_links),
        footer = modalButton("Anuluj"),
        easyClose = TRUE,
        size = "m"
      ))
    })

    # Obserwator nasłuchujący wyboru z historii
    observeEvent(input$selected_history_conv_id, {
      req(input$selected_history_conv_id)
      selected_id <- input$selected_history_conv_id
      message(paste("SERVER: Otrzymano ID z JS:", selected_id))

      if (selected_id != active_conv_id_rv()) {
        active_conv_id_rv(selected_id)
      } else {
        message("SERVER: Wybrano już aktywną konwersację, bez zmian.")
      }
      removeModal()
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Przycisk "Wyślij"
    observeEvent(input$send_query_btn, {
      req(active_conv_id_rv())
      user_text <- trimws(input$user_message_input)
      message("SERVER: Kliknięto 'Wyślij'. Tekst: '", user_text, "'")

      if (nzchar(user_text)) {
        add_user_message(user_text)
        updateTextAreaInput(session, "user_message_input", value = "")
        chat_history_rv(get_active_chat_history())
      } else if (length(conversation_attachments_rv()) > 0) {
        message("SERVER: Wysyłanie zapytania bez nowego tekstu (kontekst załączników istnieje).")
      } else {
        showNotification("Błąd: Próba wysłania pustego zapytania bez załączników.", type="error")
        return()
      }

      showNotification("Przetwarzanie zapytania...", id = "api_call_indicator", duration = NULL, type = "message")

      assistant_response_content <- tryCatch({
        get_assistant_response()
        chat_history_rv(get_active_chat_history())
        last_msg <- tail(chat_history_rv(), 1)
        if (length(last_msg) > 0) last_msg[[1]]$content %||% "[Błąd: Brak treści]" else "[Błąd: Pusta historia]"
      }, error = function(e) {
        error_message <- paste("Błąd wewnętrzny:", e$message)
        warning(error_message)
        chat_history_rv(get_active_chat_history())
        return(error_message)
      })

      removeNotification("api_call_indicator")

      if (grepl("^Błąd API:", assistant_response_content, ignore.case = TRUE)) {
        showNotification("Otrzymano odpowiedź z błędem od API.", type = "warning", duration = 5)
      } else if (grepl("^Błąd", assistant_response_content, ignore.case = TRUE)) {
        showNotification("Wystąpił błąd podczas przetwarzania zapytania.", type = "error", duration = 5)
      } else {
        showNotification("Otrzymano odpowiedź.", type = "message", duration = 3)
      }
    })

    # Dodawanie pliku
    observeEvent(input$hidden_file_input, {
      req(input$hidden_file_input)
      files_info_from_input <- input$hidden_file_input
      message(paste("SERVER: Otrzymano", nrow(files_info_from_input), "plik(ów) przez ukryty input."))

      files_added_success <- 0
      files_failed <- 0
      tryCatch({
        for (i in seq_len(nrow(files_info_from_input))) {
          file_info <- files_info_from_input[i, ]
          file_name <- file_info$name
          file_path <- file_info$datapath
          message(paste("SERVER: Próba odczytu i dodania pliku:", file_name))

          file_content <- tryCatch({
            read_file_content(file_path)
          }, error = function(e_read) {
            warning(paste("Błąd odczytu pliku", file_name, ":", e_read$message))
            showNotification(paste("Błąd odczytu pliku", file_name, ":", e_read$message), type="error", duration=8)
            files_failed <- files_failed + 1
            NULL
          })

          if (!is.null(file_content)) {
            added <- add_attachment_to_active_conversation(name = file_name, content = file_content)
            if (added) {
              files_added_success <- files_added_success + 1
            } else {
              showNotification(paste("Plik", file_name, "już istnieje w tej konwersacji."), type="warning", duration=4)
              files_failed <- files_failed + 1
            }
          }
        }

        conversation_attachments_rv(get_active_conversation_attachments())

        if (files_added_success > 0) {
          showNotification(paste("Pomyślnie dodano", files_added_success, "plik(ów) do konwersacji."), type="message", duration=4)
        }
        if (files_failed > 0) {
          showNotification(paste("Nie udało się dodać lub plik już istniał dla", files_failed, "plik(ów)."), type="warning", duration=5)
        }

      }, error = function(e) {
        error_msg <- paste("Błąd podczas przetwarzania dodawanych plików:", e$message)
        warning(error_msg)
        showNotification(error_msg, type = "error", duration = 10)
        conversation_attachments_rv(get_active_conversation_attachments())
      })

      session$sendCustomMessage(type = "resetFileInput", message = "hidden_file_input")
    })

  } # Koniec definicji funkcji 'server'

  # Dodanie niestandardowego JS z poprawioną obsługą kliknięć
  ui_with_js <- tagList(
    tags$head(
      tags$script(HTML("
        // Funkcja do obsługi kliknięć linków historii
        function handleHistoryLinkClick(event) {
          console.log('JS: handleHistoryLinkClick START');
          event.preventDefault();
          const link = event.target.closest('.history-link');
          if (link) {
            console.log('JS: Znaleziono element .history-link:', link);
            const convId = link.getAttribute('data-conv-id');
            console.log('JS: Odczytano data-conv-id:', convId);
            if (convId) {
              console.log('JS: Wysyłanie convId do Shiny:', convId);
              Shiny.setInputValue('selected_history_conv_id', convId, {priority: 'event'});
              console.log('JS: Wysłano convId do Shiny.');
            } else {
              console.error('JS: BŁĄD - Atrybut data-conv-id jest pusty lub nie istnieje.');
            }
          } else {
            console.error('JS: BŁĄD - Nie można znaleźć elementu .history-link po kliknięciu.');
          }
          console.log('JS: handleHistoryLinkClick END');
        }

        // Delegacja zdarzeń - nasłuchuj kliknięć na body
        $(document).on('click', 'body', function(event) {
          const modal = document.getElementById('history-modal');
          if (modal && modal.contains(event.target)) {
            const link = event.target.closest('.history-link');
            if (link) {
              console.log('JS: Kliknięto link historii w modalu, wywołuję handleHistoryLinkClick.');
              handleHistoryLinkClick(event);
            } else {
              console.log('JS: Kliknięto w modal, ale nie na link historii.');
            }
          }
        });

        // Funkcje JS do obsługi przycisku '+' i resetowania fileInput
        $(document).ready(function() {
          console.log('Document ready. Podpinanie obsługi przycisku +.');
          $(document).on('click', '#add_file_btn', function(e) {
            console.log('Przycisk + kliknięty.');
            e.preventDefault();
            var fileInput = document.getElementById('hidden_file_input');
            if (fileInput && fileInput.type === 'file') {
              fileInput.click();
            } else {
              console.error('Nie można znaleźć ukrytego elementu input file!');
              var container = $('#hidden_file_input').closest('.shiny-input-container');
              if (container.length > 0) {
                fileInput = container.find('input[type=file]');
                if (fileInput.length > 0) {
                  fileInput[0].click();
                } else {
                  console.error('Fallback nie znalazł inputa!');
                  alert('Błąd: Nie można zainicjować wyboru pliku.');
                }
              } else {
                console.error('Nie można znaleźć kontenera!');
                alert('Błąd: Nie można zainicjować wyboru pliku.');
              }
            }
          });
        });

        $(document).on('shiny:connected', function() {
          console.log('Shiny connected. Definiuję handler resetFileInput.');
          Shiny.addCustomMessageHandler('resetFileInput', function(id) {
            var el = document.getElementById(id);
            if (el && el.type === 'file') {
              el.value = null;
            } else {
              var container = $('#' + id).closest('.shiny-input-container');
              if (container.length > 0) {
                var fileInput = container.find('input[type=file]');
                if (fileInput.length > 0) {
                  fileInput[0].value = null;
                } else {
                  console.warn('Nie znaleziono inputu [type=file] do zresetowania (w kontenerze)');
                }
              } else {
                console.warn('Nie znaleziono inputu [type=file] ani kontenera do zresetowania');
              }
            }
          });
        });
      ")) # Koniec HTML()
    ), # Koniec tags$head()
    ui # Oryginalny UI
  ) # Koniec tagList()

  # Uruchomienie gadżetu
  runGadget(ui_with_js, server, viewer = dialogViewer("Czat LLM z Historią", width = 700, height = 650))

} # Koniec definicji funkcji chat_addin()

# Helper %||%
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Komentarze dotyczące testowania
# Aby przetestować:
# 1. Zapisz ten plik jako ui.R w folderze R/
# 2. Zapisz nowy plik history_manager.R w folderze R/
# 3. Zapisz zmodyfikowany plik chat_logic.R w folderze R/
# 4. Upewnij się, że masz pliki: file_utils.R, openai_api.R w R/
# 5. Upewnij się, że masz zainstalowane pakiety: shiny, shinyjs, miniUI, httr, pdftools, readtext, tools
# 6. Zbuduj i załaduj pakiet:
#    devtools::document()
#    devtools::load_all()
# 7. Uruchom gadżet:
#    chat_addin()
