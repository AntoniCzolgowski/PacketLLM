#' Uruchomienie gadżetu czatu LLM
#'
#' Otwiera okno dialogowe Shiny Gadget umożliwiające interakcję z LLM,
#' w tym dołączanie plików za pomocą przycisku '+'.
#'
#' @export
#' @import shiny miniUI httr pdftools readtext tools
chat_addin <- function() {

  # Definicja interfejsu użytkownika gadżetu (bez zmian)
  ui <- miniPage(
    # ... (reszta UI bez zmian) ...
    gadgetTitleBar("Czat LLM",
                   left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("new_chat", "Nowa rozmowa", primary = FALSE)),
    miniContentPanel(
      padding = 10,
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
               # Ukryty fileInput - zakładamy teraz, że ID jest bezpośrednio na <input>
               tags$div(style = "display: none;",
                        fileInput("hidden_file_input", NULL, multiple = TRUE)
               )
        ),
        column(10,
               tags$div(
                 tags$strong("Załączone pliki (dla bieżącego zapytania):"),
                 tags$div(
                   id = "attached-files-list-container",
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
                             width = '100%')
        ),
        column(3,
               style = "padding-top: 5px;",
               actionButton("send_message_btn", "Wyślij", width = '100%', style = "margin-bottom: 5px;"),
               actionButton("ask_model_btn", "Zapytaj model", class = "btn-primary", width = '100%')
        )
      )
    )
  )

  # Definicja logiki serwera gadżetu (bez zmian)
  server <- function(input, output, session) {
    # ... (cała logika serwera bez zmian) ...
    # --- Inicjalizacja ---
    new_chat()
    chat_history_rv <- reactiveVal(get_chat_history())
    attached_files_rv <- reactiveVal(character(0))

    # --- Renderowanie UI ---
    output$chat_history_output <- renderUI({
      history <- chat_history_rv()
      if (is.null(history) || length(history) == 0) {
        return(tags$p(tags$em("Rozpocznij rozmowę.")))
      }
      formatted_messages <- lapply(history, function(msg) {
        tags$div(
          style="margin-bottom: 8px; padding: 5px; border-radius: 5px;",
          if(msg$role == "user") {
            list(style = "background-color: #e1f5fe; text-align: right;", tags$strong("Ty:"))
          } else if (msg$role == "assistant") {
            list(style = "background-color: #f0f4c3;", tags$strong("Asystent:"))
          } else {
            list(style = "color: #777; font-style: italic; font-size: 0.9em;", tags$strong("System:"))
          },
          tags$div(style = "white-space: pre-wrap; word-wrap: break-word;", msg$content)
        )
      })
      tagList(formatted_messages)
    })

    output$attached_files_list_output <- renderUI({
      file_names <- attached_files_rv()
      if (length(file_names) == 0) {
        tags$p(tags$em("Brak plików dołączonych do tego zapytania."), style="padding: 5px;")
      } else {
        tags$ul(style="margin: 0; padding-left: 20px;",
                lapply(file_names, function(name) tags$li(name))
        )
      }
    })

    # --- Obsługa zdarzeń ---
    observeEvent(input$new_chat, {
      message("Rozpoczynanie nowej rozmowy...")
      new_chat()
      chat_history_rv(get_chat_history())
      attached_files_rv(character(0))
      updateTextAreaInput(session, "user_message_input", value = "")
      session$sendCustomMessage(type = 'resetFileInput', message = 'hidden_file_input')
      showNotification("Rozpoczęto nową rozmowę.", type = "message", duration = 3)
    })

    observeEvent(input$send_message_btn, {
      user_text <- trimws(input$user_message_input)
      if (nzchar(user_text)) {
        message("Dodawanie wiadomości użytkownika...")
        add_user_message(user_text)
        chat_history_rv(get_chat_history())
        updateTextAreaInput(session, "user_message_input", value = "")
      } else {
        showNotification("Wiadomość nie może być pusta.", type = "warning", duration = 3)
      }
    })

    observeEvent(input$hidden_file_input, {
      req(input$hidden_file_input)
      files <- input$hidden_file_input
      message(paste("Otrzymano", nrow(files), "plik(ów) przez ukryty input."))
      tryCatch({
        for (i in seq_len(nrow(files))) {
          file_info <- files[i, ]
          message(paste("Próba odczytu i dołączenia pliku:", file_info$name))
          file_content <- read_file_content(file_info$datapath)
          attach_file(file_info$name, file_content)
        }
        attached_files_rv(get_current_attachment_names())
        showNotification(paste("Dołączono", nrow(files), "plik(ów)."), type = "message", duration = 3)
      }, error = function(e) {
        error_msg <- paste("Błąd podczas dołączania pliku:", e$message)
        warning(error_msg)
        showNotification(error_msg, type = "error", duration = 10)
        attached_files_rv(get_current_attachment_names())
      })
      session$sendCustomMessage(type = 'resetFileInput', message = 'hidden_file_input')
    })

    observeEvent(input$ask_model_btn, {
      message("Wysyłanie zapytania do modelu...")
      user_text <- trimws(input$user_message_input)
      if (nzchar(user_text)) {
        add_user_message(user_text)
        chat_history_rv(get_chat_history())
        updateTextAreaInput(session, "user_message_input", value = "")
      }
      current_history <- get_chat_history()
      has_attachments <- length(get_current_attachment_names()) > 0
      last_msg <- if (length(current_history) > 0) tail(current_history, 1)[[1]] else NULL
      can_send <- has_attachments || (!is.null(last_msg) && last_msg$role == "user")
      if (!can_send) {
        showNotification("Dodaj wiadomość lub załącz plik przed zapytaniem modelu.", type = "warning", duration = 5)
        return()
      }
      showNotification("Przetwarzanie zapytania...", id = "api_call_indicator", duration = NULL, type = "message")
      assistant_response <- tryCatch({
        get_assistant_response()
      }, error = function(e) {
        error_message <- paste("Błąd przed wywołaniem API:", e$message)
        warning(error_message)
        list(role="system", content=error_message)
      })
      removeNotification("api_call_indicator")
      if (is.list(assistant_response) && !is.null(assistant_response$role) && assistant_response$role == "system") {
        current_history_after_call <- get_chat_history()
        last_msg_after_call <- tail(current_history_after_call, 1)[[1]]
        if(is.null(last_msg_after_call) || last_msg_after_call$content != assistant_response$content) {
          .chat_env$history[[length(.chat_env$history) + 1]] <- assistant_response
        }
      }
      chat_history_rv(get_chat_history())
      attached_files_rv(character(0))
      final_response_content <- tail(get_chat_history(), 1)[[1]]$content
      if (grepl("^Błąd API:", final_response_content, ignore.case = TRUE)) {
        showNotification("Otrzymano odpowiedź z błędem od API.", type = "warning", duration = 5)
      } else if (grepl("^Błąd przed wywołaniem API:", final_response_content, ignore.case = TRUE)) {
        showNotification("Wystąpił błąd lokalny przed wysłaniem zapytania.", type = "error", duration = 5)
      } else {
        showNotification("Otrzymano odpowiedź.", type = "message", duration = 3)
      }
    })

    observeEvent(input$cancel, {
      message("Gadżet anulowany.")
      stopApp(invisible(NULL))
    })
    observeEvent(input$done, {
      message("Gadżet zakończony (Done).")
      final_history <- get_chat_history()
      stopApp(final_history)
    })
  } # Koniec server

  # Dodanie niestandardowego JS (kliknięcie i reset) z poprawionym selektorem
  ui_with_js <- tagList(
    tags$head(
      # --- POPRAWIONY BLOK JAVASCRIPT ---
      tags$script(HTML("
            $(document).ready(function() {
              console.log('Document ready. Podpinanie obsługi przycisku +.');

              $(document).on('click', '#add_file_btn', function(e) {
                console.log('Przycisk + kliknięty.');
                e.preventDefault();

                // *** ZMIENIONY SELEKTOR ***
                // Zakładamy, że ID 'hidden_file_input' jest BEZPOŚREDNIO na <input type='file'>
                var fileInput = $('#hidden_file_input');
                console.log('Wynik selektora #hidden_file_input:', fileInput.length);

                // Sprawdź dodatkowo, czy znaleziony element to faktycznie input[type=file]
                if (fileInput.length > 0 && fileInput.is('input[type=file]')) {
                  console.log('Znaleziono ukryty input file. Wywołuję click().');
                  fileInput[0].click(); // Wywołaj natywne click
                  console.log('Po wywołaniu click() na ukrytym inpucie.');
                } else {
                  // Jeśli to nie zadziała, spróbujmy poprzedniego selektora jako fallback
                  console.warn('Selektor #hidden_file_input nie znalazł input[type=file]. Próbuję selektora zagnieżdżonego...');
                  fileInput = $('#hidden_file_input input[type=file]'); // Wróć do poprzedniego
                  if (fileInput.length > 0) {
                      console.log('Znaleziono ukryty input file za pomocą selektora zagnieżdżonego. Wywołuję click().');
                      fileInput[0].click();
                      console.log('Po wywołaniu click() na ukrytym inpucie (fallback).');
                  } else {
                      console.error('Nie można znaleźć ukrytego elementu input file ani przez #hidden_file_input, ani przez selektor zagnieżdżony!');
                      alert('Błąd: Nie można zainicjować wyboru pliku. Sprawdź strukturę HTML lub skontaktuj się z administratorem.');
                  }
                }
              });
            }); // Koniec document.ready

            // Handler do resetowania wartości inputu
            $(document).on('shiny:connected', function() {
              console.log('Shiny connected. Definiuję handler resetFileInput.');
              Shiny.addCustomMessageHandler('resetFileInput', function(id) {
                // Spróbuj obu selektorów dla pewności
                var el = $('#' + id); // Najpierw bezpośrednio po ID
                if (!el.is('input[type=file]')) { // Jeśli to nie jest input file
                    el = $('#' + id).find('input[type=file]').first(); // Szukaj wewnątrz
                }

                if (el.length > 0 && el.is('input[type=file]')) {
                  console.log('Resetuję wartość inputu: #' + id);
                  el.val(null);
                } else {
                  console.warn('Nie znaleziono inputu [type=file] do zresetowania dla ID: #' + id);
                }
              });
            }); // Koniec shiny:connected

          "))
      # --- KONIEC BLOKU JAVASCRIPT ---
    ),
    ui # Oryginalny UI zdefiniowany powyżej
  )

  # Uruchomienie gadżetu
  runGadget(ui_with_js, server, viewer = dialogViewer("Czat LLM", width = 700, height = 600))
}

# Aby przetestować:
# devtools::load_all()
# chat_addin()
