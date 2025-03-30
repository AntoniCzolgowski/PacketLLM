# gadget_ui_components.R

#' Generuje tagi <head> z zależnościami JS i CSS dla gadżetu
#'
#' @return Obiekt tagList z tagami script i style.
#' @noRd
#' @import shiny
gadget_head_tags <- function() {
  tagList(
    tags$head(
      tags$script(HTML("
        // Funkcja do wysyłania ID klikniętego przycisku do Shiny
        function sendClickToServer(inputId, convId) {
          console.log('JS: Sending click for ' + inputId + ' from conv ' + convId);
          // Używamy obiektu, aby przekazać convId i unikalny timestamp
          Shiny.setInputValue(inputId, { convId: convId, timestamp: Date.now() }, { priority: 'event' });
        }

        // Delegacja zdarzeń dla przycisków 'Wyślij'
        $(document).on('click', '.send-btn-class', function() {
          var convId = $(this).data('conv-id');
          // Wywołaj funkcję wysyłającą dane do Shiny
          sendClickToServer('dynamic_send_click', convId);
        });

        // Delegacja zdarzeń dla przycisków 'Dodaj plik'
        $(document).on('click', '.add-file-btn-class', function() {
          var convId = $(this).data('conv-id');
          // Znajdź *globalny* ukryty input i go kliknij
          // Przekaż ID konwersacji do Shiny, aby wiedzieć, do której karty dodać plik
          Shiny.setInputValue('dynamic_add_file_click', { convId: convId, timestamp: Date.now() }, { priority: 'event' });
          $('#hidden_file_input').val(null).click(); // Resetuj i kliknij globalny input
        });

        // Handler do resetowania fileInput (bez zmian)
        $(document).on('shiny:connected', function() {
          Shiny.addCustomMessageHandler('resetFileInput', function(id) {
            var el = $('#' + id);
            if (el.length > 0) {
               el.val(null);
               var container = el.closest('.shiny-input-container');
               if (container.length > 0) {
                  var fileInput = container.find('input[type=file]');
                  if (fileInput.length > 0) { fileInput.val(null); }
               }
            } else { console.warn('Nie znaleziono elementu do zresetowania: #' + id); }
          });
        });
      ")),
      tags$style(HTML("
        #hidden_file_input { display: none; }
        .close-tab-btn { font-size: 10px; line-height: 1; vertical-align: middle; margin-top: -2px; margin-left: 6px; padding: 1px 4px; }
        .tab-content > .tab-pane { padding: 15px; }
        .nav-tabs > li > a { padding: 8px 10px; }
        .nav-tabs > li > a .close-tab-btn { visibility: hidden; }
        .nav-tabs > li:hover > a .close-tab-btn { visibility: visible; }
        .nav-tabs > li.active > a .close-tab-btn { visibility: visible; }
        /* Styl dla nieaktywnych przycisków, gdy shinyjs je wyłączy */
        .btn.disabled, .btn[disabled] { cursor: not-allowed; opacity: 0.65; }
      "))
    )
  )
}

#' Tworzy UI dla zawartości pojedynczej karty czatu
#'
#' Generuje strukturę HTML dla obszaru historii, listy załączników i pól wprowadzania.
#' Używa namespace'u opartego na `conv_id` do unikalności ID elementów.
#'
#' @param conv_id Unikalny identyfikator konwersacji (używany do namespace'u).
#'
#' @return Obiekt tagList zawierający UI dla karty.
#' @noRd
#' @import shiny
create_tab_content_ui <- function(conv_id) {
  ns <- NS(conv_id) # Tworzymy namespace
  tagList(
    tags$div(
      id = ns("chat-history-container"),
      style = "overflow-y: auto; height: 300px; border: 1px solid #ccc; padding: 10px; margin-bottom: 15px; background-color: #f9f9f9;",
      uiOutput(ns("chat_history_output")) # Namespacowane wyjście
    ),
    fluidRow(
      style = "margin-bottom: 15px; display: flex; align-items: center; padding-left: 10px; padding-right: 10px;",
      column(2, style = "padding-right: 0; text-align: center;",
             # Przycisk "+" z klasą i data-* atrybutem, bez input ID
             tags$button(id = ns("add_file_btn"), type = "button", class = "btn btn-default btn-sm add-file-btn-class", `data-conv-id` = conv_id, "+")
      ),
      column(10,
             tags$div(
               tags$div(id = ns("conversation-attachments-list-container"),
                        style = "min-height: 40px; max-height: 100px; overflow-y: auto; border: 1px dashed #ddd; padding: 5px; margin-top: 0px; background-color: #fff;",
                        uiOutput(ns("attached_files_list_output")) # Namespacowane wyjście
               )
             )
      )
    ),
    fluidRow(
      style = "padding-left: 10px; padding-right: 10px;",
      column(9,
             # Input tekstowy z namespacowanym ID
             textAreaInput(ns("user_message_input"), label = NULL,
                           placeholder = "Wpisz wiadomość lub pytanie...",
                           rows = 2, resize = "vertical", width = "100%")
      ),
      column(3, style = "padding-top: 5px;",
             # Przycisk "Wyślij" z klasą i data-* atrybutem, bez input ID, ale z namespacowanym ID dla shinyjs
             tags$button(id = ns("send_query_btn"), type = "button", class = "btn btn-primary btn-block send-btn-class", `data-conv-id` = conv_id, "Wyślij")
      )
    )
  )
}
