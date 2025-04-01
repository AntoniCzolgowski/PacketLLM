# gadget_ui_components.R

#' Tworzy UI dla zawartości pojedynczej karty czatu
#'
#' Generuje strukturę HTML dla obszaru historii i dolnego paska wejściowego.
#' Zmieniono ID uiOutput dla listy załączników na `staged_files_list_output`.
#'
#' @param conv_id Unikalny identyfikator konwersacji (używany do namespace'u).
#'
#' @return Obiekt tagList zawierający UI dla karty.
#' @noRd
#' @import shiny
create_tab_content_ui <- function(conv_id) {
  ns <- NS(conv_id) # Tworzymy namespace

  tagList(
    # Kontener historii czatu (bez zmian)
    tags$div(
      class = "chat-history-container-class",
      uiOutput(ns("chat_history_output"))
    ),

    # Wiersz wejściowy (Flexbox)
    tags$div(
      class = "input-action-row",

      # 1. Przycisk Dodaj Plik (+) (bez zmian)
      tags$button(
        id = ns("add_file_btn"),
        type = "button",
        class = "btn btn-default add-file-btn-class",
        `data-conv-id` = conv_id,
        "+"
      ),

      # 2. Kontener na listę załączników (ZMIENIONE ID uiOutput)
      tags$div(
        id = ns("staged-attachments-list-container"), # ID kontenera może zostać
        class = "attachments-container",
        # Zmieniono ID wyjścia UI, aby pasowało do serwera
        uiOutput(ns("staged_files_list_output"))
      ),

      # 3. Pole wprowadzania wiadomości (bez zmian)
      textAreaInput(
        ns("user_message_input"),
        label = NULL,
        placeholder = "Wpisz wiadomość...",
        width = "100%"
      ),

      # 4. Przycisk Wyślij (bez zmian)
      tags$button(
        id = ns("send_query_btn"),
        type = "button",
        class = "btn btn-primary send-btn-class",
        `data-conv-id` = conv_id,
        "Wyślij"
      )
    ) # Koniec tags$div.input-action-row
  ) # Koniec tagList
}
