chat_addin <- function() {
  # Załaduj wymagane pakiety
  library(shiny)
  library(miniUI)

  # Definicja interfejsu użytkownika gadgetu
  ui <- miniPage(
    gadgetTitleBar("Czat LLM", right = miniTitleBarButton("new_chat", "Nowa rozmowa", primary = FALSE)),
    miniContentPanel(
      # Historia czatu (scrollowalna)
      tags$div(
        style = "overflow-y: auto; max-height: 300px; border: 1px solid #ccc; padding: 5px; margin-bottom: 10px;",
        uiOutput("chat_history")
      ),
      # Pole tekstowe i przycisk "Wyślij"
      fluidRow(
        column(9,
               textInput("user_message", label = NULL, placeholder = "Wpisz wiadomość...")
        ),
        column(3,
               actionButton("send", "Wyślij")
        )
      ),
      # Pole wyboru pliku i przycisk "Dołącz plik"
      fluidRow(
        column(9,
               fileInput("file", label = NULL,
                         accept = c(".pdf", ".docx", ".doc", ".R"),
                         buttonLabel = "Wybierz...", placeholder = "Nie wybrano pliku")
        ),
        column(3,
               actionButton("attach", "Dołącz plik")
        )
      ),
      # Przycisk "Zapytaj model"
      fluidRow(
        column(3, offset = 9,
               actionButton("ask", "Zapytaj model")
        )
      )
    )
  )

  # Definicja logiki serwera gadgetu
  server <- function(input, output, session) {
    # Inicjalizacja nowej konwersacji na starcie
    new_chat()
    # Reaktywna zmienna przechowująca bieżącą historię czatu
    conv_history <- reactiveVal(get_chat_history())

    # Renderowanie historii czatu z podziałem na role
    output$chat_history <- renderUI({
      history <- conv_history()
      if (is.null(history) || length(history) == 0) {
        return(tags$em("Historia jest pusta."))
      }
      # Tworzenie czytelnego formatu historii czatu (role: użytkownik/asystent/system)
      if (is.data.frame(history) && all(c("role", "content") %in% names(history))) {
        messages <- mapply(function(role, content) {
          tags$p(
            tags$strong(switch(role,
                               "user" = "Użytkownik: ",
                               "assistant" = "Asystent: ",
                               "system" = "System: ",
                               paste0(role, ": ")
            )),
            tags$span(style = "white-space: pre-wrap;", content)
          )
        }, history$role, history$content, SIMPLIFY = FALSE)
        do.call(tagList, messages)
      } else if (
        is.list(history) &&
        all(sapply(history, function(x) all(c("role", "content") %in% names(x))))
      ) {
        messages <- lapply(history, function(msg) {
          tags$p(
            tags$strong(switch(msg$role,
                               "user" = "Użytkownik: ",
                               "assistant" = "Asystent: ",
                               "system" = "System: ",
                               paste0(msg$role, ": ")
            )),
            tags$span(style = "white-space: pre-wrap;", msg$content)
          )
        })
        do.call(tagList, messages)
      } else if (is.character(history)) {
        # Jeżeli historia to wektor tekstowy lub jednolity ciąg znaków
        tags$pre(paste(history, collapse = "\n"))
      } else {
        # Dla innych formatów na wszelki wypadek
        tags$pre(as.character(history))
      }
    })

    # Obsługa przycisku "Nowa rozmowa"
    observeEvent(input$new_chat, {
      new_chat()
      conv_history(get_chat_history())
      updateTextInput(session, "user_message", value = "")
      # (Opcjonalnie) czyszczenie wyboru pliku, jeśli wymagane
    })

    # Obsługa przycisku "Wyślij" (dodanie wiadomości użytkownika)
    observeEvent(input$send, {
      req(input$user_message)  # Kontynuuj tylko jeśli tekst niepusty
      add_user_message(input$user_message)
      conv_history(get_chat_history())
      updateTextInput(session, "user_message", value = "")
    })

    # Obsługa przycisku "Dołącz plik"
    observeEvent(input$attach, {
      req(input$file)
      file_content <- read_file_content(input$file$datapath)
      attach_file(input$file$name, file_content)
      conv_history(get_chat_history())
    })

    # Obsługa przycisku "Zapytaj model"
    observeEvent(input$ask, {
      get_assistant_response()
      conv_history(get_chat_history())
    })

    # Zamykanie gadgetu po kliknięciu "Done" (przycisk domyślny w pasku tytułu)
    observeEvent(input$done, {
      stopApp()
    })
  }

  # Uruchomienie gadgetu w nowym oknie (popup)
  runGadget(ui, server, viewer = dialogViewer("Czat LLM", width = 600, height = 600))
}
