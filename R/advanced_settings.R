# advanced_settings.R

#' Tworzy UI dla zawartości modala ustawień zaawansowanych
#'
#' Generuje selektor modelu, suwak temperatury i pole tekstowe
#' dla komunikatu systemowego. Dodaje kontenery dla łatwiejszego
#' zarządzania stanem (enabled/disabled) i miejsca na komunikaty.
#'
#' @param available_models Wektor dostępnych nazw modeli.
#' @param model_value Aktualnie wybrany model (character).
#' @param temp_value Aktualna wartość temperatury (numeric).
#' @param sys_msg_value Aktualna wartość komunikatu systemowego (character).
#' @param model_input_id ID dla selektora modelu.
#' @param temp_input_id ID dla suwaka temperatury.
#' @param msg_input_id ID dla pola komunikatu systemowego.
#'
#' @return Obiekt tagList z elementami UI modala.
#' @noRd
#' @import shiny
create_advanced_settings_modal_ui <- function(available_models,
                                              model_value,
                                              temp_value,
                                              sys_msg_value,
                                              model_input_id = "modal_model",
                                              temp_input_id = "modal_temp",
                                              msg_input_id = "modal_system_message") {
  tagList(
    # Kontener dla selektora modelu i komunikatu o blokadzie
    tags$div(id = "model_selector_container",
             selectInput(model_input_id,
                         label = "Model LLM (dla tej konwersacji):",
                         choices = available_models,
                         selected = model_value,
                         width = "100%"),
             tags$small(id = "model_locked_message", class = "text-muted", style = "display: none;",
                        "Zmiana modelu jest zablokowana po rozpoczęciu rozmowy.") # Domyślnie ukryty
    ),
    # Kontener dla temperatury i komunikatu
    tags$div(id = "temperature_container",
             sliderInput(temp_input_id,
                         label = "Temperatura (kreatywność odpowiedzi):",
                         min = 0, max = 1, value = temp_value, step = 0.1),
             tags$small(id = "temp_disabled_message", class = "text-muted", style = "display: none;",
                        "Temperatura nie jest dostępna dla wybranych modeli (o1, o3-mini).") # Domyślnie ukryty
    ),

    # Kontener dla komunikatu systemowego i komunikatu
    tags$div(id = "system_message_container",
             textAreaInput(msg_input_id,
                           label = "Komunikat systemowy (instrukcje dla modelu w tej konwersacji):",
                           value = sys_msg_value,
                           rows = 5,
                           resize = "vertical",
                           width = "100%",
                           placeholder = "Np. Odpowiadaj zwięźle. Bądź formalny. Udawaj pirata."),
             tags$small(id = "sysmsg_disabled_message", class = "text-muted", style = "display: none;",
                        "Komunikat systemowy nie jest dostępny dla wybranych modeli (o1, o3-mini).") # Domyślnie ukryty
    )
  )
}
