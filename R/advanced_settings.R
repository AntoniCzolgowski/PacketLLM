# advanced_settings.R

#' Tworzy UI dla zawartości modala ustawień zaawansowanych
#'
#' Generuje suwak temperatury i pole tekstowe dla komunikatu systemowego.
#'
#' @param temp_value Aktualna wartość temperatury (numeric).
#' @param sys_msg_value Aktualna wartość komunikatu systemowego (character).
#' @param temp_input_id ID dla suwaka temperatury.
#' @param msg_input_id ID dla pola komunikatu systemowego.
#'
#' @return Obiekt tagList z elementami UI modala.
#' @noRd
#' @import shiny
create_advanced_settings_modal_ui <- function(temp_value, sys_msg_value, temp_input_id = "modal_temp", msg_input_id = "modal_system_message") {
  tagList(
    sliderInput(temp_input_id,
                label = "Temperatura (kreatywność odpowiedzi):",
                min = 0, max = 1, value = temp_value, step = 0.1),
    textAreaInput(msg_input_id,
                  label = "Komunikat systemowy (instrukcje dla modelu w tej konwersacji):",
                  value = sys_msg_value,
                  rows = 5,
                  resize = "vertical",
                  width = "100%",
                  placeholder = "Np. Odpowiadaj zwięźle. Bądź formalny. Udawaj pirata.")
  )
}
