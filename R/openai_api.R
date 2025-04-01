# openai_api.R

#' List of available OpenAI models for selection in the UI
#' @export
available_openai_models <- c("gpt-4o", "gpt-4o-mini", "o1", "o3-mini")

#' Models that do not support additional parameters (e.g., temperature)
#' @noRd
simplified_models_list <- c("o1", "o3-mini")

#' Check API Key
#'
#' This function checks if the API key assigned to the `OPENAI_API_KEY` variable
#' exists in the environment variables (e.g., in the .Renviron file).
#' If the key is not set, the function will raise an error and stop execution.
#' If the key is set, it returns TRUE.
#'
#' @return Returns TRUE if the API key is set; otherwise, stops execution.
#' @export
check_api_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(key)) {
    stop("API key missing in .Renviron. Set the OPENAI_API_KEY variable and restart R.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Call OpenAI API
#'
#' Function sends the conversation history to the OpenAI API and returns the model's response.
#' For models in `simplified_models_list`, it does NOT send the `temperature` parameter.
#'
#' @param messages List of messages (each message is a list with 'role' and 'content' fields).
#' @param model OpenAI model to use.
#' @param temperature Temperature parameter (used only for models that support it).
#'
#' @return Character string containing the model's response.
#' @export
call_openai_chat <- function(messages, model, temperature = 0.5) {
  check_api_key()

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The 'httr' package is required for API calls. Please install it.", call. = FALSE)
  }

  url <- "https://api.openai.com/v1/chat/completions"

  # CHANGE: Create the basic payload
  payload <- list(
    model = model,
    messages = messages
    # Without temperature for now
  )

  # CHANGE: Conditionally add temperature only for supported models
  if (!model %in% simplified_models_list) {
    payload$temperature <- temperature
    message(paste("API Call: Including temperature parameter =", temperature, "for model", model))
  } else {
    message(paste("API Call: Omitting temperature parameter for model", model))
  }

  # API call with the prepared payload
  response <- httr::POST(
    url,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
    ),
    body = payload, # Use the prepared payload
    encode = "json",
    httr::timeout(1200)
  )

  # Error handling (no changes)
  if (httr::http_error(response)) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("Error during API call: ", httr::http_status(response)$message, "\nError content: ", error_content, call. = FALSE)
  }

  content_response <- httr::content(response, "parsed")

  if (!is.null(content_response$error)) {
    stop("API Error: ", content_response$error$message, call. = FALSE)
  }

  if (is.null(content_response$choices) || length(content_response$choices) == 0 ||
      is.null(content_response$choices[[1]]$message) || is.null(content_response$choices[[1]]$message$content)) {
    print(content_response)
    stop("Unexpected response structure from OpenAI API.", call. = FALSE)
  }

  return(content_response$choices[[1]]$message$content)
}

# Helper %||% (no changes)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
