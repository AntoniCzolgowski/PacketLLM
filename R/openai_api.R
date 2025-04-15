# openai_api.R

#' List of available OpenAI models for selection in the UI
#' @export
available_openai_models <- c("gpt-4o", "gpt-4o-mini", "gpt-4.1", "o1", "o3-mini")

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
#' @examples
#' \dontrun{
#'   # This function requires the OPENAI_API_KEY environment variable to be set.
#'   # If the key is set, it will return TRUE invisibly.
#'   # If the key is NOT set, it will stop execution with an error message.
#'
#'   # Example when the key is likely set:
#'   # check_api_key() # Returns TRUE invisibly if key exists
#'
#'   # Example when the key is likely NOT set (will throw an error):
#'   # Sys.unsetenv("OPENAI_API_KEY") # Temporarily unset the key for demo
#'   # tryCatch({
#'   #   check_api_key()
#'   # }, error = function(e) {
#'   #   print(paste("Caught expected error:", e$message))
#'   # })
#' }
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
#' @examples
#' \dontrun{
#'   # This example requires the OPENAI_API_KEY environment variable to be set
#'   # and requires internet access to reach the OpenAI API.
#'
#'   # Ensure the API key is set in your environment before running:
#'   # Sys.setenv(OPENAI_API_KEY = "your_actual_openai_api_key")
#'
#'   # 1. Define the conversation history
#'   example_messages <- list(
#'     list(role = "system", content = "You are a helpful assistant providing concise answers."),
#'     list(role = "user", content = "What is the main purpose of the `httr` package in R?")
#'   )
#'
#'   # 2. Choose a model
#'   # Use a model available in `available_openai_models`
#'   # Check `simplified_models_list` to know if temperature is supported
#'   selected_model <- "gpt-4o-mini" # Supports temperature
#'
#'   # 3. Call the API
#'   api_response <- tryCatch({
#'     call_openai_chat(
#'       messages = example_messages,
#'       model = selected_model,
#'       temperature = 0.7
#'      )
#'   }, error = function(e) {
#'     paste("API call failed:", e$message)
#'   })
#'
#'   # 4. Print the response
#'   print(api_response)
#'
#'   # Example with a simplified model (omits temperature)
#'   # selected_model_simple <- "o3-mini" # Does not support temperature
#'   # api_response_simple <- tryCatch({
#'   #   call_openai_chat(
#'   #     messages = example_messages,
#'   #     model = selected_model_simple
#'   #     # Temperature argument is ignored internally
#'   #   )
#'   # }, error = function(e) {
#'   #   paste("API call failed:", e$message)
#'   # })
#'   # print(api_response_simple)
#' }
call_openai_chat <- function(messages, model, temperature = 0.5) {
  check_api_key()

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The 'httr' package is required for API calls. Please install it.", call. = FALSE)
  }

  url <- "https://api.openai.com/v1/chat/completions"

  # Create the basic payload
  payload <- list(
    model = model,
    messages = messages
    # Without temperature
  )

  # Conditionally add temperature only for supported models
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

  # Error handling
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

# Helper %||%
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
