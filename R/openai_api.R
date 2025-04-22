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
#' If the key is set, it returns TRUE invisibly.
#'
#' @return Invisible `TRUE` (`invisible(TRUE)`) if the API key is set.
#'         Otherwise, stops execution with an error.
#' @export
#' @examples
#' \dontrun{
#'   # This function requires the OPENAI_API_KEY environment variable to be set.
#'   # You can check if the key is set using Sys.getenv("OPENAI_API_KEY").
#'   # If the key is set, calling check_api_key() will return TRUE invisibly.
#'   # If the key is NOT set, it will stop execution with an error message.
#'
#'   # Example demonstrating how to handle the potential error if the key is missing:
#'   result <- tryCatch({
#'     check_api_key()
#'     "API key found." # Message if check passes
#'   }, error = function(e) {
#'     # Handle the error if the key is missing
#'     paste("Error:", e$message)
#'   })
#'   print(result)
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
#' @return Character string containing the model's response text, or `NULL` if the
#'         response structure is unexpected. If an API or HTTP error occurs,
#'         the function stops execution with an error message.
#' @export
#' @examples
#' \dontrun{
#'   # This example requires the OPENAI_API_KEY environment variable to be set
#'   # and requires internet access to reach the OpenAI API.
#'   # Before running, ensure the key is set, e.g., using:
#'   # Sys.setenv(OPENAI_API_KEY = "your_actual_openai_api_key")
#'   # Remember to replace "your_actual_openai_api_key" with your real key.
#'
#'   # 1. Define the conversation history
#'   example_messages <- list(
#'     list(role = "system", content = "You are a helpful assistant providing concise answers."),
#'     list(role = "user", content = "What is the main purpose of the 'httr' package in R?")
#'   )
#'
#'   # 2. Choose a model (ensure it's in available_openai_models)
#'   selected_model <- "gpt-4o-mini" # Supports temperature
#'
#'   # 3. Call the API using tryCatch for safety
#'   api_response <- tryCatch({
#'     call_openai_chat(
#'       messages = example_messages,
#'       model = selected_model,
#'       temperature = 0.7
#'      )
#'   }, error = function(e) {
#'     # Handle potential errors (API key missing, network issues, API errors)
#'     paste("API call failed:", e$message)
#'   })
#'
#'   # 4. Print the response (or error message)
#'   print(api_response)
#'
#'   # Example with a simplified model (omits temperature)
#'   selected_model_simple <- "o3-mini" # Does not support temperature
#'
#'   # Check if this model is actually available in your package installation
#'   if(selected_model_simple %in% PacketLLM::available_openai_models) {
#'     api_response_simple <- tryCatch({
#'       call_openai_chat(
#'         messages = example_messages,
#'         model = selected_model_simple
#'         # Temperature argument is ignored internally by the function
#'       )
#'     }, error = function(e) {
#'       paste("API call failed:", e$message)
#'     })
#'     print(api_response_simple)
#'   } else {
#'      # Use message() for console output that can be suppressed if needed
#'      message(paste("Skipping simplified model example:",
#'                     selected_model_simple, "not in available_openai_models."))
#'   }
#' }
call_openai_chat <- function(messages, model, temperature = 0.5) {
  # Option checker helper function
  is_verbose <- function() getOption("PacketLLM.verbose", default = FALSE)

  check_api_key()

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The 'httr' package is required for API calls. Please install it.", call. = FALSE)
  }

  # Safely get simplified models list
  simplified_models_list_local <- c("o1", "o3-mini")
  if (exists("simplified_models_list", where = "package:PacketLLM", inherits = FALSE)) {
    simplified_models_list_local <- get("simplified_models_list", envir = asNamespace("PacketLLM"))
  }

  url <- "https://api.openai.com/v1/chat/completions"

  # Create the basic payload
  payload <- list(
    model = model,
    messages = messages
    # Temperature added conditionally below
  )

  # Conditionally add temperature only for supported models
  if (!model %in% simplified_models_list_local) {
    payload$temperature <- temperature
  }

  # Only use message() if verbose is set
  if (is_verbose()) { message("Calling OpenAI API endpoint: ", url) }

  # API call with the prepared payload
  response <- httr::POST(
    url,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
    ),
    body = payload, # Use the prepared payload
    encode = "json",
    httr::timeout(1200) # Consider making timeout configurable or shorter default?
  )

  # Only use message() if verbose is set
  if (is_verbose()) { message("API call completed with status: ", httr::status_code(response)) }

  # Error handling for HTTP errors
  if (httr::http_error(response)) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("Error during API call: ", httr::http_status(response)$message,
         "\nResponse status: ", httr::status_code(response),
         "\nResponse content: ", error_content, call. = FALSE)
  }

  content_response <- httr::content(response, "parsed", encoding = "UTF-8") # Specify encoding

  # Error handling for errors reported in the JSON response body
  if (!is.null(content_response$error)) {
    error_msg <- content_response$error$message %||% "Unknown API error in response body."
    stop("API Error: ", error_msg, call. = FALSE)
  }

  # Check for expected response structure
  if (is.null(content_response$choices) || !is.list(content_response$choices) || length(content_response$choices) == 0 ||
      is.null(content_response$choices[[1]]$message) || !is.list(content_response$choices[[1]]$message) ||
      is.null(content_response$choices[[1]]$message$content) || !is.character(content_response$choices[[1]]$message$content)) {
    # Use warning instead of print/stop
    warning("Unexpected response structure received from OpenAI API.", call. = FALSE)
    # Only use message() if verbose is set
    if (is_verbose()) {
      message("Unexpected response structure details:")
      utils::str(content_response) # Use str for structure details if verbose
    }
    return(NULL) # Return NULL instead of stopping
  }

  # Only use message() if verbose is set
  if (is_verbose()) { message("Successfully parsed API response.")}

  return(content_response$choices[[1]]$message$content)
}

# Helper %||%
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x # Make slightly more robust
}
