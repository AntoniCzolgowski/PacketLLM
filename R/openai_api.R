#' List of available OpenAI models for selection in the UI
#' @export
available_openai_models <- c("gpt-5", "gpt-5-mini", "gpt-5-nano")

#' Models treated as "simplified" (none for GPT-5 family)
#' @noRd
simplified_models_list <- character(0)

#' Check API Key
#'
#' Verifies that OPENAI_API_KEY is set. Stops if missing.
#' @return Invisible TRUE if set.
#' @export
#' @examples
#' \dontrun{
#'   check_api_key()
#' }
check_api_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(key)) {
    stop("API key missing in .Renviron. Set OPENAI_API_KEY and restart R.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Call OpenAI API
#'
#' Sends messages to the OpenAI Chat Completions API and returns the assistant content.
#' Temperature is not sent; models run with API defaults.
#'
#' @param messages List of messages (each a list with 'role' and 'content').
#' @param model OpenAI model to use.
#' @return Character string with assistant reply, or NULL on unexpected response.
#' @export
#' @examples
#' \dontrun{
#'   msgs <- list(
#'     list(role = "system", content = "You are concise."),
#'     list(role = "user", content = "What does httr do?")
#'   )
#'   call_openai_chat(messages = msgs, model = "gpt-5-mini")
#' }
call_openai_chat <- function(messages, model) {
  is_verbose <- function() getOption("PacketLLM.verbose", default = FALSE)

  check_api_key()
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The 'httr' package is required for API calls. Please install it.", call. = FALSE)
  }

  url <- "https://api.openai.com/v1/chat/completions"
  payload <- list(model = model, messages = messages)

  if (is_verbose()) message("Calling OpenAI API endpoint: ", url)

  response <- httr::POST(
    url,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
    ),
    body = payload,
    encode = "json",
    httr::timeout(1200)
  )

  if (is_verbose()) message("API call completed with status: ", httr::status_code(response))

  if (httr::http_error(response)) {
    error_content <- httr::content(response, "text", encoding = "UTF-8")
    stop("Error during API call: ", httr::http_status(response)$message,
         "\nResponse status: ", httr::status_code(response),
         "\nResponse content: ", error_content, call. = FALSE)
  }

  content_response <- httr::content(response, "parsed", encoding = "UTF-8")

  if (!is.null(content_response$error)) {
    error_msg <- content_response$error$message %||% "Unknown API error in response body."
    stop("API Error: ", error_msg, call. = FALSE)
  }

  ok <- !is.null(content_response$choices) &&
    is.list(content_response$choices) &&
    length(content_response$choices) > 0 &&
    !is.null(content_response$choices[[1]]$message) &&
    is.list(content_response$choices[[1]]$message) &&
    !is.null(content_response$choices[[1]]$message$content) &&
    is.character(content_response$choices[[1]]$message$content)

  if (!ok) {
    warning("Unexpected response structure received from OpenAI API.", call. = FALSE)
    if (is_verbose()) utils::str(content_response)
    return(NULL)
  }

  if (is_verbose()) message("Successfully parsed API response.")
  content_response$choices[[1]]$message$content
}

# Helper %||%
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
