#' Check API Key
#'
#' Verifies that `OPENAI_API_KEY` is set. Stops if missing.
#'
#' @return Invisible TRUE if set.
#' @export
#' @examples
#' \dontrun{
#'   check_api_key()
#' }
check_api_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(key)) {
    stop("API key missing. Set OPENAI_API_KEY in your environment and restart R.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Call the configured AI model
#'
#' Sends messages to the Responses API and returns assistant text.
#'
#' @param messages List of messages, each with `role` and `content`.
#' @param model Model ID to use.
#' @param reasoning_effort Reasoning effort: `low`, `medium`, `high`, or `xhigh`.
#' @param verbosity Output verbosity: `low`, `medium`, or `high`.
#' @param max_output_tokens Optional maximum output tokens.
#' @return Character string with assistant reply, or NULL on unexpected response.
#' @export
#' @examples
#' \dontrun{
#'   msgs <- list(list(role = "user", content = "What does httr do?"))
#'   call_openai_chat(messages = msgs, model = "gpt-5.4-mini")
#' }
call_openai_chat <- function(messages,
                             model,
                             reasoning_effort = "medium",
                             verbosity = "low",
                             max_output_tokens = NA_integer_) {
  is_verbose <- function() getOption("PacketLLM.verbose", default = FALSE)

  check_api_key()
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("The 'httr' package is required for API calls. Please install it.", call. = FALSE)
  }

  url <- "https://api.openai.com/v1/responses"
  payload <- build_responses_payload(
    messages = messages,
    model = model,
    reasoning_effort = reasoning_effort,
    verbosity = verbosity,
    max_output_tokens = max_output_tokens
  )

  if (is_verbose()) message("Calling AI provider endpoint: ", url)

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

  response_text <- extract_response_text(content_response)
  if (is.null(response_text)) {
    warning("Unexpected response structure received from the AI provider.", call. = FALSE)
    if (is_verbose()) utils::str(content_response)
    return(NULL)
  }

  if (is_verbose()) message("Successfully parsed API response.")
  response_text
}

build_responses_payload <- function(messages,
                                    model,
                                    reasoning_effort = "medium",
                                    verbosity = "low",
                                    max_output_tokens = NA_integer_) {
  reasoning_effort <- normalize_choice(reasoning_effort, valid_reasoning_efforts(), "medium")
  verbosity <- normalize_choice(verbosity, valid_verbosity_levels(), "low")

  instructions <- collect_system_messages(messages)
  input_messages <- Filter(function(msg) !identical(msg$role, "system"), messages)
  input_messages <- lapply(input_messages, function(msg) {
    list(role = msg$role %||% "user", content = msg$content %||% "")
  })

  payload <- list(
    model = model,
    input = input_messages,
    instructions = instructions,
    reasoning = list(effort = reasoning_effort),
    text = list(verbosity = verbosity),
    store = FALSE
  )

  if (!is.null(max_output_tokens) && !is.na(max_output_tokens) && max_output_tokens > 0) {
    payload$max_output_tokens <- as.integer(max_output_tokens)
  }

  payload
}

collect_system_messages <- function(messages) {
  system_messages <- vapply(
    Filter(function(msg) identical(msg$role, "system"), messages),
    function(msg) msg$content %||% "",
    character(1)
  )
  paste(system_messages[nzchar(trimws(system_messages))], collapse = "\n\n")
}

extract_response_text <- function(content_response) {
  if (!is.null(content_response$output_text) && is.character(content_response$output_text)) {
    return(content_response$output_text)
  }

  output <- content_response$output
  if (is.null(output) || length(output) == 0) {
    return(NULL)
  }

  pieces <- character(0)
  for (item in output) {
    item_content <- item$content %||% list()
    for (part in item_content) {
      if (!is.null(part$text) && is.character(part$text)) {
        pieces <- c(pieces, part$text)
      }
    }
  }

  if (length(pieces) == 0) {
    return(NULL)
  }
  paste(pieces, collapse = "\n")
}

normalize_choice <- function(value, choices, default) {
  value <- value %||% default
  if (!value %in% choices) {
    return(default)
  }
  value
}
