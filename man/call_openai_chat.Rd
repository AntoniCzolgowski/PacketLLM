% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openai_api.R
\name{call_openai_chat}
\alias{call_openai_chat}
\title{Call OpenAI API}
\usage{
call_openai_chat(messages, model, temperature = 0.5)
}
\arguments{
\item{messages}{List of messages (each message is a list with 'role' and 'content' fields).}

\item{model}{OpenAI model to use.}

\item{temperature}{Temperature parameter (used only for models that support it).}
}
\value{
Character string containing the model's response.
}
\description{
Function sends the conversation history to the OpenAI API and returns the model's response.
For models in \code{simplified_models_list}, it does NOT send the \code{temperature} parameter.
}
\examples{
\dontrun{
  # This example requires the OPENAI_API_KEY environment variable to be set
  # and requires internet access to reach the OpenAI API.

  # Ensure the API key is set in your environment before running:
  # Sys.setenv(OPENAI_API_KEY = "your_actual_openai_api_key")

  # 1. Define the conversation history
  example_messages <- list(
    list(role = "system", content = "You are a helpful assistant providing concise answers."),
    list(role = "user", content = "What is the main purpose of the `httr` package in R?")
  )

  # 2. Choose a model
  # Use a model available in `available_openai_models`
  # Check `simplified_models_list` to know if temperature is supported
  selected_model <- "gpt-4o-mini" # Supports temperature

  # 3. Call the API
  api_response <- tryCatch({
    call_openai_chat(
      messages = example_messages,
      model = selected_model,
      temperature = 0.7
     )
  }, error = function(e) {
    paste("API call failed:", e$message)
  })

  # 4. Print the response
  print(api_response)

  # Example with a simplified model (omits temperature)
  # selected_model_simple <- "o3-mini" # Does not support temperature
  # api_response_simple <- tryCatch({
  #   call_openai_chat(
  #     messages = example_messages,
  #     model = selected_model_simple
  #     # Temperature argument is ignored internally
  #   )
  # }, error = function(e) {
  #   paste("API call failed:", e$message)
  # })
  # print(api_response_simple)
}
}
