% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openai_api.R
\name{check_api_key}
\alias{check_api_key}
\title{Check API Key}
\usage{
check_api_key()
}
\value{
Returns TRUE if the API key is set; otherwise, stops execution.
}
\description{
This function checks if the API key assigned to the \code{OPENAI_API_KEY} variable
exists in the environment variables (e.g., in the .Renviron file).
If the key is not set, the function will raise an error and stop execution.
If the key is set, it returns TRUE.
}
\examples{
\dontrun{
  # This function requires the OPENAI_API_KEY environment variable to be set.
  # If the key is set, it will return TRUE invisibly.
  # If the key is NOT set, it will stop execution with an error message.

  # Example when the key is likely set:
  # check_api_key() # Returns TRUE invisibly if key exists

  # Example when the key is likely NOT set (will throw an error):
  # Sys.unsetenv("OPENAI_API_KEY") # Temporarily unset the key for demo
  # tryCatch({
  #   check_api_key()
  # }, error = function(e) {
  #   print(paste("Caught expected error:", e$message))
  # })
}
}
