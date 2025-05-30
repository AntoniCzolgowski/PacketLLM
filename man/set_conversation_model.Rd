% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/history_menager.R
\name{set_conversation_model}
\alias{set_conversation_model}
\title{Sets the model for the conversation, if it hasn't started}
\usage{
set_conversation_model(id, model_name)
}
\arguments{
\item{id}{Character string. The ID of the conversation.}

\item{model_name}{Character string. The name of the new model (must be one of
\code{PacketLLM::available_openai_models}).}
}
\value{
Logical. \code{TRUE} if the model was successfully set for the conversation.
\code{FALSE} if the conversation does not exist, the conversation has already
started (model is locked), or the \code{model_name} is not valid/available.
}
\description{
Assigns a specified OpenAI language model to a conversation, but only if the
conversation exists and has not yet "started" (i.e., no assistant messages
have been added, \code{is_conversation_started(id)} is \code{FALSE}). The model name
must be one of the available models listed in \code{available_openai_models}.
}
\examples{
# Setup
reset_history_manager()
conv_set_model_id <- create_new_conversation(activate = TRUE)
print(paste("Initial model:", get_conversation_model(conv_set_model_id)))

# Set a new valid model (use a known available model)
# Ensure the model exists in PacketLLM::available_openai_models
target_model <- "gpt-4o-mini" # Assuming this is usually available
if (target_model \%in\% PacketLLM::available_openai_models) {
  result_set <- set_conversation_model(conv_set_model_id, target_model)
  print(paste("Model set successful:", result_set))
  print(paste("Model after set:", get_conversation_model(conv_set_model_id)))
} else {
   message(paste("Skipping set model example: Target model", target_model, "not in list."))
}

# Try setting an invalid model name
result_invalid <- set_conversation_model(conv_set_model_id, "invalid-model-name")
print(paste("Invalid model set successful:", result_invalid)) # FALSE
model_after_invalid <- get_conversation_model(conv_set_model_id) # Unchanged
print(paste("Model after invalid set:", model_after_invalid))

# Simulate conversation start by adding an assistant message
add_message_to_active_history("user", "Lock question")
add_message_to_active_history("assistant", "Lock answer") # This locks it

# Try setting model after lock (should fail)
result_locked <- set_conversation_model(conv_set_model_id, "gpt-4o") # Try setting back
print(paste("Model set after lock successful:", result_locked)) # FALSE
model_after_locked <- get_conversation_model(conv_set_model_id) # Unchanged
print(paste("Model after locked attempt:", model_after_locked))

# Clean up
reset_history_manager()
}
