% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/history_menager.R
\name{is_conversation_started}
\alias{is_conversation_started}
\title{Checks if the conversation has started (model locked)}
\usage{
is_conversation_started(id)
}
\arguments{
\item{id}{Conversation ID.}
}
\value{
TRUE if the model is locked, FALSE otherwise or if the conversation doesn't exist.
}
\description{
Checks if the \code{model_locked} flag is TRUE. The model is locked
after the first \strong{assistant} message is added.
}
\examples{
# Setup
reset_history_manager()
conv_lock_id <- create_new_conversation(activate = TRUE)

# Check status of a new conversation (should be FALSE)
print(paste("Locked initially:", is_conversation_started(conv_lock_id)))

# Add a user message (does NOT lock the model)
add_message_to_active_history(role = "user", content = "First message")
print(paste("Locked after user msg:", is_conversation_started(conv_lock_id)))

# Add an assistant message using the proper function
# This function correctly handles setting the model_locked flag internally
add_message_to_active_history(role = "assistant", content = "Assistant reply")

# Check status after assistant message (should be TRUE)
print(paste("Locked after assistant msg:", is_conversation_started(conv_lock_id)))

# Check non-existent conversation
print(paste("Locked for non-existent:", is_conversation_started("conv_non_existent")))

# Clean up
delete_conversation(conv_lock_id)
reset_history_manager()
}
