% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/history_menager.R
\name{reset_history_manager}
\alias{reset_history_manager}
\title{Resets the entire state of the history manager}
\usage{
reset_history_manager()
}
\value{
Invisible \code{NULL} (\code{invisible(NULL)}). Called for its side effect of
clearing the history state.
}
\description{
Clears all stored conversations, resets the active conversation ID to \code{NULL},
and resets the internal conversation counter used for generating IDs.
Effectively returns the manager to its initial empty state. Optionally prints a message
to the console in interactive sessions.
}
\examples{
# Setup: Initialize and add some data
reset_history_manager() # Ensure clean start
initialize_history_manager()
conv_reset_id <- create_new_conversation(activate = TRUE)
add_message_to_active_history("user", "Message before reset")
# MODIFIED LINE (was too long)
conv_count_before <- length(get_all_conversation_ids()) # Should be 2 initially
print(paste("Conversations before reset:", conv_count_before))
print(paste("Active ID before reset:", get_active_conversation_id())) # ID of conv_reset_id

# Reset the manager. A message might appear in the console if run interactively.
reset_history_manager()

# Verify state after reset
print(paste("Conversations after reset:", length(get_all_conversation_ids()))) # 0
print(paste("Active ID after reset:", get_active_conversation_id())) # NULL

# Note: After reset, you might need to initialize again if needed for subsequent operations
# initialize_history_manager()
}
