# tests/testthat/test-history_manager.R
# This file tests the basic state management functions from history_manager.R

library(PacketLLM) # Load your package

test_that("Conversation creation and deletion works", {
  # Ensure a clean state at the beginning of the test
  reset_history_manager()

  # Check that there are initially no conversations
  expect_equal(length(get_all_conversation_ids()), 0)
  expect_null(get_active_conversation_id())

  # Create a new conversation
  conv_id1 <- create_new_conversation(activate = FALSE, title = "Test Create 1")
  expect_type(conv_id1, "character") # Check if ID is a string
  expect_equal(length(get_all_conversation_ids()), 1) # Should be 1 conversation
  expect_null(get_active_conversation_id()) # Should not be active

  # Create a second one and activate it
  conv_id2 <- create_new_conversation(activate = TRUE, title = "Test Create 2")
  expect_equal(length(get_all_conversation_ids()), 2) # Should be 2 conversations
  expect_equal(get_active_conversation_id(), conv_id2) # Second one should be active

  # Check title retrieval
  expect_equal(get_conversation_title(conv_id1), "Test Create 1")
  expect_equal(get_conversation_title(conv_id2), "Test Create 2")
  expect_null(get_conversation_title("bad-id")) # Check non-existent ID

  # Delete the first conversation
  expect_true(delete_conversation(conv_id1))
  expect_equal(length(get_all_conversation_ids()), 1) # Should be 1 left
  expect_equal(get_active_conversation_id(), conv_id2) # Active should be unchanged

  # Delete the second (active) one
  expect_true(delete_conversation(conv_id2))
  expect_equal(length(get_all_conversation_ids()), 0) # Should be 0 left
  expect_null(get_active_conversation_id()) # Should be inactive

  # Try deleting a non-existent one
  expect_false(delete_conversation(conv_id1))

  # Reset at the end (good practice)
  reset_history_manager()
})

test_that("Setting conversation parameters works (when not locked)", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE)

  # Check default values (may vary depending on implementation)
  initial_data <- get_conversation_data(conv_id)
  expect_type(initial_data$model, "character")
  expect_type(initial_data$temperature, "double")
  expect_type(initial_data$system_message, "character")

  # Set temperature
  expect_true(set_conversation_temperature(conv_id, 0.9))
  expect_equal(get_conversation_data(conv_id)$temperature, 0.9)
  expect_false(set_conversation_temperature(conv_id, 1.1)) # Out of range
  expect_false(set_conversation_temperature("bad-id", 0.5)) # Non-existent ID

  # Set system message
  new_sys_msg <- "You are a test assistant."
  expect_true(set_conversation_system_message(conv_id, new_sys_msg))
  expect_equal(get_conversation_data(conv_id)$system_message, new_sys_msg)
  expect_false(set_conversation_system_message("bad-id", "test")) # Non-existent ID

  # Set model (assuming 'gpt-4.1' is in the available models list)
  # Ensure the list is available or use a known model from the list
  available_test_model <- PacketLLM::available_openai_models[2] # Take the second available one
  if (length(available_test_model) > 0 && is.character(available_test_model)) {
    expect_true(set_conversation_model(conv_id, available_test_model))
    expect_equal(get_conversation_data(conv_id)$model, available_test_model)
  }
  expect_false(set_conversation_model(conv_id, "non-existent-model")) # Bad model name
  expect_false(set_conversation_model("bad-id", "gpt-4o")) # Non-existent ID

  reset_history_manager()
})

test_that("Adding messages and attachments works", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE)

  # Check initial state of history and attachments
  expect_equal(get_active_chat_history(), list())
  expect_equal(get_active_conversation_attachments(), list())

  # Add user message (should return a list with type='title_set')
  result_user = add_message_to_active_history(role = "user", content = "Test message")
  expect_type(result_user, "list")
  expect_true(!is.null(result_user$type)) # Should be some type
  expect_equal(length(get_active_chat_history()), 1)
  expect_equal(get_active_chat_history()[[1]]$role, "user")

  # Add attachment
  expect_true(add_attachment_to_active_conversation("file.txt", "content"))
  expect_equal(length(get_active_conversation_attachments()), 1)
  expect_equal(get_active_conversation_attachments()[[1]]$name, "file.txt")

  # Try adding the same attachment again (should return FALSE)
  expect_false(add_attachment_to_active_conversation("file.txt", "new content"))
  expect_equal(length(get_active_conversation_attachments()), 1) # Count unchanged

  reset_history_manager()
})

test_that("Model locking works correctly", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE)
  expect_false(is_conversation_started(conv_id)) # Initially not locked

  # Adding user message does not lock
  add_message_to_active_history("user", "test user")
  expect_false(is_conversation_started(conv_id))

  # Adding system message does not lock
  add_message_to_active_history("system", "test system")
  expect_false(is_conversation_started(conv_id))

  # Adding assistant message locks
  add_message_to_active_history("assistant", "test assistant")
  expect_true(is_conversation_started(conv_id))

  # Cannot change model after lock
  expect_false(set_conversation_model(conv_id, "gpt-4o-mini"))

  reset_history_manager()
})
