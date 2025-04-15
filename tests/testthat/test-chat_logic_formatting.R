# tests/testthat/test-chat_logic_formatting.R
# This file tests the internal message preparation logic used by get_assistant_response

library(PacketLLM)

test_that("Standard model (gpt-4o) formatting is correct", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE, title = "Standard Model Test")
  model <- "gpt-4o" # Standard model
  temp <- 0.8
  sys_msg <- "You are a standard test bot."
  attachment_name <- "att1.txt"
  attachment_content <- "Attachment content"

  set_conversation_model(conv_id, model)
  set_conversation_temperature(conv_id, temp)
  set_conversation_system_message(conv_id, sys_msg)
  add_attachment_to_active_conversation(attachment_name, attachment_content)
  add_message_to_active_history(role = "user", content = "User question for standard model")

  # Get current state needed by the internal function
  current_data <- get_conversation_data(conv_id)

  # Call the internal helper function directly using :::
  # Ensure the function exists before calling
  expect_true(exists("prepare_api_messages", where = asNamespace("PacketLLM"), mode = "function"))
  prepared_data <- PacketLLM:::prepare_api_messages(
    conversation_history = current_data$history,
    attachments = current_data$attachments,
    conversation_temp = current_data$temperature,
    conversation_system_message = current_data$system_message,
    conversation_model = current_data$model
  )

  # Checks on the returned list
  expect_type(prepared_data, "list")
  expect_named(prepared_data, c("messages", "temperature"))
  expect_equal(prepared_data$temperature, temp) # Should use the set temperature

  messages <- prepared_data$messages
  expect_equal(length(messages), 2) # System + User

  # Check system message (should include attachment content)
  sys_msg_prepared <- messages[[1]]
  expect_equal(sys_msg_prepared$role, "system")
  expect_true(grepl(sys_msg, sys_msg_prepared$content, fixed = TRUE))
  expect_true(grepl("ATTACHED FILES CONTEXT", sys_msg_prepared$content, fixed = TRUE))
  expect_true(grepl(attachment_name, sys_msg_prepared$content, fixed = TRUE))
  expect_true(grepl(attachment_content, sys_msg_prepared$content, fixed = TRUE))

  # Check user message
  user_msg <- messages[[2]]
  expect_equal(user_msg$role, "user")
  expect_equal(user_msg$content, "User question for standard model")

  reset_history_manager()
})

test_that("Simplified model (o1) formatting is correct", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE, title = "Simplified Model Test")
  # Use a model from simplified_models_list (access via :::)
  simplified_model_name <- PacketLLM:::simplified_models_list[1] # Take the first simplified one
  temp_ignored <- 0.9
  sys_msg_ignored <- "This system message should be ignored."
  attachment_ignored <- "This attachment should be ignored."

  set_conversation_model(conv_id, simplified_model_name)
  set_conversation_temperature(conv_id, temp_ignored)
  set_conversation_system_message(conv_id, sys_msg_ignored)
  add_attachment_to_active_conversation("att_ignored.txt", attachment_ignored)
  add_message_to_active_history(role = "user", content = "User question 1")
  add_message_to_active_history(role = "assistant", content = "Assistant reply 1")
  add_message_to_active_history(role = "system", content = "This system message in history should be ignored.")
  add_message_to_active_history(role = "user", content = "User question 2")

  # Get current state
  current_data <- get_conversation_data(conv_id)

  # Call the internal helper function
  # Ensure the function exists before calling
  expect_true(exists("prepare_api_messages", where = asNamespace("PacketLLM"), mode = "function"))
  prepared_data <- PacketLLM:::prepare_api_messages(
    conversation_history = current_data$history,
    attachments = current_data$attachments,
    conversation_temp = current_data$temperature,
    conversation_system_message = current_data$system_message,
    conversation_model = current_data$model
  )

  # Checks on the returned list
  expect_type(prepared_data, "list")
  expect_named(prepared_data, c("messages", "temperature"))
  # Temperature should be the default for simplified, not the one set
  expect_equal(prepared_data$temperature, 0.5) # Default used in helper

  messages <- prepared_data$messages
  # Should only contain user and assistant roles from history
  expect_equal(length(messages), 3)
  expect_equal(messages[[1]]$role, "user")
  expect_equal(messages[[1]]$content, "User question 1")
  expect_equal(messages[[2]]$role, "assistant")
  expect_equal(messages[[2]]$content, "Assistant reply 1")
  expect_equal(messages[[3]]$role, "user")
  expect_equal(messages[[3]]$content, "User question 2")

  # Check again that no system messages are present
  expect_false(any(sapply(messages, function(m) m$role == "system")))

  reset_history_manager()
})


test_that("Placeholder user message is added when needed for standard model", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE)
  model <- "gpt-4o" # Standard model
  sys_msg <- "System prompt only."

  set_conversation_model(conv_id, model)
  set_conversation_system_message(conv_id, sys_msg)
  # NO user message added after system message

  # Get current state
  current_data <- get_conversation_data(conv_id)

  # Call the internal helper function
  # Ensure the function exists before calling
  expect_true(exists("prepare_api_messages", where = asNamespace("PacketLLM"), mode = "function"))
  prepared_data <- PacketLLM:::prepare_api_messages(
    conversation_history = current_data$history, # History might be empty
    attachments = current_data$attachments,
    conversation_temp = current_data$temperature,
    conversation_system_message = current_data$system_message, # Use the set one
    conversation_model = current_data$model
  )

  # Checks
  expect_type(prepared_data, "list")
  expect_named(prepared_data, c("messages", "temperature"))
  messages <- prepared_data$messages

  expect_equal(length(messages), 2) # System + Placeholder User
  expect_equal(messages[[1]]$role, "system")
  expect_equal(messages[[1]]$content, sys_msg) # Should contain the set system message
  expect_equal(messages[[2]]$role, "user")
  # Check if the user message content is the specific placeholder
  expect_true(grepl("awaiting a response", messages[[2]]$content, ignore.case = TRUE))

  reset_history_manager()
})

test_that("Placeholder user message is added when needed for simplified model", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE)
  # Use a model from simplified_models_list
  simplified_model_name <- PacketLLM:::simplified_models_list[1]
  set_conversation_model(conv_id, simplified_model_name)
  # Add only an assistant message (or leave history empty)
  add_message_to_active_history(role="assistant", content="Only assistant here")


  # Get current state
  current_data <- get_conversation_data(conv_id)

  # Call the internal helper function
  # Ensure the function exists before calling
  expect_true(exists("prepare_api_messages", where = asNamespace("PacketLLM"), mode = "function"))
  prepared_data <- PacketLLM:::prepare_api_messages(
    conversation_history = current_data$history,
    attachments = current_data$attachments,
    conversation_temp = current_data$temperature,
    conversation_system_message = current_data$system_message,
    conversation_model = current_data$model
  )

  # Checks
  expect_type(prepared_data, "list")
  expect_named(prepared_data, c("messages", "temperature"))
  messages <- prepared_data$messages

  # Simplified logic filters history, then adds placeholder if needed
  expect_equal(length(messages), 2) # Assistant + Placeholder User
  expect_equal(messages[[1]]$role, "assistant") # Original assistant msg remains
  expect_equal(messages[[2]]$role, "user")
  # Check if the user message content is the specific placeholder for simplified
  expect_equal(messages[[2]]$content, "(Awaiting response)")

  reset_history_manager()
})
