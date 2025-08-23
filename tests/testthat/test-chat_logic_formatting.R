# tests/testthat/test-chat_logic_formatting.R
# Tests the internal message preparation logic used by get_assistant_response

library(PacketLLM)

test_that("Standard model (gpt-5) formatting is correct", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE, title = "Standard Model Test")
  model <- "gpt-5"
  sys_msg <- "You are a standard test bot."
  attachment_name <- "att1.txt"
  attachment_content <- "Attachment content"

  set_conversation_model(conv_id, model)
  set_conversation_system_message(conv_id, sys_msg)
  add_attachment_to_active_conversation(attachment_name, attachment_content)
  add_message_to_active_history(role = "user", content = "User question for standard model")

  current_data <- get_conversation_data(conv_id)

  # Call internal helper; be robust to possible legacy 'conversation_temp' arg.
  prep_fn <- getFromNamespace("prepare_api_messages", "PacketLLM")
  args <- list(
    conversation_history       = current_data$history,
    attachments                = current_data$attachments,
    conversation_system_message= current_data$system_message,
    conversation_model         = current_data$model
  )
  if ("conversation_temp" %in% names(formals(prep_fn))) {
    args$conversation_temp <- current_data$temperature
  }
  prepared_data <- do.call(prep_fn, args)

  expect_type(prepared_data, "list")
  expect_true("messages" %in% names(prepared_data))

  messages <- prepared_data$messages
  expect_equal(length(messages), 2) # system + user

  # System message should include attachments block and content
  sys_msg_prepared <- messages[[1]]
  expect_equal(sys_msg_prepared$role, "system")
  expect_true(grepl(sys_msg, sys_msg_prepared$content, fixed = TRUE))
  expect_true(grepl("ATTACHED FILES CONTEXT", sys_msg_prepared$content, fixed = TRUE))
  expect_true(grepl(attachment_name, sys_msg_prepared$content, fixed = TRUE))
  expect_true(grepl(attachment_content, sys_msg_prepared$content, fixed = TRUE))

  # User message unchanged
  user_msg <- messages[[2]]
  expect_equal(user_msg$role, "user")
  expect_equal(user_msg$content, "User question for standard model")

  reset_history_manager()
})

test_that("Placeholder user message is added when needed (no user content yet)", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE)
  model <- "gpt-5"
  sys_msg <- "System prompt only."

  set_conversation_model(conv_id, model)
  set_conversation_system_message(conv_id, sys_msg)
  # No user message

  current_data <- get_conversation_data(conv_id)

  prep_fn <- getFromNamespace("prepare_api_messages", "PacketLLM")
  args <- list(
    conversation_history       = current_data$history,
    attachments                = current_data$attachments,
    conversation_system_message= current_data$system_message,
    conversation_model         = current_data$model
  )
  if ("conversation_temp" %in% names(formals(prep_fn))) {
    args$conversation_temp <- current_data$temperature
  }
  prepared_data <- do.call(prep_fn, args)

  expect_type(prepared_data, "list")
  expect_true("messages" %in% names(prepared_data))
  messages <- prepared_data$messages

  expect_equal(length(messages), 2) # system + placeholder user
  expect_equal(messages[[1]]$role, "system")
  expect_equal(messages[[1]]$content, sys_msg)
  expect_equal(messages[[2]]$role, "user")
  expect_true(grepl("awaiting a response", messages[[2]]$content, ignore.case = TRUE))

  reset_history_manager()
})

test_that("No simplified models are configured", {
  # Internal list should be empty now
  expect_equal(length(PacketLLM:::simplified_models_list), 0)
})

