# tests/testthat/test-history_manager.R
# Basic state management tests (no temperature logic)

library(testthat)
library(PacketLLM)

test_that("Conversation creation and deletion works", {
  reset_history_manager()

  expect_equal(length(get_all_conversation_ids()), 0)
  expect_null(get_active_conversation_id())

  conv_id1 <- create_new_conversation(activate = FALSE, title = "Test Create 1")
  expect_type(conv_id1, "character")
  expect_equal(length(get_all_conversation_ids()), 1)
  expect_null(get_active_conversation_id())

  conv_id2 <- create_new_conversation(activate = TRUE, title = "Test Create 2")
  expect_equal(length(get_all_conversation_ids()), 2)
  expect_equal(get_active_conversation_id(), conv_id2)

  expect_equal(get_conversation_title(conv_id1), "Test Create 1")
  expect_equal(get_conversation_title(conv_id2), "Test Create 2")
  expect_null(get_conversation_title("bad-id"))

  expect_true(delete_conversation(conv_id1))
  expect_equal(length(get_all_conversation_ids()), 1)
  expect_equal(get_active_conversation_id(), conv_id2)

  expect_true(delete_conversation(conv_id2))
  expect_equal(length(get_all_conversation_ids()), 0)
  expect_null(get_active_conversation_id())

  expect_false(delete_conversation(conv_id1))

  reset_history_manager()
})

test_that("Setting conversation parameters works", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE)

  initial_data <- get_conversation_data(conv_id)
  expect_type(initial_data$model, "character")
  expect_type(initial_data$system_message, "character")

  # System message
  new_sys_msg <- "You are a test assistant."
  expect_true(set_conversation_system_message(conv_id, new_sys_msg))
  expect_equal(get_conversation_data(conv_id)$system_message, new_sys_msg)

  # Non-existent conversation (expect warning and FALSE)
  expect_warning(
    res_bad <- set_conversation_system_message("bad-id", "test"),
    "Attempting to set system message for non-existent conversation"
  )
  expect_false(res_bad)

  # Model set to a valid option
  avail <- PacketLLM::available_openai_models
  expect_true(length(avail) >= 1)
  if (length(avail) >= 2) {
    target <- avail[2]
  } else {
    target <- avail[1]
  }
  expect_true(set_conversation_model(conv_id, target))
  expect_equal(get_conversation_data(conv_id)$model, target)

  # Invalid model name (expect warning and FALSE)
  expect_warning(
    res_invalid <- set_conversation_model(conv_id, "non-existent-model"),
    "Attempting to set unavailable model"
  )
  expect_false(res_invalid)

  # Non-existent ID (expect warning and FALSE)
  expect_warning(
    res_badid <- set_conversation_model("bad-id", avail[1]),
    "Attempting to set model for non-existent conversation"
  )
  expect_false(res_badid)

  reset_history_manager()
})

test_that("Adding messages and attachments works", {
  reset_history_manager()
  create_new_conversation(activate = TRUE)

  expect_equal(get_active_chat_history(), list())
  expect_equal(get_active_conversation_attachments(), list())

  res_user <- add_message_to_active_history(role = "user", content = "Test message")
  expect_type(res_user, "list")
  expect_true(!is.null(res_user$type))
  expect_equal(length(get_active_chat_history()), 1)
  expect_equal(get_active_chat_history()[[1]]$role, "user")

  expect_true(add_attachment_to_active_conversation("file.txt", "content"))
  expect_equal(length(get_active_conversation_attachments()), 1)
  expect_equal(get_active_conversation_attachments()[[1]]$name, "file.txt")

  # Duplicate name -> expect warning and FALSE
  expect_warning(
    res_dup <- add_attachment_to_active_conversation("file.txt", "new content"),
    "already exists in conversation"
  )
  expect_false(res_dup)
  expect_equal(length(get_active_conversation_attachments()), 1)

  reset_history_manager()
})

test_that("Model locking works correctly", {
  reset_history_manager()
  conv_id <- create_new_conversation(activate = TRUE)
  expect_false(is_conversation_started(conv_id))

  add_message_to_active_history("user", "test user")
  expect_false(is_conversation_started(conv_id))

  add_message_to_active_history("system", "test system")
  expect_false(is_conversation_started(conv_id))

  add_message_to_active_history("assistant", "test assistant")
  expect_true(is_conversation_started(conv_id))

  # After lock, changing model should warn and return FALSE
  avail <- PacketLLM::available_openai_models
  target_after_lock <- if (length(avail) >= 2) avail[2] else avail[1]
  expect_warning(
    res_lock <- set_conversation_model(conv_id, target_after_lock),
    "Cannot change model - conversation has already started"
  )
  expect_false(res_lock)

  reset_history_manager()
})

test_that("persistent history can be restored for gadget sessions", {
  history_file <- tempfile(fileext = ".rds")
  old_path <- getOption("PacketLLM.history_path")
  options(PacketLLM.history_path = history_file)
  on.exit(options(PacketLLM.history_path = old_path), add = TRUE)

  reset_history_manager(clear_persistent = TRUE)
  conv_id <- initialize_history_manager(persist = TRUE)
  add_message_to_active_history("user", "Remember this chat")
  save_result <- PacketLLM:::save_history_manager()
  expect_true(isTRUE(save_result))

  reset_history_manager()
  restored_id <- initialize_history_manager(persist = TRUE)

  expect_equal(restored_id, conv_id)
  expect_equal(length(get_conversation_history(restored_id)), 1)
  expect_equal(get_conversation_history(restored_id)[[1]]$content, "Remember this chat")

  reset_history_manager(clear_persistent = TRUE)
})
