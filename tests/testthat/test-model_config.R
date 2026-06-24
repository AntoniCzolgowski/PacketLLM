library(PacketLLM)

test_that("model presets expose expected current choices", {
  presets <- available_model_presets()

  expect_equal(presets$preset, c("Best", "Balanced", "Fast"))
  expect_equal(presets$model, c("gpt-5.5", "gpt-5.4", "gpt-5.4-mini"))
  expect_equal(PacketLLM::available_openai_models, presets$model)
})

test_that("default settings are valid", {
  defaults <- PacketLLM:::default_model_settings()

  expect_true(defaults$model %in% PacketLLM::available_openai_models)
  expect_true(defaults$reasoning_effort %in% PacketLLM:::valid_reasoning_efforts())
  expect_true(defaults$verbosity %in% PacketLLM:::valid_verbosity_levels())
  expect_true(defaults$assistant_behavior %in% PacketLLM:::assistant_behavior_choices())
})
