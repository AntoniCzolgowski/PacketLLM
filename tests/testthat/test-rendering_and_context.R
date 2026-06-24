test_that("response parser separates text and code blocks", {
  blocks <- PacketLLM:::split_response_blocks("Text\n```r\nx <- 1\n```\nMore")

  expect_equal(length(blocks), 3)
  expect_equal(blocks[[1]]$type, "text")
  expect_equal(blocks[[2]]$type, "code")
  expect_equal(blocks[[2]]$lang, "r")
  expect_equal(blocks[[2]]$content, "x <- 1")
})

test_that("PacketLLM change blocks parse into before and after parts", {
  block <- paste(
    "File: R/example.R",
    "Target: captured selection",
    "Before:",
    "```r",
    "x <- 1",
    "```",
    "After:",
    "```r",
    "x <- 2",
    "```",
    sep = "\n"
  )

  change <- PacketLLM:::parse_packetllm_change(block)
  expect_equal(change$file, "R/example.R")
  expect_equal(change$target, "captured selection")
  expect_equal(change$before, "x <- 1")
  expect_equal(change$after, "x <- 2")
})

test_that("context formatter handles unavailable context", {
  context <- capture_rstudio_context(mode = "none")

  expect_false(context$available)
  expect_equal(PacketLLM:::format_rstudio_context_for_prompt(context), "")
})

test_that("markdown renderer formats common markdown safely", {
  rendered <- htmltools::renderTags(PacketLLM:::render_markdown_blocks("## Title\n\n- **bold** item\n\n`x <- 1`\n\n<script>alert(1)</script>"))
  html <- rendered$html

  expect_true(grepl("<h2>Title</h2>", html, fixed = TRUE))
  expect_true(grepl("<strong>bold</strong>", html, fixed = TRUE))
  expect_true(grepl("packet-inline-code", html, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;alert(1)&lt;/script&gt;", html, fixed = TRUE))
  expect_false(grepl("<script>alert(1)</script>", html, fixed = TRUE))
})

test_that("code cards expose replace when a selection target exists", {
  rendered <- htmltools::renderTags(PacketLLM:::render_code_card(
    "x <- 2",
    lang = "r",
    conv_id = "conv_1",
    action_state = list(can_insert = TRUE, can_replace = TRUE)
  ))

  expect_true(grepl("packet-action-replace", rendered$html, fixed = TRUE))
  expect_true(grepl(">Replace</button>", rendered$html, fixed = TRUE))
})
