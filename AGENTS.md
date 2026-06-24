# Repository Guidelines

## Project Structure & Module Organization

PacketLLM is an R package for an RStudio/Shiny gadget that integrates OpenAI chat models. Source code lives in `R/`, with UI components, gadget server logic, OpenAI API calls, file parsing, and conversation history management split across focused files. Tests live in `tests/testthat/`, package documentation is generated into `man/`, package metadata is in `DESCRIPTION` and `NAMESPACE`, and the user-facing vignette is in `vignettes/PacketLLM-introduction.Rmd`. `README.md` is generated from `README.Rmd`, so edit the `.Rmd` source first.

## Build, Test, and Development Commands

- `Rscript -e "testthat::test_local()"`: run the package test suite locally.
- `R CMD check --no-manual .`: perform the main package validation used before release.
- `Rscript -e "roxygen2::roxygenise()"`: regenerate `NAMESPACE` and `man/` files after changing roxygen comments.
- `Rscript -e "rmarkdown::render('README.Rmd')"`: rebuild `README.md` from its source document.
- `Rscript -e "devtools::load_all(); PacketLLM::run_llm_chat_app()"`: load the package and launch the gadget during interactive development.

## Coding Style & Naming Conventions

Use idiomatic R with two-space indentation, `<-` assignment, and concise helper functions. Keep exported function names in lower snake case, matching existing names such as `run_llm_chat_app()` and `create_new_conversation()`. Add roxygen2 comments for exported functions and prefer explicit imports in roxygen tags over broad namespace usage. Keep Shiny UI helpers, API calls, file utilities, and history logic in their existing modules instead of mixing responsibilities.

## Testing Guidelines

Tests use `testthat` edition 3. Add tests under `tests/testthat/` using filenames like `test-feature_name.R`. Prefer isolated tests that reset mutable package state with `reset_history_manager()` when exercising conversations. Cover message formatting, attachment handling, model-specific behavior, and error paths for API-facing helpers. Avoid tests that require a live `OPENAI_API_KEY` unless they are explicitly skipped when the variable is absent.

## Commit & Pull Request Guidelines

Recent history uses short, informal commit messages, but new commits should be clear and imperative, for example `Add attachment parsing tests`. Keep each commit focused on one change. Pull requests should describe the user-visible impact, list validation commands run, mention documentation updates, and link related GitHub issues when available. Include screenshots or notes for changes to the Shiny gadget UI.

## Security & Configuration Tips

Never commit API keys or local `.Renviron` files. PacketLLM expects `OPENAI_API_KEY` from the environment. Keep uploaded-file handling conservative because file contents may be sent as model context.
