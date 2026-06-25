# Repository Guidelines

## Project Structure & Module Organization

PacketLLM is an R package for an RStudio/Shiny gadget that integrates OpenAI chat models. Source code lives in `R/`, with UI components, gadget server logic, API calls, file parsing, and history management split across focused files. Tests live in `tests/testthat/`, generated docs in `man/`, metadata in `DESCRIPTION` and `NAMESPACE`, and the vignette in `vignettes/PacketLLM-introduction.Rmd`. `README.md` is generated from `README.Rmd`, so edit the `.Rmd` source first.

## Build, Test, and Development Commands

- `Rscript -e "testthat::test_local()"`: run the package test suite locally.
- `Rscript -e "devtools::check()"`: run the standard development check before submitting changes.
- `R CMD check --no-manual .`: run release-style package validation.
- `Rscript -e "roxygen2::roxygenise()"`: regenerate `NAMESPACE` and `man/` files after changing roxygen comments.
- `Rscript -e "rmarkdown::render('README.Rmd')"`: rebuild `README.md` from its source document.
- `Rscript -e "devtools::load_all(); PacketLLM::run_llm_chat_app()"`: load the package and launch the gadget during interactive development.

## Coding Style & Naming Conventions

Use idiomatic R with two-space indentation, `<-` assignment, and concise helpers. Keep exported names in lower snake case, matching `run_llm_chat_app()` and `create_new_conversation()`. Use roxygen2 comments for exported functions and prefer explicit imports in roxygen tags. Keep UI helpers, API calls, file utilities, and history logic in their existing modules.

## Package Maintenance

Preserve CRAN compatibility: avoid platform-specific assumptions, keep checks non-interactive, and maintain clean metadata. Do not change `Version` in `DESCRIPTION` unless the work is explicit release preparation. Regenerate roxygen2 output whenever exported APIs or docs change.

## Testing Guidelines

Tests use `testthat` edition 3. Add tests under `tests/testthat/` with filenames like `test-feature_name.R`. Reset mutable state with `reset_history_manager()` when exercising conversations. Cover message formatting, attachment handling, model-specific behavior, and API error paths. Avoid live `OPENAI_API_KEY` tests unless skipped when the variable is absent.

## Commit & Pull Request Guidelines

Recent history uses short, informal commit messages, but new commits should be clear and imperative, for example `Add attachment parsing tests`. Keep each commit focused and keep diffs small. Pull requests should describe impact, list validation commands, mention docs updates, and link issues when available. Include screenshots or notes for Shiny UI changes.

## Security & Configuration Tips

Never commit or hardcode API keys, tokens, or local `.Renviron` files. PacketLLM expects `OPENAI_API_KEY` from the environment. Keep uploaded-file handling conservative because file contents may be sent as model context.
