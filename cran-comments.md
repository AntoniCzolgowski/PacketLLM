## Submission

This is a submission of PacketLLM (version 0.1.1). Thank you for the review.

### What's changed since 0.1.0

* Models: replace old built-in defaults with model presets backed by `gpt-5.5`, `gpt-5.4`, and `gpt-5.4-mini`.
* RStudio workflow: add editor-aware context capture and Addin registration.
* UI: redesign the gadget with responsive code-focused output and safe copy, insert, and replace actions.
* Safety: render chat content safely rather than interpreting arbitrary model/user text as raw HTML.

## R CMD check results

* Local `devtools::check()` on Windows: 0 ERRORs, 0 WARNINGs, 1 NOTE.
* NOTE: `checking for future file timestamps ... unable to verify current time`.

## Additional notes

* PacketLLM requires an `OPENAI_API_KEY` environment variable for live API calls.
* Examples and tests that would require network access or API credentials are wrapped to avoid CRAN check issues.
