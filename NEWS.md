# PacketLLM 0.1.3

* Attachments: add support for `.csv` files. CSV data is converted into a model-friendly representation consisting of a dimensions header, a column schema with inferred types, and the rows rendered as a Markdown table.
* Attachments: the CSV field delimiter is detected automatically (comma, semicolon, tab, or pipe), and semicolon files with comma decimal marks are handled.
* Attachments: large CSV files are truncated to the first 1000 rows with an explicit note; over-long cell values are shortened.
* Docs: update README, vignette, and the in-app help to list `.csv` as a supported attachment format.

# PacketLLM 0.1.2

* UI: add onboarding help modal (shown once on first launch, reopenable via Help button) covering context modes, editor actions, settings, and persistence.
* UI: show model ID alongside preset name in the selector (e.g. "Best - gpt-5.5").
* UI: add visible "Context:" label before the context mode selector.
* UI: render the assistant label in accent color for easier chat scanning.
* UI: grey out the custom instruction field when behavior is not set to Custom.
* Context: replace manual refresh button with automatic 1-second editor polling; context updates silently when selection or file changes.
* Settings: fix custom instruction being silently ignored when behavior preset was not set to Custom.
* Code: consolidate internal `%||%` helper to a single definition.
* Code: replace internal `getFromNamespace` reflective lookup with a direct reference.
* Docs: fix vignette `install.packages()` call (missing quotes); add `usethis` and `remotes` to Suggests.

# PacketLLM 0.1.1

* Models: replace old built-in defaults with model presets (`Best`, `Balanced`, `Fast`) backed by `gpt-5.5`, `gpt-5.4`, and `gpt-5.4-mini`.
* RStudio workflow: add editor-aware context capture, Addin registration, and explicit copy, insert, and replace actions.
* UI/Settings: redesign the gadget with responsive code-focused output, assistant behavior presets, reasoning effort, verbosity, and optional max output controls.
* Safety: render model and user text safely instead of treating arbitrary chat content as raw HTML.
* Rendering: add safe Markdown and LaTeX math formatting for assistant output.
* History: save gadget conversations locally so they can be restored after closing and reopening the gadget.

# PacketLLM 0.1.0

* Initial release to CRAN.
* Provides an RStudio Gadget (`run_llm_chat_app()`) for interactive AI-assisted development.
* Supports multiple, tabbed conversations.
* Allows uploading `.R`, `.pdf`, and `.docx` files as context.
* Enables setting the model and assistant instructions per conversation before the first message.
* Features asynchronous API calls to keep the R console responsive.
* Includes helper functions for managing conversation history and settings (`create_new_conversation`, `get_active_chat_history`, `set_conversation_model`, etc.).
* Requires an `OPENAI_API_KEY` environment variable for API access.
* Added basic unit tests using `testthat`.
* Added `README.md` and an introductory vignette (`vignettes/PacketLLM-introduction.Rmd`).
