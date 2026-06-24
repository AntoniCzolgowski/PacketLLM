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
