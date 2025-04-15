# PacketLLM 0.1.0

*   **Initial Release to CRAN.**
*   Provides an RStudio Gadget (`run_llm_chat_app()`) for interactive chat with OpenAI models (e.g., GPT-4o, GPT-4.1).
*   Supports multiple, tabbed conversations.
*   Allows uploading `.R`, `.pdf`, and `.docx` files as context for models supporting attachments.
*   Enables setting the model, temperature, and system message per conversation (before the first message).
*   Features asynchronous API calls to keep the R console responsive.
*   Includes helper functions for managing conversation history and settings (`create_new_conversation`, `get_active_chat_history`, `set_conversation_model`, etc.).
*   Requires an `OPENAI_API_KEY` environment variable for API access.
*   Added basic unit tests using `testthat`.
*   Added `README.md` and an introductory vignette (`vignettes/PacketLLM-introduction.Rmd`).
