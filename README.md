
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PacketLLM

> An RStudio gadget that brings an AI assistant directly into your
> development workflow — context-aware, non-blocking, and
> editor-integrated.

[![CRAN
status](https://www.r-pkg.org/badges/version/PacketLLM)](https://CRAN.R-project.org/package=PacketLLM)
[![R-CMD-check](https://github.com/AntoniCzolgowski/PacketLLM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AntoniCzolgowski/PacketLLM/actions)

PacketLLM runs as an RStudio gadget (Viewer pane or pop-out window) and
connects to an OpenAI-compatible backend. Unlike a standalone chat
interface, it automatically captures your active selection, open file,
and package metadata so you can ask questions about your actual code
without copy-pasting anything.

------------------------------------------------------------------------

## Key features

**Context-aware conversations**

The gadget polls the RStudio editor every second and feeds the current
context into every message. Four modes let you control scope:

| Mode    | What the model sees                                           |
|---------|---------------------------------------------------------------|
| Auto    | Selection if present, else active file, else project metadata |
| Focused | Active selection or file only                                 |
| Project | Active file + `DESCRIPTION` + project file list               |
| None    | Only the typed message                                        |

**Non-blocking API calls**

Requests run in a background thread via `future` and `promises`. The R
console stays fully usable while the model is thinking.

**Editor actions**

Each code block in a response exposes three buttons:

- **Copy** — puts the code on the clipboard.
- **Insert** — places the code at the captured cursor position in the
  active source document.
- **Replace** — opens a before/after preview, then validates that the
  editor selection has not changed since it was captured before writing
  the replacement. If it has changed, the action is blocked until
  context refreshes.

**Persistent conversations**

History, settings, and attached file names are written to
`tools::R_user_dir("PacketLLM", "data")` automatically. Closing the
gadget and reopening it — even after restarting RStudio — restores every
conversation exactly as it was.

**Model presets**

Three presets cover common trade-offs between quality and latency:

| Preset   | Model        | Best for                               |
|----------|--------------|----------------------------------------|
| Best     | gpt-5.5      | Complex reasoning, architecture review |
| Balanced | gpt-5.4      | Day-to-day development                 |
| Fast     | gpt-5.4-mini | Quick edits, short questions           |

The model is selectable before the first message in a conversation and
locked after that to keep the context consistent.

**Assistant behavior presets**

| Preset       | Effect                                    |
|--------------|-------------------------------------------|
| Default      | Practical, precise R-focused responses    |
| Concise      | Short explanations, compact code          |
| Code-focused | Runnable code first, minimal prose        |
| Reviewer     | Leads with bugs, risks, and CRAN concerns |
| Custom       | Your own system instruction               |

**File attachments**

Attach `.R`, `.pdf`, `.docx`, or `.txt` files as additional context.
Attached files are stored in the conversation and included in every
subsequent message until the conversation is closed.

**Safe rendering**

Assistant output is parsed into typed blocks (text, code, change cards)
and rendered as structured HTML. User and model content is never
injected as raw HTML. Markdown headings, lists, tables, inline
formatting, and LaTeX math (`\(...\)` / `\[...\]`) are all supported.

------------------------------------------------------------------------

## Installation

``` r
install.packages("PacketLLM")
```

Development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("AntoniCzolgowski/PacketLLM")
```

------------------------------------------------------------------------

## Configuration

PacketLLM reads the API key from the `OPENAI_API_KEY` environment
variable. Add it to your user-level `.Renviron`:

``` r
usethis::edit_r_environ()
```

Then add the line:

``` sh
OPENAI_API_KEY='your_key_here'
```

Restart R. Never hardcode keys in scripts or commit them to a
repository.

------------------------------------------------------------------------

## Usage

Launch from the RStudio Addins menu under **PacketLLM**, or from the
console:

``` r
library(PacketLLM)
run_llm_chat_app()
```

The gadget opens in the Viewer pane. Select some code in the editor,
switch back to the gadget, and the context status label confirms what
was captured. Type a question and press **Send**.

### Typical workflow

1.  Select a function in the editor.
2.  Open PacketLLM (Addins menu or `run_llm_chat_app()`).
3.  Ask a question — no need to paste the code, it is already in
    context.
4.  Click **Replace** on a suggested code block to preview and apply the
    change directly to the editor.

### Multiple conversations

Use **New chat** to open a separate tab. Each tab has its own context
mode, model, attached files, and history. Close tabs with the **x** on
the tab; the last tab cannot be closed.

------------------------------------------------------------------------

## Package API

The public API exposes the history manager and key helpers for scripting
and testing:

``` r
# Conversation lifecycle
initialize_history_manager(persist = TRUE)
create_new_conversation()
delete_conversation(id)
reset_history_manager()

# History
add_message_to_active_history(role, content)
get_active_chat_history()
get_conversation_history(id)

# Settings
set_conversation_model(id, model_name)
set_conversation_system_message(id, message)

# Context
capture_rstudio_context(mode = "auto")

# Files
read_file_content(file_path)
parse_pages(pages_str)
```

------------------------------------------------------------------------

## Requirements

- R \>= 4.1.0
- RStudio (for editor-aware features; the gadget runs outside RStudio
  with reduced functionality)
- An OpenAI API key set as `OPENAI_API_KEY`

**Imports:** `future`, `httr`, `pdftools`, `promises`, `readtext`,
`rstudioapi`, `shiny`, `shinyjs`

------------------------------------------------------------------------

## License

MIT. See `LICENSE` for details.
