
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PacketLLM

PacketLLM is an AI assistant gadget for RStudio. It helps with code
explanation, code generation, documentation, review, and file-aware
development tasks without turning the workflow into a separate browser
app.

## Highlights

- **RStudio-aware context**: uses selected code, the active source file,
  package metadata, or attached files depending on the selected context
  scope.
- **Responsive gadget UI**: works in the Viewer pane and in a popped-out
  browser window.
- **Code-focused responses**: renders explanations, code blocks, and
  proposed changes in a clean developer-oriented layout with Markdown
  and LaTeX math support.
- **Safe editor actions**: copy is always available; insert and replace
  are enabled only when PacketLLM has a verified RStudio editor target.
- **Restored conversations**: gadget conversations are saved locally so
  you can close PacketLLM, run code in RStudio, and reopen the chat.
- **Model presets**: choose Best, Balanced, or Fast while advanced
  settings reveal the exact model ID.
- **Assistant behavior**: use presets such as Default, Concise,
  Code-focused, Reviewer, or Custom.

## Installation

You can install the development version of PacketLLM from GitHub with:

``` r
# install.packages("remotes")  # Uncomment if you don't have remotes installed
remotes::install_github("AntoniCzolgowski/PacketLLM")
```

## Configuration

PacketLLM currently uses an OpenAI-compatible backend and reads the API
key from the `OPENAI_API_KEY` environment variable. Do not hardcode keys
in scripts or commit them to a repository.

Add the following line to your user-level or project-level .Renviron
file:

``` sh
OPENAI_API_KEY='your_secret_api_key_here'
```

Use `usethis::edit_r_environ()` to open your user `.Renviron`, then
restart R.

## Usage

Launch PacketLLM from the RStudio Addins menu, or run:

``` r
library(PacketLLM)
run_llm_chat_app()
```

In RStudio, PacketLLM can detect the current selection, active source
file, and project metadata. It can copy generated code, insert code at
the captured editor target, or replace a captured selection after
verifying that the target still matches.
