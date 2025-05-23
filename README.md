
<!-- README.md is generated from README.Rmd. Please edit that file -->

PacketLLM: Interactive OpenAI Chat within RStudio

PacketLLM integrates OpenAI’s powerful language models (such as GPT-4o
and GPT-4.1) directly into your RStudio workflow via an interactive
RStudio Gadget interface.

Stop switching between windows! Leverage state-of-the-art AI to
streamline tasks such as: - Generating R code snippets - Explaining
complex code or concepts - Debugging assistance - Summarizing
documents - Analyzing text content from files - Brainstorming ideas

…all without leaving your familiar RStudio environment.

Key Features

Seamless RStudio Integration:  
Runs as a responsive Shiny Gadget within RStudio’s Viewer pane (can be
popped out into a separate window).

Interactive Chat:  
Engage in natural language conversations with selected OpenAI models.

Multiple Conversations:  
Manage several distinct chat sessions simultaneously using a familiar
tabbed interface.

Contextual File Uploads:  
Provide context to the LLM by uploading local files directly into a
conversation.  
Supported formats: - .R / .R scripts - .pdf documents - .docx Word
documents

Per-Conversation Settings:  
Tailor the AI’s behavior for each chat by adjusting: - The specific
OpenAI model (e.g., gpt-4o, GPT-4.1, gpt-4o-mini). - The temperature
setting to control response creativity (for supported models). - A
custom system message to define the AI’s role or instructions (for
supported models).

Asynchronous Operations:  
API calls are handled asynchronously, ensuring the R console remains
responsive while waiting for the model’s reply.

Model Flexibility:  
Easily switch between available OpenAI chat models before starting a new
conversation.

Installation

You can install the development version of PacketLLM from GitHub with:

``` r
# install.packages("remotes")  # Uncomment if you don't have remotes installed
remotes::install_github("AntoniCzolgowski/PacketLLM")
```

Configuration: OpenAI API Key (Crucial!)

PacketLLM requires an OpenAI API key to communicate with the language
models.

Obtain an API Key:  
If you don’t have one, sign up and get an API key from OpenAI.

Set the Environment Variable:  
The recommended and most secure way to provide the key is by setting the
OPENAI_API_KEY environment variable. PacketLLM will automatically detect
and use it.

Add the following line to your user-level or project-level .Renviron
file:

``` sh
OPENAI_API_KEY='your_secret_api_key_here'
```

Use `usethis::edit_r_environ()` to easily open your .Renviron file.

Important: After editing .Renviron, restart your R session for the
changes to take effect. Never hardcode your API key directly into
scripts or share it publicly.

Basic Usage

Once installed and configured with your API key, simply load the library
and run the main function in your RStudio console:

``` r
library(PacketLLM)

# Launch the PacketLLM Chat Gadget
run_llm_chat_app()
```
