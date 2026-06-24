context_mode_choices <- function() {
  c("Auto" = "auto", "Focused" = "focused", "Project" = "project", "None" = "none")
}

rstudio_available <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) &&
    isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))
}

#' Capture RStudio editor and project context
#'
#' Captures lightweight context from the active RStudio editor and project. The
#' function returns a graceful empty context outside RStudio.
#'
#' @param mode One of `auto`, `focused`, `project`, or `none`.
#' @param max_chars Maximum characters to keep from source content.
#' @return A structured list describing the captured context.
#' @export
capture_rstudio_context <- function(mode = "auto", max_chars = 6000) {
  mode <- normalize_context_mode(mode)
  empty_context <- list(
    mode = mode,
    label = "No RStudio context",
    source = "none",
    document_id = NULL,
    path = NULL,
    cursor_position = NULL,
    selection_range = NULL,
    selection_text = "",
    file_excerpt = "",
    project_path = NULL,
    package_metadata = "",
    file_index = character(0),
    available = FALSE
  )

  if (identical(mode, "none") || !rstudio_available()) {
    return(empty_context)
  }

  editor <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  project_path <- tryCatch(rstudioapi::getActiveProject(), error = function(e) NULL)
  package_metadata <- read_package_metadata(project_path)
  file_index <- project_file_index(project_path)

  selection <- first_editor_selection(editor)
  selection_text <- selection$text %||% ""
  selection_range <- selection$range
  document_id <- editor$id %||% NULL
  path <- editor$path %||% NULL
  cursor_position <- cursor_from_selection_range(selection_range)

  contents <- editor$contents %||% character(0)
  file_text <- if (length(contents) > 0) paste(contents, collapse = "\n") else ""
  file_excerpt <- truncate_context_text(file_text, max_chars)
  selection_excerpt <- truncate_context_text(selection_text, max_chars)

  has_selection <- nzchar(trimws(selection_excerpt))
  has_file <- nzchar(trimws(file_excerpt)) || !is.null(path)
  has_project <- !is.null(project_path)

  source <- "none"
  label <- "No RStudio context"

  if (identical(mode, "auto")) {
    if (has_selection) {
      source <- "selection"
      label <- paste("Using selection from", context_path_label(path))
    } else if (has_file) {
      source <- "file"
      label <- paste("Using active file", context_path_label(path))
    } else if (has_project) {
      source <- "project"
      label <- "Using project metadata"
    }
  } else if (identical(mode, "focused")) {
    if (has_selection) {
      source <- "selection"
      label <- paste("Using selection from", context_path_label(path))
    } else if (has_file) {
      source <- "file"
      label <- paste("Using active file", context_path_label(path))
    }
  } else if (identical(mode, "project")) {
    if (has_file && has_project) {
      source <- "project_file"
      label <- paste("Using active file and project metadata")
    } else if (has_file) {
      source <- "file"
      label <- paste("Using active file", context_path_label(path))
    } else if (has_project) {
      source <- "project"
      label <- "Using project metadata"
    }
  }

  list(
    mode = mode,
    label = label,
    source = source,
    document_id = document_id,
    path = path,
    cursor_position = cursor_position,
    selection_range = selection_range,
    selection_text = selection_excerpt,
    file_excerpt = file_excerpt,
    project_path = project_path,
    package_metadata = package_metadata,
    file_index = file_index,
    available = !identical(source, "none")
  )
}

normalize_context_mode <- function(mode) {
  mode <- tolower(mode %||% "auto")
  if (!mode %in% c("auto", "focused", "project", "none")) {
    mode <- "auto"
  }
  mode
}

first_editor_selection <- function(editor) {
  if (is.null(editor) || is.null(editor$selection) || length(editor$selection) == 0) {
    return(list(text = "", range = NULL))
  }
  selection <- editor$selection[[1]]
  list(text = selection$text %||% "", range = selection$range)
}

cursor_from_selection_range <- function(selection_range) {
  if (is.null(selection_range)) {
    return(NULL)
  }
  if (is.list(selection_range) && !is.null(selection_range$end)) {
    return(selection_range$end)
  }
  selection_range
}

context_path_label <- function(path) {
  if (is.null(path) || !nzchar(path)) {
    return("active editor")
  }
  basename(path)
}

truncate_context_text <- function(text, max_chars = 6000) {
  text <- paste(text %||% "", collapse = "\n")
  if (!nzchar(text) || nchar(text, type = "chars") <= max_chars) {
    return(text)
  }
  paste0(substr(text, 1, max_chars), "\n\n[PacketLLM truncated additional context]")
}

read_package_metadata <- function(project_path) {
  if (is.null(project_path) || !nzchar(project_path)) {
    return("")
  }
  desc_path <- file.path(project_path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    return("")
  }
  lines <- readLines(desc_path, warn = FALSE)
  keep <- grepl("^(Package|Title|Description|Imports|Suggests|Depends|License):", lines)
  paste(lines[keep], collapse = "\n")
}

project_file_index <- function(project_path, max_files = 60) {
  if (is.null(project_path) || !nzchar(project_path) || !dir.exists(project_path)) {
    return(character(0))
  }
  files <- list.files(
    project_path,
    pattern = "\\.(R|Rmd|qmd|Rd)$|^(DESCRIPTION|NAMESPACE|README\\.Rmd|README\\.md)$",
    recursive = TRUE,
    full.names = FALSE,
    ignore.case = TRUE
  )
  files <- files[!grepl("(^|/)(\\.git|\\.Rproj\\.user|doc|Meta)(/|$)", files)]
  head(sort(files), max_files)
}

format_rstudio_context_for_prompt <- function(context) {
  if (is.null(context) || !isTRUE(context$available)) {
    return("")
  }

  sections <- character(0)
  sections <- c(sections, paste("Context mode:", context$mode))
  if (!is.null(context$path)) {
    sections <- c(sections, paste("Active file:", context$path))
  }
  if (nzchar(context$selection_text %||% "") && context$source %in% c("selection", "project_file")) {
    sections <- c(sections, paste0("Selected code:\n", context$selection_text))
  } else if (nzchar(context$file_excerpt %||% "") && context$source %in% c("file", "project_file")) {
    sections <- c(sections, paste0("Active file excerpt:\n", context$file_excerpt))
  }
  if (context$source %in% c("project", "project_file")) {
    if (!is.null(context$project_path)) {
      sections <- c(sections, paste("Project:", context$project_path))
    }
    if (nzchar(context$package_metadata %||% "")) {
      sections <- c(sections, paste0("Package metadata:\n", context$package_metadata))
    }
    if (length(context$file_index %||% character(0)) > 0) {
      sections <- c(sections, paste0("Project files:\n", paste(context$file_index, collapse = "\n")))
    }
  }

  paste(sections, collapse = "\n\n")
}

# Helper %||%
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
