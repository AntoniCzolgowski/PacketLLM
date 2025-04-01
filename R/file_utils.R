#' Read file content
#'
#' This function reads the content of a file with the extension .R, .pdf, or .docx
#' and returns it as a single character string.
#' For PDF files, if the `pages` parameter is provided, only the selected pages will be read.
#'
#' @param file_path Character string. Path to the file.
#' @param pages Optional. A numeric vector specifying which pages (for PDF) should be read.
#'
#' @return A character string containing the file content.
#' @export
read_file_content <- function(file_path, pages = NULL) {
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  ext <- tolower(tools::file_ext(file_path))

  if (ext %in% c("r", "R")) {
    # Read R file
    lines <- readLines(file_path, warn = FALSE)
    return(paste(lines, collapse = "\n"))

  } else if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("The 'pdftools' package is required to read PDF files. Please install it.")
    }
    pages_text <- pdftools::pdf_text(file_path)
    if (!is.null(pages)) {
      if (!is.numeric(pages)) {
        stop("The 'pages' parameter must be a numeric vector.")
      }
      valid_pages <- pages[pages >= 1 & pages <= length(pages_text)]
      pages_text <- pages_text[valid_pages]
    }
    return(paste(pages_text, collapse = "\n\n"))

  } else if (ext == "docx") {
    if (!requireNamespace("readtext", quietly = TRUE)) {
      stop("The 'readtext' package is required to read DOCX files. Please install it.")
    }
    doc <- readtext::readtext(file_path)
    return(doc$text)

  } else {
    stop("Unsupported file format: ", ext)
  }
}

#' Parse page range
#'
#' This function processes a character string specifying a page range (e.g., "1-3,5")
#' and returns a numeric vector.
#'
#' @param pages_str Character string specifying pages, e.g., "1-3,5".
#'
#' @return A numeric vector containing the individual pages.
#' @export
parse_pages <- function(pages_str) {
  if (!is.character(pages_str) || length(pages_str) != 1) {
    stop("pages_str must be a single character string.")
  }

  parts <- strsplit(pages_str, ",")[[1]]
  pages <- integer(0)

  for (part in parts) {
    part <- trimws(part)
    if (grepl("-", part)) {
      bounds <- as.integer(strsplit(part, "-")[[1]])
      if (length(bounds) == 2 && !any(is.na(bounds))) {
        pages <- c(pages, seq(bounds[1], bounds[2]))
      } else {
        stop("Invalid range format: ", part)
      }
    } else {
      num <- as.integer(part)
      if (is.na(num)) {
        stop("Invalid number: ", part)
      }
      pages <- c(pages, num)
    }
  }
  unique(sort(pages))
}
