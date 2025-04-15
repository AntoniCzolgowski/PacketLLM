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
#' @examples
#' # --- Example for reading an R file ---
#' # Create a temporary R file
#' temp_r_file <- tempfile(fileext = ".R")
#' writeLines(c("x <- 1", "print(x + 1)"), temp_r_file)
#'
#' # Read the content
#' r_content <- read_file_content(temp_r_file)
#' print(r_content)
#'
#' # Clean up the temporary file
#' unlink(temp_r_file)
#'
#' # --- Example for reading a text file (simulates basic docx/pdf content) ---
#' temp_txt_file <- tempfile(fileext = ".txt")
#' # Simulate a docx by saving as txt and renaming (for example purposes only)
#' temp_docx_sim_file <- sub("\\.txt$", ".docx", temp_txt_file)
#' writeLines(c("This is the first line.", "This is the second line."), temp_txt_file)
#' file.rename(temp_txt_file, temp_docx_sim_file)
#'
#' # Reading requires the 'readtext' package for .docx
#' if (requireNamespace("readtext", quietly = TRUE)) {
#'   docx_content <- tryCatch({
#'     read_file_content(temp_docx_sim_file)
#'   }, error = function(e) {
#'     # Handle cases where readtext might fail even if installed
#'     paste("Could not read simulated DOCX:", e$message)
#'   })
#'   print(docx_content)
#' } else {
#'   print("Skipping DOCX example: 'readtext' package not installed.")
#' }
#'
#' # Clean up
#' unlink(temp_docx_sim_file, force = TRUE) # Use force if rename was tricky
#'
#' # --- Example for PDF (requires pdftools, only run if installed) ---
#' \dontrun{
#' # This part requires the 'pdftools' package and a valid PDF file.
#' # For a runnable example, you might need to include a small sample PDF
#' # in your package's inst/extdata directory and use system.file().
#'
#' # Example assuming you have a PDF at 'path/to/your/file.pdf'
#' # pdf_path <- "path/to/your/file.pdf"
#' # if (file.exists(pdf_path) && requireNamespace("pdftools", quietly = TRUE)) {
#' #   # Read all pages
#' #   pdf_content_all <- read_file_content(pdf_path)
#' #   print(substr(pdf_content_all, 1, 100)) # Print first 100 chars
#' #
#' #   # Read specific pages (e.g., page 1)
#' #   pdf_content_page1 <- read_file_content(pdf_path, pages = 1)
#' #   print(pdf_content_page1)
#' # } else {
#' #   print("Skipping PDF example: 'pdftools' not installed or file not found.")
#' # }
#' }
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
    # For the example, we need to handle the simulated .txt as well
    if (ext == "txt") {
      lines <- readLines(file_path, warn = FALSE)
      return(paste(lines, collapse = "\n"))
    }
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
#' @examples
#' # Example 1: Simple range and single page
#' page_string1 <- "1-3, 5"
#' parsed_pages1 <- parse_pages(page_string1)
#' print(parsed_pages1) # Output: [1] 1 2 3 5
#'
#' # Example 2: Multiple ranges and single pages, with spaces
#' page_string2 <- " 2, 4-6, 9 , 11-12 "
#' parsed_pages2 <- parse_pages(page_string2)
#' print(parsed_pages2) # Output: [1] 2 4 5 6 9 11 12
#'
#' # Example 3: Single number
#' page_string3 <- "10"
#' parsed_pages3 <- parse_pages(page_string3)
#' print(parsed_pages3) # Output: [1] 10
#'
#' # Example 4: Invalid input (will cause an error)
#' page_string_invalid <- "1-3, five"
#' \dontrun{
#' # This will stop with an error message about "five"
#' tryCatch(parse_pages(page_string_invalid), error = function(e) print(e$message))
#' }
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
