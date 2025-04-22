# file_utils.R

#' Read file content
#'
#' This function reads the content of a file with the extension .R, .pdf, or .docx
#' and returns it as a single character string. TXT files are also supported.
#' For PDF files, if the `pages` parameter is provided, only the selected pages will be read.
#'
#' @param file_path Character string. Path to the file.
#' @param pages Optional. A numeric vector specifying which pages (for PDF) should be read.
#'
#' @return A character string containing the file content, with pages separated by
#'         double newlines for PDF files. Stops with an error if the file does not
#'         exist, the format is unsupported, or required packages (`pdftools` for PDF,
#'         `readtext` for DOCX) are not installed or if `pages` is not numeric when provided.
#' @export
#' @examples
#' # --- Example for reading an R file ---
#' # Create a temporary R file
#' temp_r_file <- tempfile(fileext = ".R")
#' writeLines(c("x <- 1", "print(x + 1)"), temp_r_file)
#'
#' # Read the content
#' r_content <- tryCatch(read_file_content(temp_r_file), error = function(e) e$message)
#' print(r_content)
#'
#' # Clean up the temporary file
#' unlink(temp_r_file)
#'
#' # --- Example for reading a TXT file ---
#' temp_txt_file <- tempfile(fileext = ".txt")
#' writeLines(c("Line one.", "Second line."), temp_txt_file)
#' txt_content <- tryCatch(read_file_content(temp_txt_file), error = function(e) e$message)
#' print(txt_content)
#' unlink(temp_txt_file)
#'
#' # --- Example for PDF (requires pdftools, only run if installed) ---
#' \dontrun{
#' # This part requires the 'pdftools' package and a valid PDF file.
#' # Provide a path to an actual PDF file to test this functionality.
#' # Replace "path/to/your/sample.pdf" with a real path.
#'
#' pdf_file_path <- "path/to/your/sample.pdf"
#'
#' # Check if pdftools is installed and the file exists
#' if (requireNamespace("pdftools", quietly = TRUE) && file.exists(pdf_file_path)) {
#'
#'   # Example: Read all pages
#'   pdf_content_all <- tryCatch(
#'     read_file_content(pdf_file_path),
#'     error = function(e) paste("Error reading all pages:", e$message)
#'   )
#'   # print(substr(pdf_content_all, 1, 100)) # Print first 100 chars
#'
#'   # Example: Read only page 1
#'   pdf_content_page1 <- tryCatch(
#'     read_file_content(pdf_file_path, pages = 1),
#'     error = function(e) paste("Error reading page 1:", e$message)
#'   )
#'   # print(pdf_content_page1)
#'
#' } else if (!requireNamespace("pdftools", quietly = TRUE)) {
#'   message("Skipping PDF example: 'pdftools' package not installed.")
#' } else {
#'   message("Skipping PDF example: File not found at '", pdf_file_path, "'")
#' }
#' }
#' # Note: Reading DOCX files is also supported if the 'readtext' package
#' # is installed, but a simple runnable example is difficult to create
#' # without including a sample file or complex setup.
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
    # Wrap pdf_text call in tryCatch for robustness
    pages_text <- tryCatch({
      pdftools::pdf_text(file_path)
    }, error = function(e) {
      stop("Failed to extract text from PDF '", file_path, "': ", e$message)
    })

    if (!is.null(pages)) {
      if (!is.numeric(pages)) {
        stop("The 'pages' parameter must be a numeric vector.")
      }
      pages <- as.integer(pages) # Ensure integer pages
      pages <- pages[pages > 0] # Ensure positive pages
      if(length(pages) == 0 && !is.null(attr(pages,"orig_length")) && attr(pages,"orig_length") > 0){
        warning("No valid positive page numbers provided in 'pages' parameter.")
        return("") # Return empty if only invalid pages requested
      }
      valid_pages <- pages[pages <= length(pages_text)]
      if(length(valid_pages) == 0 && length(pages) > 0){
        warning("Specified pages (", paste(pages, collapse=", "), ") are outside the range of the document (1-", length(pages_text), "). Returning empty string.")
        return("")
      } else if (length(valid_pages) < length(pages)) {
        invalid_pages <- setdiff(pages, valid_pages)
        warning("Pages ", paste(invalid_pages, collapse=", "), " are outside the document range (1-", length(pages_text), "). Reading only valid pages: ", paste(valid_pages, collapse=", "))
      }
      pages_text <- pages_text[valid_pages]
    }
    return(paste(pages_text, collapse = "\n\n"))

  } else if (ext == "docx") {
    if (!requireNamespace("readtext", quietly = TRUE)) {
      stop("The 'readtext' package is required to read DOCX files. Please install it.")
    }
    # Ensure readtext returns expected structure
    doc <- tryCatch(readtext::readtext(file_path), error = function(e) {
      stop("Failed to read DOCX file '", file_path, "': ", e$message)
    })
    # Validate structure more carefully
    if (!is.data.frame(doc) || !"text" %in% names(doc) || nrow(doc) == 0) {
      warning("readtext::readtext did not return expected structure for file: ", file_path)
      return("") # Return empty string or handle differently?
    }
    # If multiple rows returned (e.g., track changes?), concatenate text? Assume first row for now.
    if (nrow(doc) > 1) {
      warning("readtext::readtext returned multiple text elements for file: ", file_path, ". Concatenating.")
      return(paste(doc$text, collapse="\n\n"))
    }
    return(doc$text[1])

  } else if (ext == "txt") { # Explicitly handle TXT
    lines <- readLines(file_path, warn = FALSE)
    return(paste(lines, collapse = "\n"))
  } else {
    stop("Unsupported file format: '", ext, "'. Supported formats are R, PDF, DOCX, TXT.")
  }
}

#' Parse page range
#'
#' This function processes a character string specifying a page range (e.g., "1-3,5")
#' and returns a numeric vector containing the individual page numbers, sorted and unique.
#'
#' @param pages_str Character string specifying pages, e.g., "1-3,5".
#'
#' @return A numeric vector containing the unique page numbers specified in the
#'         input string, sorted in ascending order. Returns an empty integer vector
#'         if the input string is empty or contains only whitespace. Stops with an
#'         error if the input `pages_str` is not a single character string or if the
#'         format within the string is invalid (e.g., non-numeric parts, invalid ranges).
#' @export
#' @examples
#' # Example 1: Simple range and single page
#' page_string1 <- "1-3, 5"
#' parsed_pages1 <- parse_pages(page_string1)
#' print(parsed_pages1) # Output: [1] 1 2 3 5
#'
#' # Example 2: Multiple ranges and single pages, with spaces and duplicates
#' page_string2 <- " 2, 4-6, 9 , 11-12, 5 "
#' parsed_pages2 <- parse_pages(page_string2)
#' print(parsed_pages2) # Output: [1] 2 4 5 6 9 11 12 (sorted, unique)
#'
#' # Example 3: Single number
#' page_string3 <- "10"
#' parsed_pages3 <- parse_pages(page_string3)
#' print(parsed_pages3) # Output: [1] 10
#'
#' # Example 4: Empty string input
#' page_string_empty <- ""
#' parsed_pages_empty <- parse_pages(page_string_empty)
#' print(parsed_pages_empty) # Output: integer(0)
#'
#' # Example 5: Invalid input (non-numeric) - demonstrates error handling
#' page_string_invalid <- "1-3, five"
#' \dontrun{
#' # This will stop with an error message about "five"
#' tryCatch(parse_pages(page_string_invalid), error = function(e) print(e$message))
#' }
#'
#' # Example 6: Invalid range format (missing end) - demonstrates error handling
#' page_string_invalid_range <- "1-"
#' \dontrun{
#' # This will stop with an error message about invalid range format
#' tryCatch(parse_pages(page_string_invalid_range), error = function(e) print(e$message))
#' }
#'
#' # Example 7: Invalid range format (start > end) - demonstrates error handling
#' page_string_invalid_order <- "5-3"
#' \dontrun{
#' # This will stop with an error message about invalid range values
#' tryCatch(parse_pages(page_string_invalid_order), error = function(e) print(e$message))
#' }
parse_pages <- function(pages_str) {
  if (!is.character(pages_str) || length(pages_str) != 1) {
    stop("pages_str must be a single character string.")
  }
  # Handle empty or whitespace string
  if (!nzchar(trimws(pages_str))) {
    return(integer(0))
  }

  parts <- strsplit(pages_str, ",")[[1]]
  pages <- integer(0)

  for (part in parts) {
    part <- trimws(part)
    if (!nzchar(part)) next # Skip empty parts resulting from trailing/double commas

    # Check for range format (contains a hyphen)
    if (grepl("-", part, fixed = TRUE)) { # Use fixed=TRUE for hyphen
      bounds_str <- strsplit(part, "-")[[1]]
      # Validate exactly two parts and basic numeric format
      if (length(bounds_str) != 2 || !all(grepl("^\\s*[0-9]+\\s*$", bounds_str))) {
        stop("Invalid range format: '", part, "'. Expected format 'start-end' with positive integers.")
      }
      # Convert to integer safely
      bounds <- suppressWarnings(as.integer(bounds_str))
      # Check for NA conversion or invalid values (non-positive, start > end)
      if (any(is.na(bounds)) || bounds[1] <= 0 || bounds[2] <= 0 || bounds[1] > bounds[2]) {
        stop("Invalid range values in '", part, "'. Start and end must be positive integers, and start <= end.")
      }
      pages <- c(pages, seq.int(bounds[1], bounds[2])) # Use seq.int for efficiency
    } else {
      # Check for single number format
      if (!grepl("^\\s*[0-9]+\\s*$", part)) {
        stop("Invalid page number format: '", part, "'. Expected an integer.")
      }
      # Convert to integer safely
      num <- suppressWarnings(as.integer(part))
      if (is.na(num) || num <= 0) {
        stop("Invalid page number value: '", part, "'. Must be a positive integer.")
      }
      pages <- c(pages, num)
    }
  }
  # Return unique sorted pages
  unique(sort(pages))
}
