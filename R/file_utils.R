#' Odczyt zawartości pliku
#'
#' Funkcja odczytuje zawartość pliku o rozszerzeniu .R, .pdf lub .docx i zwraca ją jako pojedynczy łańcuch znaków.
#' W przypadku plików PDF, jeżeli podano parametr \code{pages}, tylko wybrane strony zostaną odczytane.
#'
#' @param file_path Łańcuch znaków. Ścieżka do pliku.
#' @param pages Opcjonalnie. Wektor numeryczny określający, które strony (dla PDF) mają być odczytane.
#'
#' @return Łańcuch znaków zawierający treść pliku.
#' @export
read_file_content <- function(file_path, pages = NULL) {
  if (!file.exists(file_path)) {
    stop("Plik nie został znaleziony: ", file_path)
  }

  ext <- tolower(tools::file_ext(file_path))

  if (ext %in% c("r", "R")) {
    # Odczyt pliku R
    lines <- readLines(file_path, warn = FALSE)
    return(paste(lines, collapse = "\n"))

  } else if (ext == "pdf") {
    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("Pakiet 'pdftools' jest wymagany do odczytu plików PDF. Zainstaluj go.")
    }
    pages_text <- pdftools::pdf_text(file_path)
    if (!is.null(pages)) {
      if (!is.numeric(pages)) {
        stop("Parametr 'pages' musi być wektorem numerycznym.")
      }
      valid_pages <- pages[pages >= 1 & pages <= length(pages_text)]
      pages_text <- pages_text[valid_pages]
    }
    return(paste(pages_text, collapse = "\n\n"))

  } else if (ext == "docx") {
    if (!requireNamespace("readtext", quietly = TRUE)) {
      stop("Pakiet 'readtext' jest wymagany do odczytu plików DOCX. Zainstaluj go.")
    }
    doc <- readtext::readtext(file_path)
    return(doc$text)

  } else {
    stop("Nieobsługiwany format pliku: ", ext)
  }
}

#' Parsowanie zakresu stron
#'
#' Funkcja przetwarza ciąg znaków określający zakres stron (np. "1-3,5") i zwraca wektor numeryczny.
#'
#' @param pages_str Łańcuch znaków określający strony, np. "1-3,5".
#'
#' @return Wektor numeryczny zawierający poszczególne strony.
#' @export
parse_pages <- function(pages_str) {
  if (!is.character(pages_str) || length(pages_str) != 1) {
    stop("pages_str musi być pojedynczym łańcuchem znaków.")
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
        stop("Nieprawidłowy format zakresu: ", part)
      }
    } else {
      num <- as.integer(part)
      if (is.na(num)) {
        stop("Nieprawidłowa liczba: ", part)
      }
      pages <- c(pages, num)
    }
  }
  unique(sort(pages))
}
