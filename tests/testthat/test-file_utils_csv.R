test_that("read_file_content renders a comma-separated CSV as Markdown", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("name,age,active",
               "Alice,30,TRUE",
               "Bob,25,FALSE"), tmp)

  out <- read_file_content(tmp)

  expect_type(out, "character")
  expect_length(out, 1)
  # Metadata header reports the dimensions.
  expect_match(out, "2 rows and 3 columns")
  # Column schema lists the columns with inferred types.
  expect_match(out, "- name \\(text\\)")
  expect_match(out, "- age \\(integer\\)")
  expect_match(out, "- active \\(logical\\)")
  # Markdown table header and separator row are present.
  expect_match(out, "| name | age | active |", fixed = TRUE)
  expect_match(out, "| --- | --- | --- |", fixed = TRUE)
  # Data rows are present.
  expect_match(out, "| Alice | 30 | TRUE |", fixed = TRUE)
  expect_match(out, "| Bob | 25 | FALSE |", fixed = TRUE)
})

test_that("read_file_content auto-detects a semicolon delimiter", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("city;population",
               "Warsaw;1800000",
               "Krakow;780000"), tmp)

  out <- read_file_content(tmp)

  expect_match(out, "2 rows and 2 columns")
  expect_match(out, "| city | population |", fixed = TRUE)
  expect_match(out, "| Warsaw | 1800000 |", fixed = TRUE)
})

test_that("read_file_content handles semicolon files with decimal commas", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("item;price",
               "apple;1,50",
               "pear;2,25"), tmp)

  out <- read_file_content(tmp)

  # The comma decimal mark should be read as a numeric column.
  expect_match(out, "- price \\(numeric\\)")
  expect_match(out, "| apple | 1.5 |", fixed = TRUE)
})

test_that("read_file_content escapes pipe characters in CSV cells", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("col1,col2",
               '"a|b",c'), tmp)

  out <- read_file_content(tmp)

  # A literal pipe inside a cell must be escaped so it does not break the table.
  expect_match(out, "a\\|b", fixed = TRUE)
})

test_that("read_file_content truncates large CSV files with a note", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  n <- 1200L
  lines <- c("id,value", paste0(seq_len(n), ",", seq_len(n)))
  writeLines(lines, tmp)

  out <- read_file_content(tmp)

  expect_match(out, sprintf("%d rows and 2 columns", n))
  expect_match(out, "more rows not shown")
  expect_match(out, "truncated to the first 1000 rows")
  # The first row is shown but a row beyond the limit is not.
  expect_match(out, "| 1 | 1 |", fixed = TRUE)
  expect_false(grepl("| 1200 | 1200 |", out, fixed = TRUE))
})

test_that("read_file_content handles an empty CSV file", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(character(0), tmp)

  out <- read_file_content(tmp)
  expect_match(out, "empty CSV file", fixed = TRUE)
})

test_that("read_file_content handles a header-only CSV file", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("a,b,c", tmp)

  out <- read_file_content(tmp)
  expect_match(out, "0 rows and 3 columns")
  expect_match(out, "| a | b | c |", fixed = TRUE)
})

test_that("internal delimiter detector picks the consistent delimiter", {
  expect_equal(PacketLLM:::.detect_csv_delimiter(c("a,b,c", "1,2,3")), ",")
  expect_equal(PacketLLM:::.detect_csv_delimiter(c("a;b;c", "1;2;3")), ";")
  expect_equal(PacketLLM:::.detect_csv_delimiter(c("a\tb", "1\t2")), "\t")
  # Falls back to comma when no delimiter is present.
  expect_equal(PacketLLM:::.detect_csv_delimiter(c("singlecolumn")), ",")
})
