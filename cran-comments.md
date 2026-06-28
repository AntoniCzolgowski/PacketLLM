## Submission

This is a resubmission of PacketLLM as version 0.1.3.

### Changes since 0.1.2

* Add support for attaching `.csv` files. CSV data is converted into a model-friendly representation (dimensions header, column schema with inferred types, and the rows as a Markdown table).
* Detect the CSV field delimiter automatically (comma, semicolon, tab, or pipe) and handle semicolon files with comma decimal marks.
* Truncate large CSV files to the first 1000 rows with an explicit note and shorten over-long cell values.
* Update README, vignette, and in-app help to list `.csv` as a supported attachment format.

### R CMD check results

0 errors | 0 warnings | 0 notes

Tested on Windows (local) with R 4.5.2.

### Additional notes

* PacketLLM requires an `OPENAI_API_KEY` environment variable for live API calls.
* All examples and tests that require network access or API credentials are wrapped with `\dontrun{}` or skipped when the variable is absent.
