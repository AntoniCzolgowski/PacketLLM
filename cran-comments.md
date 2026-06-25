## Submission

This is a resubmission of PacketLLM as version 0.1.2.

### Changes since 0.1.1

* Add onboarding help modal and reopenable Help button.
* Add automatic editor context polling (replaces manual refresh).
* Fix custom instruction being silently ignored when behavior preset was not Custom.
* Show model ID alongside preset name in the selector.
* Consolidate internal helper; remove reflective `getFromNamespace` lookup.
* Fix vignette `install.packages()` call; add `usethis` and `remotes` to Suggests.

### R CMD check results

0 errors | 0 warnings | 0 notes

Tested on Windows (local) with R 4.5.2.

### Additional notes

* PacketLLM requires an `OPENAI_API_KEY` environment variable for live API calls.
* All examples and tests that require network access or API credentials are wrapped with `\dontrun{}` or skipped when the variable is absent.
