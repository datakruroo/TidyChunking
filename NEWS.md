# TidyChunking 0.1.0

## New Features

* Added `chunk_for_keyword_extraction()` for intelligent markdown chunking
* Added `filter_chunks_for_keywords()` for content filtering
* Added `preview_chunks()` for quick chunk overview
* Added `extract_competencies_tidyllm()` for AI-powered competency extraction
* Support for hierarchical markdown structure (headings levels 1-3)
* Automatic content type classification
* Optimal chunk size management with paragraph-aware splitting

## Documentation

* Comprehensive README with Thai and English examples
* Full function documentation with roxygen2
* Basic test suite with testthat

## Dependencies

* Core dependencies: dplyr, tidyr, stringr, purrr, tibble, magrittr
* Optional AI features: tidyllm, jsonlite