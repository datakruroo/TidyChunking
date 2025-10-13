#' Preview Chunk Structure
#'
#' Provides a quick overview of chunk structure, content types, and statistics
#' to help assess the quality of chunking results.
#'
#' @param chunks A tibble of chunks from \code{\link{chunk_for_keyword_extraction}}
#'
#' @return Invisibly returns the input chunks tibble. The function is primarily 
#'   called for its side effects (printing summary information).
#'
#' @details
#' This function displays:
#' \itemize{
#'   \item First 20 chunks with truncated preview text
#'   \item Total number of chunks
#'   \item Count of chunks by content type
#'   \item Word count summary statistics
#' }
#'
#' @examples
#' \dontrun{
#' # Chunk text and preview results
#' chunks <- chunk_for_keyword_extraction(markdown_text)
#' preview_chunks(chunks)
#' 
#' # Preview filtered chunks
#' keyword_chunks <- filter_chunks_for_keywords(chunks)
#' preview_chunks(keyword_chunks)
#' }
#'
#' @importFrom dplyr mutate select count
#' @importFrom stringr str_trunc
#' @importFrom magrittr %>%
#'
#' @export
preview_chunks <- function(chunks) {
  
  preview_data <- chunks %>% 
    dplyr::mutate(
      preview = stringr::str_trunc(chunk_text, 80),
      hierarchy_short = stringr::str_trunc(hierarchy, 60)
    ) %>% 
    dplyr::select(chunk_id, hierarchy_short, word_count, content_type, preview)
  
  print(preview_data, n = 20)
  
  # Summary
  cat("\n=== CHUNK SUMMARY ===\n")
  cat("Total chunks:", nrow(chunks), "\n")
  cat("Content types:\n")
  print(dplyr::count(chunks, content_type))
  cat("\nWord count stats:\n")
  print(summary(chunks$word_count))
  
  invisible(chunks)
}