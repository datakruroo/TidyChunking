#' Validate Extracted Competencies
#'
#' Check if extracted competencies are grounded in the source text by verifying
#' that the terms appear in or are closely related to the original chunks.
#'
#' @param competencies A tibble of extracted competencies from \code{\link{extract_competencies_tidyllm}}
#' @param chunks A tibble of original chunks from \code{\link{chunk_for_keyword_extraction}}
#'
#' @return A tibble with additional validation columns:
#' \describe{
#'   \item{text_found}{Whether the exact term appears in the source text}
#'   \item{partial_match}{Whether key words from the term appear in the source text}
#'   \item{confidence}{Confidence score for term grounding (0-1)}
#' }
#'
#' @examples
#' \dontrun{
#' # After extracting competencies
#' competencies <- extract_competencies_tidyllm(chunks)
#' validated <- validate_competencies(competencies, chunks)
#' 
#' # Check validation results
#' table(validated$text_found)
#' summary(validated$confidence)
#' }
#'
#' @importFrom dplyr left_join mutate case_when select
#' @importFrom stringr str_detect str_to_lower str_extract_all str_escape
#' @importFrom purrr map2_lgl
#'
#' @export
validate_competencies <- function(competencies, chunks) {
  
  if (nrow(competencies) == 0) {
    return(competencies)
  }
  
  # Join with source chunks
  validated <- competencies %>% 
    dplyr::left_join(
      chunks %>% dplyr::select(chunk_id, chunk_text), 
      by = c("source_chunk" = "chunk_id")
    ) %>% 
    dplyr::mutate(
      # Check for exact term match
      text_found = purrr::map2_lgl(term, chunk_text, function(t, text) {
        if (is.na(t) || is.na(text)) return(FALSE)
        stringr::str_detect(stringr::str_to_lower(text), 
                           stringr::str_to_lower(stringr::str_escape(t)))
      }),
      
      # Check for partial word matches
      partial_match = purrr::map2_lgl(term, chunk_text, function(t, text) {
        if (is.na(t) || is.na(text)) return(FALSE)
        
        # Extract key words from term (excluding common words)
        common_words <- c("data", "the", "of", "and", "to", "for", "in", "a", "an")
        term_words <- stringr::str_extract_all(stringr::str_to_lower(t), "\\w+")[[1]]
        key_words <- term_words[!term_words %in% common_words & nchar(term_words) > 2]
        
        if (length(key_words) == 0) return(FALSE)
        
        # Check if at least half of key words appear in text
        matches <- sum(stringr::str_detect(stringr::str_to_lower(text), key_words))
        matches >= ceiling(length(key_words) / 2)
      }),
      
      # Calculate confidence score
      confidence = dplyr::case_when(
        text_found ~ 1.0,
        partial_match ~ 0.7,
        stringr::str_detect(stringr::str_to_lower(chunk_text), 
                          stringr::str_to_lower(definition)) ~ 0.5,
        TRUE ~ 0.2
      )
    ) %>% 
    dplyr::select(-chunk_text)  # Remove chunk text for cleaner output
  
  # Print validation summary
  cat("🔍 Competency Validation Summary\n")
  cat("================================\n")
  cat("Total competencies:", nrow(validated), "\n")
  cat("Exact text matches:", sum(validated$text_found), 
      "(", round(100 * sum(validated$text_found) / nrow(validated), 1), "%)\n")
  cat("Partial matches:", sum(validated$partial_match), 
      "(", round(100 * sum(validated$partial_match) / nrow(validated), 1), "%)\n")
  cat("High confidence (>0.7):", sum(validated$confidence > 0.7), 
      "(", round(100 * sum(validated$confidence > 0.7) / nrow(validated), 1), "%)\n")
  cat("Low confidence (<0.5):", sum(validated$confidence < 0.5), 
      "(", round(100 * sum(validated$confidence < 0.5) / nrow(validated), 1), "%)\n\n")
  
  if (sum(validated$confidence < 0.5) > 0) {
    cat("⚠️  Low confidence terms may be AI-generated. Consider reviewing:\n")
    low_conf <- validated[validated$confidence < 0.5, ]
    max_show <- min(5, nrow(low_conf))
    if (max_show > 0) {
      for (i in seq_len(max_show)) {
        cat("  •", low_conf$term[i], "\n")
      }
    }
    cat("\n")
  }
  
  return(validated)
}