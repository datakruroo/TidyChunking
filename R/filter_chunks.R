#' Filter Chunks for Keyword Extraction
#'
#' Filters chunks to retain only those suitable for keyword extraction by 
#' removing metadata sections, short content, and other non-informative chunks.
#'
#' @param chunks A tibble of chunks from \code{\link{chunk_for_keyword_extraction}}
#'
#' @return A filtered tibble containing only chunks with:
#' \itemize{
#'   \item content_type of "main_content" or "example"
#'   \item word_count >= 50
#' }
#' The results are arranged by chunk_id in ascending order.
#'
#' @details
#' This function is designed to work with the output of 
#' \code{\link{chunk_for_keyword_extraction}} and removes:
#' \itemize{
#'   \item Metadata sections (references, glossaries, etc.)
#'   \item Table-only content
#'   \item Very short chunks (< 50 words)
#'   \item Other non-substantive content
#' }
#'
#' @examples
#' \dontrun{
#' # First chunk the text
#' chunks <- chunk_for_keyword_extraction(markdown_text)
#' 
#' # Then filter for keyword extraction
#' keyword_chunks <- filter_chunks_for_keywords(chunks)
#' 
#' # View the filtered results
#' preview_chunks(keyword_chunks)
#' }
#'
#' @importFrom dplyr filter arrange
#' @importFrom magrittr %>%
#'
#' @export
filter_chunks_for_keywords <- function(chunks) {
  
  chunks %>% 
    # เอาแต่ main_content และ example
    dplyr::filter(content_type %in% c("main_content", "example")) %>% 
    # ไม่เอาที่สั้นเกินไป
    dplyr::filter(word_count >= 50) %>% 
    # เรียงตามลำดับ
    dplyr::arrange(chunk_id)
}