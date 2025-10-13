#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

# Suppress R CMD check notes about undefined global variables
utils::globalVariables(c(
  "chunk_id", "chunk_text", "word_count", "content_type", "hierarchy",
  "line_num", "line_text", "is_heading", "heading_level", "heading_text",
  "h1", "h2", "h3", "new_chunk", "needs_split", "split_chunks",
  "sub_chunk_id", "sub_chunk_text", "sub_word_count", "final_chunk_id",
  "final_text", "final_word_count", "preview", "hierarchy_short",
  "competencies", "term", "level", "parent_h1", "parent_h2", "start_line",
  "end_line", "is_separator"
))