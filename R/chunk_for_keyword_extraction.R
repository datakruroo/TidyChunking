#' Chunk Markdown Text for Keyword Extraction
#'
#' Intelligently chunks markdown text into meaningful sections optimized for 
#' keyword extraction and text analysis. The function respects markdown 
#' hierarchical structure while maintaining optimal chunk sizes.
#'
#' @param markdown_text Character string containing markdown text to be chunked
#' @param max_words Maximum number of words per chunk (default: 800)
#' @param min_words Minimum number of words per chunk (default: 100)
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{chunk_id}{Unique identifier for each chunk}
#'   \item{chunk_text}{The actual text content of the chunk}
#'   \item{word_count}{Number of words in the chunk}
#'   \item{heading}{The heading text for this section (if any)}
#'   \item{level}{Heading level (1, 2, or 3)}
#'   \item{parent_h1}{Parent level 1 heading}
#'   \item{parent_h2}{Parent level 2 heading}
#'   \item{hierarchy}{Full hierarchical path (e.g., "Chapter 1 > Section A > Subsection 1")}
#'   \item{content_type}{Type of content: "main_content", "example", "metadata", "table_only", or "other"}
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Splits text into lines and identifies markdown headings (# ## ###)
#'   \item Tracks hierarchical structure and parent sections
#'   \item Creates chunk boundaries based on level 2-3 headings
#'   \item Classifies content type (main content, examples, metadata, etc.)
#'   \item Splits oversized chunks while preserving paragraph boundaries
#' }
#'
#' Content is classified as:
#' \itemize{
#'   \item \strong{main_content}: Substantial text content suitable for analysis
#'   \item \strong{example}: Example or case study sections
#'   \item \strong{metadata}: References, glossaries, related research sections
#'   \item \strong{table_only}: Sections containing primarily tables
#'   \item \strong{other}: Content that doesn't fit other categories
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' markdown_text <- "# Chapter 1\n\n## Section A\n\nThis is content...\n\n## Section B\n\nMore content..."
#' chunks <- chunk_for_keyword_extraction(markdown_text)
#' 
#' # With custom parameters
#' chunks <- chunk_for_keyword_extraction(markdown_text, max_words = 500, min_words = 50)
#' 
#' # View chunk structure
#' preview_chunks(chunks)
#' }
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate case_when filter group_by summarise arrange select if_else first bind_rows
#' @importFrom tidyr fill unnest
#' @importFrom stringr str_split str_detect str_remove str_count str_trunc regex
#' @importFrom purrr pmap
#' @importFrom magrittr %>%
#'
#' @export
chunk_for_keyword_extraction <- function(markdown_text, 
                                         max_words = 800,
                                         min_words = 100) {
  
  # Step 1: แยกเป็นบรรทัด
  lines <- tibble::tibble(
    line_num = seq_along(stringr::str_split(markdown_text, "\n")[[1]]),
    line_text = stringr::str_split(markdown_text, "\n")[[1]]
  )
  
  # Step 2: ระบุ headings
  lines <- lines %>% 
    dplyr::mutate(
      # ตรวจจับ heading
      is_heading = stringr::str_detect(line_text, "^#{1,3} [^#]"),
      heading_level = dplyr::case_when(
        stringr::str_detect(line_text, "^# [^#]") ~ 1L,
        stringr::str_detect(line_text, "^## [^#]") ~ 2L,
        stringr::str_detect(line_text, "^### [^#]") ~ 3L,
        TRUE ~ NA_integer_
      ),
      heading_text = dplyr::if_else(
        is_heading,
        stringr::str_remove(line_text, "^#{1,3} "),
        NA_character_
      ),
      # ตรวจจับ page separator
      is_separator = stringr::str_detect(line_text, "^---+$")
    )
  
  # Step 3: Track parent sections
  lines <- lines %>% 
    dplyr::mutate(
      # เก็บ heading แต่ละระดับ
      h1 = dplyr::if_else(heading_level == 1, heading_text, NA_character_),
      h2 = dplyr::if_else(heading_level == 2, heading_text, NA_character_),
      h3 = dplyr::if_else(heading_level == 3, heading_text, NA_character_)
    ) %>% 
    # Forward fill
    tidyr::fill(h1, .direction = "down") %>% 
    tidyr::fill(h2, .direction = "down") %>% 
    tidyr::fill(h3, .direction = "down")
  
  # Step 4: สร้าง chunk boundaries ตาม Level 2-3 headings
  lines <- lines %>% 
    dplyr::mutate(
      # ขึ้น chunk ใหม่เมื่อเจอ heading level 2 หรือ 3
      new_chunk = is_heading & heading_level %in% c(2, 3),
      chunk_id = cumsum(new_chunk | (line_num == 1))
    )
  
  # Step 5: รวม lines เป็น chunks
  chunks <- lines %>% 
    dplyr::group_by(chunk_id) %>% 
    dplyr::summarise(
      chunk_text = paste(line_text, collapse = "\n"),
      heading = dplyr::first(heading_text[is_heading & !is.na(heading_text)]),
      level = dplyr::first(heading_level[!is.na(heading_level)]),
      parent_h1 = dplyr::first(h1[!is.na(h1)]),
      parent_h2 = dplyr::first(h2[!is.na(h2)]),
      start_line = dplyr::first(line_num),
      end_line = dplyr::first(line_num),
      .groups = "drop"
    )
  
  # Step 6: นับคำและ classify content
  chunks <- chunks %>% 
    dplyr::mutate(
      word_count = stringr::str_count(chunk_text, "\\w+"),
      
      # Classify content type
      content_type = dplyr::case_when(
        # Skip metadata sections
        stringr::str_detect(heading, stringr::regex(
          "related research|related priorities|references|acronym|glossary|evidence|gartner recommended",
          ignore_case = TRUE
        )) ~ "metadata",
        
        # Skip pure table sections (ถ้าเป็นตารางเปล่า ๆ)
        stringr::str_count(chunk_text, "\\|") > 20 & word_count < 200 ~ "table_only",
        
        # Example sections
        stringr::str_detect(heading, regex("example|case study", ignore_case = TRUE)) ~ "example",
        
        # Main content (มี text เยอะ)
        word_count >= min_words ~ "main_content",
        
        TRUE ~ "other"
      ),
      
      # สร้าง hierarchy path
      hierarchy = dplyr::case_when(
        !is.na(heading) & level == 2 ~ 
          paste(parent_h1, heading, sep = " > "),
        !is.na(heading) & level == 3 ~ 
          paste(parent_h1, parent_h2, heading, sep = " > "),
        TRUE ~ parent_h1
      )
    )
  
  # Step 7: แบ่ง chunks ที่ยาวเกินไป
  chunks_split <- chunks %>% 
    dplyr::mutate(
      needs_split = word_count > max_words,
      split_chunks = purrr::pmap(
        list(chunk_text, needs_split, word_count, chunk_id, hierarchy),
        function(text, needs_split, wc, id, hier) {
          if (!needs_split) {
            return(tibble::tibble(
              sub_chunk_id = 1,
              sub_chunk_text = text,
              sub_word_count = wc
            ))
          }
          
          # Split by paragraphs
          paragraphs <- stringr::str_split(text, "\n\n+")[[1]]
          
          # Group paragraphs into sub-chunks
          sub_chunks <- list()
          current_chunk <- c()
          current_words <- 0
          sub_id <- 1
          
          for (para in paragraphs) {
            para_words <- stringr::str_count(para, "\\w+")
            
            if (current_words + para_words > max_words && length(current_chunk) > 0) {
              # Save current chunk
              sub_chunks[[sub_id]] <- tibble::tibble(
                sub_chunk_id = sub_id,
                sub_chunk_text = paste(current_chunk, collapse = "\n\n"),
                sub_word_count = current_words
              )
              # Start new chunk
              current_chunk <- c(para)
              current_words <- para_words
              sub_id <- sub_id + 1
            } else {
              current_chunk <- c(current_chunk, para)
              current_words <- current_words + para_words
            }
          }
          
          # Save last chunk
          if (length(current_chunk) > 0) {
            sub_chunks[[sub_id]] <- tibble::tibble(
              sub_chunk_id = sub_id,
              sub_chunk_text = paste(current_chunk, collapse = "\n\n"),
              sub_word_count = current_words
            )
          }
          
          dplyr::bind_rows(sub_chunks)
        }
      )
    ) %>% 
    tidyr::unnest(split_chunks) %>% 
    dplyr::mutate(
      # สร้าง final chunk ID
      final_chunk_id = paste0(chunk_id, ".", sub_chunk_id),
      # ใช้ sub_chunk_text
      final_text = sub_chunk_text,
      final_word_count = sub_word_count
    ) %>% 
    dplyr::select(
      chunk_id = final_chunk_id,
      chunk_text = final_text,
      word_count = final_word_count,
      heading,
      level,
      parent_h1,
      parent_h2,
      hierarchy,
      content_type
    )
  
  return(chunks_split)
}