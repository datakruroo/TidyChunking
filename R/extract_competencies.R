#' Extract Competencies Using TidyLLM
#'
#' Extract data literacy competencies from chunked text using Large Language Models
#' via the tidyllm package. This function processes each chunk to identify relevant
#' skills, knowledge, behaviors, tools, practices, and roles.
#'
#' @param chunks A tibble of chunks from \code{\link{chunk_for_keyword_extraction}}
#' @param max_per_chunk Maximum number of competencies to extract per chunk (default: 15)
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{term}{The competency term or phrase}
#'   \item{category}{Type of competency: "skill", "knowledge", "behavior", "tool", "practice", or "role"}
#'   \item{importance}{Importance level: "high", "medium", or "low"}
#'   \item{definition}{Brief explanation of the competency}
#'   \item{source_chunk}{ID of the source chunk}
#'   \item{source_hierarchy}{Hierarchical path of the source chunk}
#' }
#'
#' @details
#' This function requires the \code{tidyllm} and \code{jsonlite} packages.
#' It uses OpenAI's GPT-4o-mini model by default to extract competencies.
#' 
#' Competency categories:
#' \itemize{
#'   \item \strong{knowledge}: Concepts and theories to understand
#'   \item \strong{skill}: Abilities to perform tasks
#'   \item \strong{behavior}: Mindsets and approaches
#'   \item \strong{tool}: Technologies and software to use
#'   \item \strong{practice}: Methods and procedures to follow
#'   \item \strong{role}: Positions and responsibilities in organizations
#' }
#'
#' The function automatically adjusts the number of competencies to extract
#' based on chunk size (roughly 1 competency per 50 words, capped at max_per_chunk).
#'
#' @examples
#' \dontrun{
#' # Requires tidyllm and jsonlite packages
#' if (require(tidyllm) && require(jsonlite)) {
#'   chunks <- chunk_for_keyword_extraction(markdown_text)
#'   keyword_chunks <- filter_chunks_for_keywords(chunks)
#'   competencies <- extract_competencies_tidyllm(keyword_chunks)
#'   
#'   # View results
#'   head(competencies)
#'   table(competencies$category)
#' }
#' }
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate select filter
#' @importFrom tidyr unnest
#' @importFrom purrr pmap
#' @importFrom stringr str_count
#' @importFrom magrittr %>%
#'
#' @export
extract_competencies_tidyllm <- function(chunks, max_per_chunk = 15) {
  
  # Check for required packages
  if (!requireNamespace("tidyllm", quietly = TRUE)) {
    stop("Package 'tidyllm' is required but not installed. Please install it with: install.packages('tidyllm')")
  }
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required but not installed. Please install it with: install.packages('jsonlite')")
  }
  
  # สร้าง schema ด้วย tidyllm_schema() + field helpers
  competency_schema <- tidyllm::tidyllm_schema(
    name = "competency_extraction",
    # Array of objects ใช้ field_object() กับ .vector = TRUE
    competencies = tidyllm::field_object(
      .description = "Array of extracted competencies",
      .vector = TRUE,  # สำคัญ! บอกว่าเป็น array
      # Define structure ของแต่ละ object
      term = tidyllm::field_chr(.description = "Competency term or phrase"),
      category = tidyllm::field_fct(
        .description = "Type of competency",
        .levels = c("skill", "knowledge", "behavior", "tool", "practice", "role")
      ),
      importance = tidyllm::field_fct(
        .description = "Importance level",
        .levels = c("high", "medium", "low")
      ),
      definition = tidyllm::field_chr(.description = "Brief explanation of competency")
    )
  )
  
  # Extract
  all_competencies <- chunks %>% 
    dplyr::mutate(
      competencies = purrr::pmap(
        list(chunk_text, chunk_id, hierarchy, word_count),
        function(text, id, hier, wc) {
          
          cat("Processing chunk", id, "(", wc, "words)...\n")
          
          n_comp <- ceiling(wc / 50)
          n_comp <- min(n_comp, max_per_chunk)
          n_comp <- max(n_comp, 3)
          
          prompt_text <- paste0(
            'Extract the top ', n_comp, ' data literacy COMPETENCIES from this text.\n\n',
            'A competency is what a data literate person needs to KNOW, DO, or DEMONSTRATE.\n\n',
            'Categories:\n',
            '- knowledge: concepts, theories to understand\n',
            '- skill: abilities to perform\n',
            '- behavior: mindsets and approaches\n',
            '- tool: technologies to use\n',
            '- practice: methods to follow\n',
            '- role: positions in organizations\n\n',
            'Importance: high, medium, low\n\n',
            'Return ONLY a JSON array (no other text):\n',
            '[\n',
            '  {\n',
            '    "term": "data storytelling",\n',
            '    "category": "skill",\n',
            '    "importance": "high",\n',
            '    "definition": "Communicate insights through narrative and visualization"\n',
            '  }\n',
            ']\n\n',
            'Section: ', hier, '\n\n',
            'Text:\n', text
          )
          
          result <- tryCatch({
            
            response <- tidyllm::llm_message(prompt_text) %>% 
              tidyllm::chat(
                tidyllm::openai(.model = "gpt-4o-mini"),
                .json_schema = competency_schema,
                .temperature = 0.3,
                .timeout = 120
              )
            
            reply <- tidyllm::get_reply(response)
            
            if (is.na(reply) || nchar(reply) == 0) {
              return(tibble::tibble())
            }
            
            # Parse JSON
            parsed <- jsonlite::fromJSON(reply)
            comp_data <- parsed$competencies
            
            if (is.data.frame(comp_data) && nrow(comp_data) > 0) {
              comp_data %>% 
                tibble::as_tibble() %>% 
                dplyr::mutate(
                  source_chunk = id,
                  source_hierarchy = hier
                )
            } else {
              tibble::tibble()
            }
            
          }, error = function(e) {
            warning("Chunk ", id, ": ", e$message, call. = FALSE)
            tibble::tibble()
          })
          
          Sys.sleep(1.5)
          return(result)
        },
        .progress = TRUE
      )
    )
  
  # Process
  result_df <- all_competencies %>% 
    dplyr::select(chunk_id, hierarchy, competencies) %>% 
    tidyr::unnest(competencies, keep_empty = TRUE)
  
  if (nrow(result_df) == 0 || !"term" %in% names(result_df)) {
    return(tibble::tibble(
      term = character(),
      category = character(),
      importance = character(),
      definition = character(),
      source_chunk = character(),
      source_hierarchy = character()
    ))
  }
  
  result_df <- result_df %>% dplyr::filter(!is.na(term))
  
  cat("\n✓ Extracted", nrow(result_df), "competencies from",
      length(unique(result_df$source_chunk)), "chunks\n")
  
  return(result_df)
}