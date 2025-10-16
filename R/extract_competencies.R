#' Extract Competencies Using TidyLLM
#'
#' Extract data literacy competencies for graduate teachers to implement data-driven 
#' classroom practices from chunked text using Large Language Models via the tidyllm package. 
#' This function processes each chunk to identify relevant skills, knowledge, behaviors, 
#' tools, practices, and roles specifically for educational contexts.
#'
#' @param chunks A tibble of chunks from \code{\link{chunk_for_keyword_extraction}}
#' @param max_per_chunk Maximum number of competencies to extract per chunk (default: 15)
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{term}{The competency term or phrase for teachers}
#'   \item{category}{Type of competency: "skill", "knowledge", "behavior", "tool", "practice", or "role"}
#'   \item{importance}{Importance level for classroom implementation: "high", "medium", or "low"}
#'   \item{definition}{Brief explanation of the competency in educational context}
#'   \item{source_chunk}{ID of the source chunk}
#'   \item{source_hierarchy}{Hierarchical path of the source chunk}
#' }
#'
#' @details
#' This function requires the \code{tidyllm} and \code{jsonlite} packages.
#' It uses OpenAI's GPT-4o-mini model by default to extract competencies.
#' 
#' \strong{API Key Setup:}
#' Before using this function, you need to set up your OpenAI API key. The easiest way is to add it to your \code{.Renviron} file:
#' \preformatted{
#' # Add this line to your ~/.Renviron file
#' OPENAI_API_KEY="your-api-key-here"
#' 
#' # Restart R session after editing .Renviron
#' # You can edit .Renviron with: usethis::edit_r_environ()
#' }
#' 
#' Competency categories for data-driven classroom teaching:
#' \itemize{
#'   \item \strong{knowledge}: Concepts and theories teachers need to understand about data use in education
#'   \item \strong{skill}: Practical abilities teachers need to perform data-related tasks in classroom
#'   \item \strong{behavior}: Mindsets and approaches teachers should adopt for data-driven teaching
#'   \item \strong{tool}: Technologies and instruments teachers use for data collection and analysis
#'   \item \strong{practice}: Methods and procedures teachers follow in data-driven instruction
#'   \item \strong{role}: Responsibilities teachers have in data-driven educational settings
#' }
#'
#' The function focuses on competencies that enable teachers to:
#' \itemize{
#'   \item Use data to improve student learning outcomes
#'   \item Make evidence-based instructional decisions
#'   \item Assess and analyze student performance data
#'   \item Create data-informed learning environments
#' }
#'
#' \strong{Important}: This function extracts only competencies that are explicitly 
#' mentioned or directly implied in the source text. It does not generate new terms 
#' that are not present in the original content, ensuring accuracy and relevance to 
#' the specific document being analyzed.
#'
#' The function automatically adjusts the number of competencies to extract
#' based on chunk size (roughly 1 competency per 50 words, capped at max_per_chunk).
#'
#' @examples
#' \dontrun{
#' # Setup: Add OPENAI_API_KEY to your .Renviron file first
#' # You can use: usethis::edit_r_environ()
#' # Then add: OPENAI_API_KEY="your-api-key-here"
#' # Restart R after editing .Renviron
#' 
#' # Requires tidyllm and jsonlite packages
#' if (require(tidyllm) && require(jsonlite)) {
#'   # Example with educational content
#'   educational_text <- "Teachers need to analyze student assessment data..."
#'   chunks <- chunk_for_keyword_extraction(educational_text)
#'   keyword_chunks <- filter_chunks_for_keywords(chunks)
#'   
#'   # Extract teacher competencies for data-driven classroom
#'   teacher_competencies <- extract_competencies_tidyllm(keyword_chunks)
#'   
#'   # View results focused on teaching
#'   head(teacher_competencies)
#'   table(teacher_competencies$category)
#'   table(teacher_competencies$importance)
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
  
  # Check for API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "" || is.na(api_key)) {
    stop("OpenAI API key not found. Please set OPENAI_API_KEY in your .Renviron file.\n",
         "You can edit .Renviron with: usethis::edit_r_environ()\n",
         "Add this line: OPENAI_API_KEY=\"your-api-key-here\"\n",
         "Then restart R.")
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
            'Extract the top ', n_comp, ' data literacy COMPETENCIES for GRADUATE TEACHERS to implement DATA-DRIVEN CLASSROOM practices from this text.\n\n',
            'IMPORTANT: Only extract competencies that are explicitly mentioned or directly implied in the given text. Do not create new terms that are not present in the source material.\n\n',
            'Focus on competencies that enable teachers to:\n',
            '- Use data to improve student learning outcomes\n',
            '- Make evidence-based instructional decisions\n',
            '- Assess and analyze student performance data\n',
            '- Create data-informed learning environments\n\n',
            'Look for terms, concepts, skills, or practices mentioned in the text that relate to:\n',
            '- Data collection and analysis in educational settings\n',
            '- Assessment and evaluation methods\n',
            '- Teaching strategies based on evidence\n',
            '- Educational technology for data use\n',
            '- Student performance monitoring\n',
            '- Instructional decision-making\n\n',
            'Categories:\n',
            '- knowledge: concepts, theories teachers need to understand about data use in education\n',
            '- skill: practical abilities teachers need to perform data-related tasks in classroom\n',
            '- behavior: mindsets and approaches teachers should adopt for data-driven teaching\n',
            '- tool: technologies and instruments teachers use for data collection and analysis\n',
            '- practice: methods and procedures teachers follow in data-driven instruction\n',
            '- role: responsibilities teachers have in data-driven educational settings\n\n',
            'Importance levels for classroom implementation: high, medium, low\n\n',
            'Extract only terms that appear in or are directly supported by the text content.\n\n',
            'Return ONLY a JSON array (no other text):\n',
            '[\n',
            '  {\n',
            '    "term": "[exact term or phrase from the text]",\n',
            '    "category": "skill",\n',
            '    "importance": "high",\n',
            '    "definition": "[definition based on context in the text]"\n',
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
                .temperature = 0.1,  # ลดลงเพื่อให้ติดกับข้อความมากขึ้น
                .top_p = 0.8,       # ลดลงเพื่อลดการสร้างคำใหม่
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
            # Detailed error reporting for API issues
            error_msg <- e$message
            if (grepl("quota|insufficient_quota|rate_limit", error_msg, ignore.case = TRUE)) {
              warning("Chunk ", id, ": API Quota/Rate Limit Error - ", error_msg, 
                     "\nCheck your OpenAI billing and usage at https://platform.openai.com/account/billing", 
                     call. = FALSE)
            } else if (grepl("api_key|authentication|401", error_msg, ignore.case = TRUE)) {
              warning("Chunk ", id, ": API Key Error - ", error_msg,
                     "\nVerify your OPENAI_API_KEY is correct in .Renviron", 
                     call. = FALSE)
            } else if (grepl("network|timeout|connection", error_msg, ignore.case = TRUE)) {
              warning("Chunk ", id, ": Network Error - ", error_msg,
                     "\nCheck your internet connection", 
                     call. = FALSE)
            } else {
              warning("Chunk ", id, ": ", error_msg, call. = FALSE)
            }
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