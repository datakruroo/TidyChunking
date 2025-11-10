# Fixed version for PLO-based competency extraction
library(TidyChunking)
library(dplyr)

# Create test data (assuming you have 'test' dataset)
# If you don't have it, uncomment this:
# test <- data.frame(
#   chunk_id = c("1.1", "2.1", "3.1", "4.1", "5.1"),
#   chunk_text = c(
#     "Chapter 1: Introduction",
#     "AI competency framework for teachers...",
#     "Purpose and target audience for AI...", 
#     "Alignment with ICT competency framework...",
#     "Ensuring inclusive digital futures..."
#   ),
#   word_count = c(3, 568, 206, 772, 392),
#   hierarchy = c("Chapter 1", "AI competency", "AI competency", "AI competency", "Chapter 2"),
#   stringsAsFactors = FALSE
# ) %>% as_tibble()

# 🔧 Fix: Convert your prompt STRING to a FUNCTION
my_prompt_function <- function(n_comp, hierarchy, text) {
  paste0(
    'From the following text, please extract "key terms" or "competency components" relevant to undergraduate graduate teachers in elementary and early childhood education programs.\n\n',
    
    'Focus especially on competencies that align with these four main Program Learning Outcomes (PLOs):\n',
    '- PLO 1: Curriculum development, instructional design, teaching and learning innovation, technology integration, and data skills.\n',
    '- PLO 2: Adaptability and change management, professional ethics, lifelong learning, empathy, communication, teamwork, and leadership.\n',
    '- PLO 3: Growth mindset, lifelong learning, self-regulation, emotional intelligence, professional development, collaboration, and well-being.\n',
    '- PLO 4: Inclusive education, global citizenship, strategic/collaborative use of data, and engagement in professional learning communities.\n\n',
    
    'Additionally, please include any competencies that are specific to the field of elementary and early childhood education—such as child development, age-appropriate pedagogy, classroom management for young learners, partnership with families, etc.—even if they are not explicitly mentioned above.\n\n',
    
    'Do not restrict the extraction only to these four PLO dimensions; feel free to extract any other relevant competencies indicated by the text, especially those unique to elementary and early childhood teaching.\n\n',
    
    'For each competency, indicate:\n',
    '- term (key word)\n',
    '- plo (1, 2, 3, 4, or "other" if it does not fit the four PLOs)\n',
    '- category (knowledge, skill, behavior, technology, value, practice, etc.)\n',
    '- brief_definition (short description/definition)\n',
    '- importance (high, medium, low)\n\n',
    
    'Only extract what clearly appears in the content; do not assume or invent.\n',
    'If the text is not related, please return empty array.\n',
    'Return JSON format:\n',
    '[\n',
    '  {\n',
    '    "term": "competency name",\n',
    '    "plo": "1",\n',
    '    "category": "knowledge",\n',
    '    "brief_definition": "definition",\n',
    '    "importance": "high"\n',
    '  }\n',
    ']\n\n',
    
    'Section: ', hierarchy, '\n',
    'Extract ', n_comp, ' competencies from:\n',
    text
  )
}

# 🔧 Fix: Create custom schema that matches your desired output
plo_schema <- tidyllm::tidyllm_schema(
  name = "plo_competency_extraction",
  competencies = tidyllm::field_object(
    .description = "Array of PLO-aligned competencies",
    .vector = TRUE,
    term = tidyllm::field_chr(.description = "Competency term"),
    plo = tidyllm::field_fct(
      .description = "Program Learning Outcome alignment",
      .levels = c("1", "2", "3", "4", "other")
    ),
    category = tidyllm::field_fct(
      .description = "Competency category", 
      .levels = c("knowledge", "skill", "behavior", "technology", "value", "practice", "other")
    ),
    brief_definition = tidyllm::field_chr(.description = "Brief definition"),
    importance = tidyllm::field_fct(
      .description = "Importance level",
      .levels = c("high", "medium", "low")
    )
  )
)

cat("=== Fixed PLO Competency Extraction ===\n")

if (require(tidyllm) && require(jsonlite) && check_openai_setup()$status == "success") {
  
  # ✅ Correct usage: pass FUNCTION and SCHEMA
  word_test <- extract_competencies_tidyllm(
    chunks = test %>% slice(1:5),
    max_per_chunk = 10,
    model = "gpt-4o-mini",  # Use valid model name
    custom_prompt = my_prompt_function,  # ✅ FUNCTION not string
    custom_schema = plo_schema          # ✅ Custom schema
  )
  
  cat("✅ SUCCESS! PLO-aligned competencies extracted:\n")
  print(word_test)
  
  # Show PLO distribution
  if (nrow(word_test) > 0) {
    cat("\n=== PLO Distribution ===\n")
    print(table(word_test$plo))
    
    cat("\n=== Category Distribution ===\n") 
    print(table(word_test$category))
    
    cat("\n=== Sample Results ===\n")
    print(word_test %>% select(term, plo, category, importance) %>% head(8))
  }
  
} else {
  cat("❌ API setup required\n")
}

cat("\n💡 Key fixes:\n")
cat("1. Convert prompt STRING → FUNCTION\n")
cat("2. Use valid model name (gpt-4o-mini not gpt-4.1)\n") 
cat("3. Create custom schema for PLO structure\n")