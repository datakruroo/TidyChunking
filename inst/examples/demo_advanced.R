# TidyChunking Advanced Demo - Customizable extract_competencies_tidyllm
# =======================================================================

library(TidyChunking)

# Create simple test text
test_text <- paste(
  "# Data Analysis Skills",
  "Data analysis requires statistical knowledge and programming skills.",
  "Excel is used for basic calculations and visualization.",
  "R programming enables advanced data manipulation and modeling.",
  "Communication skills are important for presenting results.",
  sep = "\n"
)

cat("=== TidyChunking Advanced Customization Demo ===\n\n")

# Basic chunking
chunks <- chunk_for_keyword_extraction(test_text)

# Use all chunks since text is short
keyword_chunks <- chunks  # Skip filtering for short demo text

cat("Created", nrow(chunks), "chunks,", nrow(keyword_chunks), "to process\n\n")

if (require(tidyllm) && require(jsonlite)) {
  
  if (check_openai_setup()$status == "success") {
    
    # ================================================
    # Demo 1: Default Teacher Competencies
    # ================================================
    
    cat("=== Demo 1: Default Teacher Competencies ===\n")
    
    # Default usage - teacher focus with gpt-4o-mini
    teacher_comp <- extract_competencies_tidyllm(
      keyword_chunks, 
      max_per_chunk = 3
    )
    
    if (nrow(teacher_comp) > 0) {
      cat("Default extraction results:\n")
      print(teacher_comp[, c("term", "category", "importance")])
    }
    
    # ================================================
    # Demo 2: Different Model
    # ================================================
    
    cat("\n=== Demo 2: Using Different Model ===\n")
    
    # Same default prompt but with GPT-3.5-turbo
    teacher_gpt35 <- extract_competencies_tidyllm(
      keyword_chunks[1, , drop = FALSE],
      max_per_chunk = 2,
      model = "gpt-3.5-turbo"
    )
    
    if (nrow(teacher_gpt35) > 0) {
      cat("GPT-3.5-turbo results:\n")
      print(teacher_gpt35[, c("term", "category")])
    }
    
    # ================================================
    # Demo 3: Custom Business Prompt
    # ================================================
    
    cat("\n=== Demo 3: Custom Business Prompt ===\n")
    
    # Define custom prompt for business skills
    business_prompt_fn <- function(n_comp, hier, text) {
      paste0(
        "Extract ", n_comp, " BUSINESS COMPETENCIES from this text.\n",
        "Categories: technical, analytical, communication, leadership\n",
        "Levels: essential, important, basic\n",
        "JSON format: [{\"skill\": \"name\", \"type\": \"technical\", \"level\": \"essential\"}]\n",
        "Text: ", text
      )
    }
    
    # Custom schema for business
    business_schema <- tidyllm::tidyllm_schema(
      name = "business",
      competencies = tidyllm::field_object(
        .vector = TRUE,
        skill = tidyllm::field_chr(.description = "Business skill"),
        type = tidyllm::field_fct(.levels = c("technical", "analytical", "communication", "leadership")),
        level = tidyllm::field_fct(.levels = c("essential", "important", "basic"))
      )
    )
    
    # Extract with custom prompt and schema
    business_skills <- extract_competencies_tidyllm(
      keyword_chunks[1, , drop = FALSE],
      max_per_chunk = 2,
      model = "gpt-4o-mini",
      custom_prompt = business_prompt_fn,
      custom_schema = business_schema
    )
    
    if (nrow(business_skills) > 0) {
      cat("Custom business extraction:\n")
      print(business_skills[, c("skill", "type", "level")])
    }
    
    # ================================================
    # Demo 4: Minimal Research Skills
    # ================================================
    
    cat("\n=== Demo 4: Simple Research Skills ===\n")
    
    # Very simple custom prompt
    research_prompt <- function(n, h, t) {
      paste("Find", n, "research skills. JSON: [{\"skill\": \"name\"}]. Text:", t)
    }
    
    # Simple schema
    research_schema <- tidyllm::tidyllm_schema(
      name = "research",
      competencies = tidyllm::field_object(
        .vector = TRUE,
        skill = tidyllm::field_chr(.description = "Research skill")
      )
    )
    
    research_skills <- extract_competencies_tidyllm(
      keyword_chunks[1, , drop = FALSE],
      max_per_chunk = 2,
      custom_prompt = research_prompt,
      custom_schema = research_schema
    )
    
    if (nrow(research_skills) > 0) {
      cat("Simple research extraction:\n")
      print(research_skills$skill)
    }
    
    cat("\n=== All demos completed! ===\n")
    cat("Key customization options:\n")
    cat("1. model - Choose different OpenAI models\n") 
    cat("2. custom_prompt - Function(n_comp, hierarchy, text) -> string\n")
    cat("3. custom_schema - tidyllm_schema() for output structure\n")
    
  } else {
    cat("OpenAI API not configured. Set OPENAI_API_KEY in .Renviron\n")
  }
} else {
  cat("Install required packages: install.packages(c('tidyllm', 'jsonlite'))\n")
}