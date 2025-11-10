# ตัวอย่างการปรับแต่ง extract_competencies_tidyllm
# ===================================================

library(TidyChunking)

# ข้อมูลทดสอบ
text <- "Data analysis requires statistical knowledge and R programming skills. Excel is used for visualization. Communication skills help present findings to stakeholders."

# 1. สร้าง chunks
chunks <- chunk_for_keyword_extraction(text)

if (require(tidyllm) && require(jsonlite) && check_openai_setup()$status == "success") {
  
  cat("=== 1. Default (Teacher Competencies) ===\n")
  default_result <- extract_competencies_tidyllm(chunks, max_per_chunk = 3)
  if (nrow(default_result) > 0) {
    print(default_result[, c("term", "category", "importance")])
  } else {
    cat("No competencies extracted\n")
  }
  
  cat("\n=== 2. Different Model (GPT-4o) ===\n")
  gpt4o_result <- extract_competencies_tidyllm(chunks, max_per_chunk = 3, model = "gpt-4o")
  if (nrow(gpt4o_result) > 0) {
    print(gpt4o_result[1:min(3, nrow(gpt4o_result)), c("term", "category")])
  } else {
    cat("No competencies extracted with GPT-4o\n")
  }
  
  cat("\n=== 3. Custom Business Prompt (GPT-4o-mini) ===\n")
  business_prompt <- function(n, h, t) {
    paste("Extract", n, "business skills. JSON: [{\"skill\": \"name\", \"type\": \"technical\"}]. Text:", t)
  }
  
  business_schema <- tidyllm::tidyllm_schema(
    name = "business",
    competencies = tidyllm::field_object(
      .vector = TRUE,
      skill = tidyllm::field_chr(.description = "Business skill"),
      type = tidyllm::field_fct(.levels = c("technical", "analytical", "communication"))
    )
  )
  
  business_result <- extract_competencies_tidyllm(
    chunks, 
    max_per_chunk = 3,
    model = "gpt-4o-mini",  # Force model that supports json_schema
    custom_prompt = business_prompt,
    custom_schema = business_schema
  )
  
  if (nrow(business_result) > 0 && "skill" %in% names(business_result)) {
    print(business_result[, c("skill", "type")])
  } else {
    cat("Custom business extraction result:\n")
    print(names(business_result))
    print(head(business_result))
  }
  
  cat("\n=== Customization Complete! ===\n")
} else {
  cat("Setup required: API key or packages missing\n")
}