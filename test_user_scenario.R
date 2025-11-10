# Test script to reproduce user's issue
library(TidyChunking)

# Create test data similar to user's case
test <- data.frame(
  chunk_id = c("1.1", "2.1", "3.1", "4.1", "5.1"),
  chunk_text = c(
    "Data analysis requires skills.",
    "Programming is important.",
    "Statistics knowledge helps.",
    "Excel for basic tasks.",
    "R for advanced analysis."
  ),
  word_count = c(5, 3, 3, 4, 4),
  heading = c("Analysis", "Programming", "Statistics", "Excel", "R"),
  level = c(1, 1, 1, 1, 1),
  parent_h1 = c("Analysis", "Programming", "Statistics", "Excel", "R"),
  parent_h2 = rep(NA, 5),
  hierarchy = c("Analysis", "Programming", "Statistics", "Excel", "R"),
  content_type = rep("main_content", 5)
)

# Convert to tibble like user would have
library(dplyr)
test <- as_tibble(test)

# Define custom prompt like user did
my_prompt <- function(n_comp, hierarchy, text) {
  paste("Extract", n_comp, "business skills from:", text, "JSON format required.")
}

cat("=== Testing user's scenario ===\n")

if (require(tidyllm) && require(jsonlite) && check_openai_setup()$status == "success") {
  
  # Test the exact scenario user encountered
  cat("Running: extract_competencies_tidyllm(chunks = test |> slice(1:5), max_per_chunk = 15, model = \"gpt-4o-mini\", custom_prompt = my_prompt)\n")
  
  word_test <- extract_competencies_tidyllm(
    chunks = test %>% slice(1:5),
    max_per_chunk = 15,
    model = "gpt-4o-mini",  # Use working model instead of gpt-4.1
    custom_prompt = my_prompt
  )
  
  cat("SUCCESS! No more 'could not find function prompt_function' error\n")
  print(word_test)
  
} else {
  cat("API not configured\n")
}