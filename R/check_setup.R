#' Check OpenAI API Setup
#'
#' Utility function to diagnose common OpenAI API issues before running
#' competency extraction.
#'
#' @return A list with diagnostic information
#'
#' @examples
#' \dontrun{
#' # Check your API setup
#' check_openai_setup()
#' }
#'
#' @export
check_openai_setup <- function() {
  
  # Check API key
  api_key <- Sys.getenv("OPENAI_API_KEY")
  api_key_ok <- api_key != "" && !is.na(api_key) && startsWith(api_key, "sk-")
  
  # Check required packages
  tidyllm_available <- requireNamespace("tidyllm", quietly = TRUE)
  jsonlite_available <- requireNamespace("jsonlite", quietly = TRUE)
  
  # Test basic API call if everything looks good
  api_test_ok <- FALSE
  api_error <- NULL
  
  if (api_key_ok && tidyllm_available && jsonlite_available) {
    tryCatch({
      # Simple test call
      test_response <- tidyllm::llm_message("Test") %>% 
        tidyllm::chat(tidyllm::openai(.model = "gpt-4o-mini"), .timeout = 30)
      if (!is.null(test_response)) api_test_ok <- TRUE
    }, error = function(e) {
      api_error <<- e$message
    })
  }
  
  # Results
  results <- list(
    api_key_found = api_key != "" && !is.na(api_key),
    api_key_format_ok = startsWith(api_key, "sk-"),
    api_key_length = nchar(api_key),
    tidyllm_available = tidyllm_available,
    jsonlite_available = jsonlite_available,
    api_test_ok = api_test_ok,
    api_error = api_error
  )
  
  # Print diagnostic
  cat("🔍 OpenAI API Setup Diagnosis\n")
  cat("=====================================\n\n")
  
  cat("📋 Environment:\n")
  cat("  ✓ API Key found:", ifelse(results$api_key_found, "YES", "❌ NO"), "\n")
  cat("  ✓ API Key format:", ifelse(results$api_key_format_ok, "YES (sk-...)", "❌ INVALID"), "\n")
  cat("  ✓ API Key length:", results$api_key_length, "(should be ~51)\n\n")
  
  cat("📦 Packages:\n")
  cat("  ✓ tidyllm:", ifelse(results$tidyllm_available, "YES", "❌ MISSING"), "\n")
  cat("  ✓ jsonlite:", ifelse(results$jsonlite_available, "YES", "❌ MISSING"), "\n\n")
  
  cat("🌐 API Connection:\n")
  if (!api_key_ok || !tidyllm_available || !jsonlite_available) {
    cat("  ❌ Cannot test - fix above issues first\n\n")
  } else {
    cat("  ✓ API Test:", ifelse(results$api_test_ok, "SUCCESS", "❌ FAILED"), "\n")
    if (!results$api_test_ok) {
      cat("  ❌ Error:", results$api_error, "\n")
    }
    cat("\n")
  }
  
  # Recommendations
  cat("💡 Recommendations:\n")
  if (!results$api_key_found) {
    cat("  1. Set your API key: usethis::edit_r_environ()\n")
    cat("     Add: OPENAI_API_KEY=\"your-key-here\"\n")
    cat("  2. Restart R session\n")
  }
  if (!results$tidyllm_available || !results$jsonlite_available) {
    cat("  3. Install missing packages: install.packages(c('tidyllm', 'jsonlite'))\n")
  }
  if (!results$api_test_ok && !is.null(results$api_error)) {
    if (grepl("quota|insufficient_quota", results$api_error, ignore.case = TRUE)) {
      cat("  4. Check billing: https://platform.openai.com/account/billing\n")
    } else if (grepl("rate_limit", results$api_error, ignore.case = TRUE)) {
      cat("  4. Rate limit hit - wait a moment and try again\n")
    }
  }
  
  cat("\n")
  invisible(results)
}