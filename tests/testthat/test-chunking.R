test_that("chunk_for_keyword_extraction works with basic markdown", {
  markdown_text <- "# Chapter 1\n\n## Section A\n\nContent here.\n\n## Section B\n\nMore content."
  
  result <- chunk_for_keyword_extraction(markdown_text, max_words = 100, min_words = 10)
  
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expect_true(all(c("chunk_id", "chunk_text", "word_count", "content_type") %in% names(result)))
})

test_that("filter_chunks_for_keywords removes unwanted content", {
  # Create sample chunks
  chunks <- data.frame(
    chunk_id = c("1.1", "2.1", "3.1"),
    chunk_text = c("Main content here", "References section", "Example content"),
    word_count = c(100, 50, 80),
    content_type = c("main_content", "metadata", "example"),
    stringsAsFactors = FALSE
  )
  
  result <- filter_chunks_for_keywords(chunks)
  
  expect_equal(nrow(result), 2)  # Should keep main_content and example
  expect_true(all(result$content_type %in% c("main_content", "example")))
})

test_that("preview_chunks returns input invisibly", {
  chunks <- data.frame(
    chunk_id = "1.1",
    chunk_text = "Test content",
    word_count = 50,
    content_type = "main_content",
    hierarchy = "Test > Section",
    stringsAsFactors = FALSE
  )
  
  result <- capture.output(returned <- preview_chunks(chunks))
  
  expect_identical(returned, chunks)
})