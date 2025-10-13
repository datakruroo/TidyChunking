#' TidyChunking: Intelligent Markdown Document Chunking
#'
#' An R package for intelligently chunking markdown documents into meaningful 
#' sections optimized for keyword extraction and text analysis.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{chunk_for_keyword_extraction}}}{Chunk markdown text into meaningful sections}
#'   \item{\code{\link{filter_chunks_for_keywords}}}{Filter chunks suitable for keyword extraction}
#'   \item{\code{\link{preview_chunks}}}{Preview chunk structure and statistics}
#'   \item{\code{\link{extract_competencies_tidyllm}}}{Extract competencies using AI (requires tidyllm)}
#' }
#'
#' @section Quick Start:
#' 
#' \preformatted{
#' library(TidyChunking)
#' 
#' # Basic chunking
#' markdown_text <- "# Chapter 1\n\n## Section A\n\nContent here..."
#' chunks <- chunk_for_keyword_extraction(markdown_text)
#' 
#' # Filter for keywords
#' keyword_chunks <- filter_chunks_for_keywords(chunks)
#' 
#' # Preview results
#' preview_chunks(keyword_chunks)
#' }
#'
#' @docType package
#' @name TidyChunking
"_PACKAGE"