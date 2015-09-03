#' Check the words in text against a dictionary
#'
#' This function checks the quality of an OCR text against a dictionary. It will
#' return a number between \code{0} and \code{1}, which is the ratio of words
#' found in the dictionary to the total number of words in the document. The
#' higher the number, the better the quality of the OCR. These measures should
#' not be taken in an absolute sense. That is, a score of 1 does not indicate
#' perfect OCR. They should only be used to determine the relative quality of
#' OCR within a corpus of texts. You can pass a character vector of any length.
#' So, if you split a text into chunks, you can evaluate the OCR quality of each
#' chunk.
#'
#' @param text A character vector.
#' @param sample_size If this value is positive, then this many words from the
#'   \code{text} will be selected for comparison. This is useful for large
#'   texts.
#' @return A vector of numeric values between \code{0} and \code{1}.
#' @examples
#' paragraph <- "Fourr score and sleven years ago our fathers brought
#'   forth on this continent, a new nation, conceived in Liberty,
#'   and dedicated to tlhe proposition that all men are created equal."
#'
#' ocr_dictionary(paragraph)
#'
#' @export
ocr_dictionary <- function(text, sample_size = -1L) {

  stopifnot(is.character(text))
  if (!missing(sample_size)) stopifnot(sample_size > 0)

  text <- text %>%
    stringr::str_to_lower() %>%
    stringr::str_split(stringr::boundary("word"))

  vapply(text, function(words) {

    if (0 < sample_size & sample_size < length(words))
      words <- sample(words, sample_size)

    mismatches <- setdiff(words, words_en)
    errors <- Filter(function(x) {x %in% mismatches}, words)

    1 - (length(errors) / length(words))

  }, numeric(1))

}
