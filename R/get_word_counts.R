#' Generate word counts
#'
#' @param df A data.frame
#' @param column A column name as a string
#' @param sentiments An optional data.frame of sentiments from tidytext::get_sentiments("bing")
#' @param exclude_words An optional vector of words to exclude from the counts. Stop words (e.g. and, but, or) are handled automatically.
#' @param min_count The minimum word frequency to include in the final counts. Useful for pruning wordclouds.
#'
#' @return A data.frame of word counts, optionally by sentiment
#' @export
#' @import tidytext
#' @import dplyr
#'
get_word_counts <- function(df, column, sentiments = NULL, exclude_words = "", min_count = 1){

  if (!is.null(sentiments) & !is.data.frame(sentiments)){
    stop("Sentiments must be a data.frame from tidytext::get_sentiments('bing')")
  }

  tokens <- df %>%
    tidytext::unnest_tokens(.data$word, !!as.name(column)) %>%
    dplyr::anti_join(tidytext::stop_words, by = "word") %>%
    dplyr::filter(!is.na(.data$word),
                  !.data$word %in% exclude_words
                  )

  if (!is.null(sentiments)){

    tokens <- dplyr::inner_join(tokens, sentiments, by = "word") %>%
      dplyr::group_by(.data$word, .data$sentiment) %>%
      dplyr::count() %>%
      dplyr::filter(.data$n >= min_count) %>%
      dplyr::arrange(.data$sentiment, -.data$n)

  } else{

    tokens <- tokens %>%
      dplyr::group_by(.data$word) %>%
      dplyr::count() %>%
      dplyr::filter(.data$n >= min_count) %>%
      dplyr::arrange(-.data$n)

  }

  return(tokens)

}
