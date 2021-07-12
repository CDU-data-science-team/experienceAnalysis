#' Plot the _n_-grams with the highest TF-IDFs
#'
#' @param tfidf_ngrams A data frame from \code{\link{calc_tfidf_ngrams}}.
#'
#' @return A `ggplot` (`ggplot::geom_col`).
#' @export
#'
#' @examples
#' library(experienceAnalysis)
#' books <- janeaustenr::austen_books() # Jane Austen books
#' emma <- paste(books[books$book == "Emma", ], collapse = " ") # String with whole book
#' pp <- paste(books[books$book == "Pride & Prejudice", ], collapse = " ") # String with whole book
#'
#' # Make data frame with books Emma and Pride & Prejudice
#' x <- data.frame(
#'   text = c(emma, pp),
#'   book = c("Emma", "Pride & Prejudice")
#' )
#'
#' calc_tfidf_ngrams(x, target_col_name = "book", text_col_name = "text",
#'                   filter_class = "Emma",
#'                   ngrams_type = "Bigrams",
#'                   number_of_ngrams = 5
#' ) %>%
#'   dplyr::filter(ngram != "4 4") %>% # First bigram is useless and distorts the plot
#'   plot_tfidf_ngrams(title = "Bigrams with highest TF-IDFs in Emma")


plot_tfidf_ngrams <- function(tfidf_ngrams, ngrams_type, filter_class) {

  p <- tfidf_ngrams %>%
    ggplot2::ggplot(ggplot2::aes(tf_idf, reorder(ngram, tf_idf))) +
    ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
    ggplot2::labs(x = "TF-IDF*", y = NULL,
                  title = paste0("Most frequent ", ngrams_type,
                                 " in feedback text that is about\n",
                                 "\"", filter_class, "\"")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 12)
    )

  return(p)
}
