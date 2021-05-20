#' Title
#'
#' @param tfidf_ngrams
#'
#' @return
#' @export
#'
#' @examples

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
