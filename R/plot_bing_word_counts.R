#' Plot bar plots of the most frequent words.
#'
#' @param bing_word_counts A data frame from \code{\link{calc_bing_word_counts}}.
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
#' # Word counts for both books
#' calc_bing_word_counts(x, target_col_name = "book", text_col_name = "text",
#'                       filter_class = NULL) %>%
#'     plot_bing_word_counts()

plot_bing_word_counts <- function(bing_word_counts) {

  p <- bing_word_counts %>%
    dplyr::group_by(sentiment) %>%
    dplyr::top_n(10) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(n, reorder(word, n), fill = sentiment)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~ sentiment, scales = "free_y") +
    #ggplot2::labs(x = "Contribution to sentiment") +
    ggplot2::ylab("") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.text.y = ggplot2::element_text(size = 12)
    )

  return(p)
}
