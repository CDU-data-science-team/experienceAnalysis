#' Plot sentiment counts in a text
#'
#' Bar plots of NRC sentiment counts in one or more documents.
#'
#' @param net_sentiment_long_nrc A data frame from
#'     \code{\link{tidy_net_sentiment_nrc}}.
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
#' # Net sentiment in each book
#' net_sentiment_wide_nrc <- calc_net_sentiment_nrc(x, target_col_name = "book",
#'                                                  text_col_name = "text",
#'                                                  filter_class = NULL)
#'
#' net_sentiment_wide_nrc
#'
#' # Tidy net_sentiment_wide_nrc and place the most "angry" book at the top
#' net_sentiment_long_nrc <- tidy_net_sentiment_nrc(net_sentiment_wide_nrc,
#'                                                  sorting_sentiments = "anger",
#'                                                  num_of_docs = 2) %>%
#'   dplyr::select(-text)
#'
#' net_sentiment_long_nrc
#'
#' plot_net_sentiment_long_nrc(net_sentiment_long_nrc)

plot_net_sentiment_long_nrc <- function(net_sentiment_long_nrc) {

  p <- net_sentiment_long_nrc %>%
    ggplot2::ggplot(ggplot2::aes(value, name)) +
    ggplot2::geom_col(fill = "blue", alpha = 0.6) +
    ggplot2::facet_wrap(~ linenumber, ncol = 5) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::ylab('')

  return(p)
}
