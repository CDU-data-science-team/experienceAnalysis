#' Plot a network of bigrams
#'
#' @param tfidf_ngrams A data frame from \code{\link{calc_bigrams_network}}.
#'
#' @return A `ggraph` (`ggraph::ggraph`) network of bigrams.
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
#' # Bigrams for Emma
#' calc_bigrams_network(x, target_col_name = "book", text_col_name = "text",
#'                      filter_class = "Emma", bigrams_prop = 3) %>%
#'     plot_bigrams_network()

plot_bigrams_network <- function(bigrams_table) {

  a <- grid::arrow(
    type = "closed",
    length = grid::unit(.15, "inches")
  )

  if (nrow(bigrams_table) != 0) {

    bigrams_table %>%
      igraph::graph_from_data_frame() %>%
      ggraph::ggraph(layout = "fr") +
      ggraph::geom_edge_link(
        ggplot2::aes(edge_alpha = n),
        show.legend = FALSE,
        arrow = a
      ) +
      ggraph::geom_node_point(color = "blue", size = 5) +
      ggraph::geom_node_text(
        ggplot2::aes(label = name),
        vjust = 1,
        hjust = 1,
        size = 5
      ) +
      ggplot2::theme_void()
  } else {

    plot(x = 0, y = 0, xaxt = 'n', yaxt = 'n', ann = FALSE, type = 'n')
    text(x = 0, y = 0, labels = "Not enough data for the selected label.\n Try to increase the proportion of bigrams.")
  }
}
