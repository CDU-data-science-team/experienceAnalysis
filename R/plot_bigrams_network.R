#' Title
#'
#' @param tfidf_ngrams
#'
#' @return
#' @export
#'
#' @examples

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
