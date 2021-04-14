#' Title
#'
#' @param net_sentiment_long_nrc
#'
#' @return
#' @export
#'
#' @examples

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
