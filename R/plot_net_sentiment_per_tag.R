#' Title
#'
#' @param net_sentiment_all_dicts
#' @param target_col_name
#'
#' @return
#' @export
#'
#' @examples

plot_net_sentiment_per_tag <- function(net_sentiment_all_dicts, target_col_name) {

  p <- net_sentiment_all_dicts %>%
    ggplot2::ggplot(
      ggplot2::aes(
        sentiment,
        reorder(net_sentiment_all_dicts[[target_col_name]], sentiment) # Don't know how to do it with non-standard evaluation.
      )
    ) +
    ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
    ggplot2::facet_wrap(~ method, ncol = 1, scales = "free") +
    ggplot2::labs(
      x = "Net sentiment",
      y = NULL,
      title = "Net sentiment per tag"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.text.y = ggplot2::element_text(size = 12)
    )

  return(p)
}
