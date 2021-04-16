#' Title
#'
#' @param best_estimators
#'
#' @return
#' @export
#'
#' @examples

plot_best_estimators <- function(best_estimators) {

  p <- best_estimators %>%
    ggplot2::ggplot(ggplot2::aes(value,
                                 reorder(learner, -aux),
                                 fill = name)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = 12, angle = 90, hjust = 0.95, vjust = 0.2
      ),
      axis.text.y = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 12)
    ) +
    ggthemes::scale_fill_colorblind()

  return(p)
}
