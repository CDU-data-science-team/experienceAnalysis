#' Plot a confusion matrix
#'
#' @param x A data frame with two columns: the column with the actual classes;
#'     and the column with the predicted classes. Any other columns will be
#'     ignored.
#' @param target_col_name A string with the column name of the target variable.
#' @param target_pred_col_name A string with the column name of the predictions
#'     for the target variable.
#' @param type A string indicating the of plot: "mosaic" or "heatmap".
#'     Defaults to- and currntly can only be- "heatmap".
#'
#' @details This function differs from `ggplot2::autoplot` because the gradient
#'     fills the heatmap (confusion matrix) based on absolute counts. It does
#'     not make sense to compare colours between different "Truth" columns. By
#'     contrast `plot_confusion_matrix` expresses column counts as proportions
#'     of the total count in that column. The proportions are used to define a
#'     colour scale. The _actual_ counts are plotted though.
#'
#' @return A `ggplot` (`ggplot::geom_tile`).
#' @export
#'
#' @examples
#' library(experienceAnalysis)
#' mtcars %>%
#'   dplyr::mutate(carb_pred = sample(carb, size = nrow(.))) %>%  # Mock predictions column
#'   plot_confusion_matrix(
#'     target_col_name = "carb",
#'     target_pred_col_name = "carb_pred"
#'   )

plot_confusion_matrix <- function(x, target_col_name, target_pred_col_name,
                                  type = "heatmap") {

  cm <- experienceAnalysis::calc_confusion_matrix(x, target_col_name,
                                                  target_pred_col_name)

  data_heatmap <- cm$table %>%
    as.data.frame() %>%
    dplyr::group_by(Truth) %>%
    dplyr::mutate(
      prop = round(Freq / sum(Freq) * 100),
      bin = dplyr::case_when(
        dplyr::between(prop, 0, 9) ~ "gray88",
        dplyr::between(prop, 10, 19) ~ "gray80",
        dplyr::between(prop, 20, 29) ~ "gray73",
        dplyr::between(prop, 30, 39) ~ "gray61",
        dplyr::between(prop, 40, 59) ~ "gray57",
        dplyr::between(prop, 60, 69) ~ "gray54",
        dplyr::between(prop, 70, 79) ~ "gray48",
        dplyr::between(prop, 80, 100) ~ "gray37",
      )
    )

  data_heatmap %>%
    ggplot2::ggplot(ggplot2::aes(Truth, forcats::fct_rev(Prediction))) +
    ggplot2::geom_tile(ggplot2::aes(fill = bin, colour = "")) + # Colour will create a grid. Whichever value will trigger it to draw the grid.
    ggplot2::scale_fill_manual(values = sort(unique(data_heatmap$bin))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5,
                                          hjust = 1),
      axis.text = ggplot2::element_text(size = 15)
    ) +
    ggplot2::geom_text(ggplot2::aes(label = Freq)) +
    ggplot2::ylab("Prediction")
}
