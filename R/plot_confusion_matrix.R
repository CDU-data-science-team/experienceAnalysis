#' Title
#'
#' @param x
#' @param target_col_name
#' @param target_pred_col_name
#' @param grouping_variable
#' @param type
#'
#' @return
#' @export
#'
#' @examples

plot_confusion_matrix <- function(x, target_col_name, target_pred_col_name,
                                  grouping_variable = NULL, type = "heatmap") {

  cm <- experienceAnalysis::calc_confusion_matrix(x, target_col_name,
                                                  target_pred_col_name,
                                                  grouping_variable)

  cm %>%
    lapply(
      function(x) {
        x %>%
          ggplot2::autoplot(type) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5,
                                                hjust = 1),
            axis.text = ggplot2::element_text(size = 15)
          )
      }
    )
}
