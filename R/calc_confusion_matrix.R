#' Calculate the confusion matrix
#'
#' Calculates the confusion matrix of observed and predicted classes, after
#' converting them into factors first (not currently done by
#' `conf_mat{yardstick}`).
#'
#' @param x A data frame with two columns: the column with the actual classes;
#'     and the column with the predicted classes. Any other columns will be
#'     ignored.
#' @param target_col_name A string with the column name of the target variable.
#' @param target_pred_col_name A string with the column name of the predictions
#'     for the target variable.
#' @param ... Further arguments passed from other methods.
#'
#' @return An object with class `conf_mat()` (see `conf_mat{yardstick}`).
#' @export
#'
#' @examples

calc_confusion_matrix <- function(x, target_col_name, target_pred_col_name,
                                  ...) {

  # Convert cols to factors
  aux <- x %>%
    dplyr::select(dplyr::all_of(c(target_col_name, target_pred_col_name))) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.factor(.)
      )
    )

  cm <- aux %>%
    yardstick::conf_mat(., {{target_col_name}}, {{target_pred_col_name}}, ...)

  return(cm)
}
