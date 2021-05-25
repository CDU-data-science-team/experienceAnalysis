#' Calculate the confusion matrix for each group
#'
#' Calculates the confusion matrix of observed and predicted classes, overall
#' or for each group (if any).
#'
#' @param x A data frame with two or more columns, of which two should be
#'     the following: the column with the actual classes; and the column with
#'     the predicted classes. If there are grouping variables, the rest of the
#'     columns should have the groups.
#' @param target_col_name A string with the column name of the target variable.
#' @param target_pred_col_name A string with the column name of the predictions
#'     for the target variable.
#' @param grouping_variables A string or vector of strings with the column
#'     name(s) (if any) of the grouping variable(s). Defaults to `NULL`.
#' @param ... Further arguments passed from other methods.
#'
#' @return A list with as many elements as the number of groups (no groups
#'     returns a list with one element). Each element contains an object with
#'     class `conf_mat()` (see `conf_mat{yardstick}`).
#' @export
#'
#' @examples

calc_confusion_matrix <- function(x, target_col_name, target_pred_col_name,
                                  grouping_variables = NULL, ...) {

  aux <- x %>%
    dplyr::select(dplyr::all_of(c(grouping_variables, target_col_name,
                                  target_pred_col_name))) %>%
    dplyr::mutate(
      dplyr::across(
        -dplyr::all_of(grouping_variables),
        ~ as.factor(.)
      )
    )

  if (!is.null(grouping_variables)) {

    cm <- aux %>%
      split(f = .[[grouping_variables]]) %>%
      purrr::map(
        ~ yardstick::conf_mat(., {{target_col_name}}, {{target_pred_col_name}},
                              ...)
      )
  } else {

    cm <- aux %>%
      yardstick::conf_mat(., {{target_col_name}}, {{target_pred_col_name}},
                          ...) %>%
      list() # Convert to list to ensure returned object class is consistent.
  }

  return(cm)
}
