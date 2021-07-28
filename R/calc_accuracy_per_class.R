#' Calculate classifier accuracy for each class and group
#'
#' Calculates the accuracy of a predictive model for each class.
#'
#' @param x A data frame with two columns: the column with the actual classes;
#'     and the column with the predicted classes. Any other columns will be
#'     ignored.
#' @param target_col_name A string with the column name of the target variable.
#' @param target_pred_col_name A string with the column name of the predictions
#'     for the target variable.
#' @param column_names A vector of strings or `NULL`, used to specify the
#'     names of the returned data frame/tibble. See Details.
#'
#' @details This function was originally designed for use with package
#'     [`{pxtextminingdashboard}`](https://github.com/CDU-data-science-team/pxtextminingdashboard),
#'     in which case `column_names` is set to `c("class", "accuracy")`. It can,
#'     however, be used outside the context of`{pxtextminingdashboard}`, by
#'     controlling the `column_names` argument:
#'     \itemize{
#'       \item{When `column_names` is `NULL`, then the returned data frame names
#'         are `c(target_col_name, "accuracy")`.}
#'       \item{When `column_names` is a vector of strings, the returned data
#'         frame names are as in the vector.}
#'     }
#'
#' @return A data frame/tibble with as many rows as the number of unique labels.
#' @export
#'
#' @examples
#' library(experienceAnalysis)
#' mtcars %>%
#'   dplyr::mutate(carb_pred = sample(carb, size = nrow(.))) %>%  # Mock predictions column
#'   calc_accuracy_per_class(
#'     target_col_name = "carb",
#'     target_pred_col_name = "carb_pred"
#'   )
#'
#' # Custom column names
#' mtcars %>%
#'   dplyr::mutate(carb_pred = sample(carb, size = nrow(.))) %>%  # Mock predictions column
#'   calc_accuracy_per_class(
#'     target_col_name = "carb",
#'     target_pred_col_name = "carb_pred",
#'     column_names = c("class", "accuracy_per_class")
#'   )

calc_accuracy_per_class <- function(x, target_col_name, target_pred_col_name,
                                    column_names = NULL)
  {

  accuracy_per_class <- x %>%
    dplyr::mutate(actual_vs_predicted =
                    .[[target_col_name]] == .[[target_pred_col_name]]) %>%
    dplyr::filter(!is.na(actual_vs_predicted)) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(target_col_name)
      )
    ) %>%
    dplyr::summarise(accuracy = sum(actual_vs_predicted) /
                       length(actual_vs_predicted)) %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(
      function(x) {
        if (!all(is.null(column_names))) {
          column_names
        } else {
          c(target_col_name, "accuracy")
        }
      },
      .cols = names(.)
    )

  return(accuracy_per_class)
}
