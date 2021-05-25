#' Calculate classifier accuracy for each class and group
#'
#' Calculates the accuracy of a predictive model for each class and group
#' (if any).
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
#' @param column_names A vector of strings, `NULL` or "px", used to specify the
#'     names of the returned data frame/tibble. See Details.
#'
#' @details This function was originally designed for use with package
#'     [`{pxtextminingdashboard}`](https://github.com/CDU-data-science-team/pxtextminingdashboard),
#'     in which case `column_names` is set to "px". It can, however, be used
#'     outside the context of `{pxtextminingdashboard}`, by controlling the
#'     `column_names` argument:
#'     - When `column_names == "px"`, the returned data frame has at least three
#'     columns, namely, "organization" (the group), "class" (the label) and
#'     "accuracy". When there are more than one groups, the first one is
#'     considered to be the "main" one and is named "organization" by default.
#'     - When `column_names` is `NULL`, then the returned data frame names are
#'     `c(grouping_variables, target_col_name, "accuracy")`.
#'     - When `column_names` is a vector of strings, the returned data frame
#'     names are as in the vector.
#'
#' @return A data frame/tibble with as many rows as the number of unique labels
#'     for each group (if any). See Details.
#' @export
#'
#' @examples

calc_accuracy_per_class <- function(x, target_col_name, target_pred_col_name,
                                    grouping_variables = NULL,
                                    column_names = c(NULL, "px"))
  {

  column_names <- experienceAnalysis::prep_colnames_and_filters(
    x = NULL, grouping_variables,
    target_col_name, filter_class = NULL,
    # main_group_col_name = grouping_variables[1],
    filter_main_group = NULL,
    column_names)$column_names

  accuracy_per_class <- x %>%
    dplyr::mutate(actual_vs_predicted =
                    .[[target_col_name]] == .[[target_pred_col_name]]) %>%
    dplyr::filter(!is.na(actual_vs_predicted)) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(grouping_variables, target_col_name))
      )
    ) %>%
    dplyr::summarise(accuracy = sum(actual_vs_predicted) /
                       length(actual_vs_predicted)) %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(~ column_names, .cols = names(.))

  return(accuracy_per_class)
}
