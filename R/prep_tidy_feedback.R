#' Unnest tokens for each label in a labeled text
#'
#' Creates table with tokens and other columns (e.g. labels, groups)
#' passed by the user.
#'
#' @param x A data frame with three columns: the column with the classes; the
#'     column with the text; and the column(s) with the group(s).
#' @param target_col_name A string with the column name of the target variable.
#' @param text_col_name A string with the column name of the text variable.
#' @param grouping_variables A string or vector of strings with the column
#'     name(s) (if any) of the grouping variable(s). Defaults to `NULL`.
#'
#' @return
#' @export
#'
#' @examples

prep_tidy_feedback <- function(x, target_col_name, text_col_name,
                               grouping_variables = NULL) {

  aux <- experienceAnalysis::prep_colnames_and_filters(
    x, grouping_variables,
    target_col_name, filter_class = NULL,
    filter_main_group = NULL,
    column_names = NULL)

  main_group_col_name <- aux$main_group_col_name

  tidy_feedback <- x %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(target_col_name, main_group_col_name)))
    ) %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>%
    tidytext::unnest_tokens(word, !! rlang::sym(text_col_name)) %>%
    dplyr::ungroup()

  return(tidy_feedback)
}
