#' Unnest tokens for each label in a labelled text
#'
#' Creates table with tokens for each class (if any).
#'
#' @param x x A data frame with one or more columns: the column with the classes
#'     (if `target_col_name` is not `NULL`); and the column with the text. Any
#'     other columns will be ignored.
#' @param target_col_name A string with the column name of the target variable.
#'     Defaults to `NULL`.
#' @param text_col_name A string with the column name of the text variable.
#'
#' @return A data frame with two or three columns: classes (if `target_col_name`
#'     is not `NULL`); line numbers; and tokens.
#' @export
#'
#' @examples

prep_tidy_feedback <- function(x, target_col_name = NULL, text_col_name) {

  tidy_feedback <- x %>%
    dplyr::select(
      dplyr::all_of(c(target_col_name, text_col_name))
    ) %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>%
    tidytext::unnest_tokens(word, !! rlang::sym(text_col_name)) %>%
    dplyr::ungroup()

  return(tidy_feedback)
}
