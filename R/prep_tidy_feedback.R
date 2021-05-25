#' Title
#'
#' @param x
#' @param target_col_name
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
    tidytext::unnest_tokens(word, !! text_col_name) %>%
    dplyr::ungroup()

  return(tidy_feedback)
}
