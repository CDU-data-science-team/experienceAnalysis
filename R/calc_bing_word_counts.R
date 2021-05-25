#' Title
#'
#' @param x
#' @param target_col_name
#' @param filter_organization
#' @param filter_class
#'
#' @return
#' @export
#'
#' @examples

calc_bing_word_counts <- function(x, target_col_name, text_col_name,
                                  grouping_variables = NULL,
                                  filter_main_group = NULL,
                                  filter_class = NULL) {

  aux <- experienceAnalysis::prep_colnames_and_filters(
    x, grouping_variables,
    target_col_name, filter_class,
    filter_main_group,
    column_names = NULL)

  filter_class <- aux$filter_class
  filter_main_group <- aux$filter_main_group
  main_group_col_name <- aux$main_group_col_name

  tidy_feedback <- experienceAnalysis::prep_tidy_feedback(x, target_col_name,
                                                          text_col_name)

  # Most common positive and negative words
  bing_word_counts <- tidy_feedback %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ . %in% filter_class
      ),
      dplyr::across(
        dplyr::all_of(main_group_col_name),
        ~ . %in% filter_main_group
      )
    ) %>%
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
    dplyr::count(word, sentiment, sort = TRUE)

  return(bing_word_counts)
}
