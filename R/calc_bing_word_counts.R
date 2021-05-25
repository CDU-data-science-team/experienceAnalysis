#' Counts of words with a positive or negative sentiment
#'
#' Count the number of times a word with a positive or negative sentiment occurs
#' in a given text.
#'
#' @param x A data frame with three columns: the column with the classes; the
#'     column with the text; and the column(s) with the group(s).
#' @param target_col_name A string with the column name of the target variable.
#' @param text_col_name A string with the column name of the text variable.
#' @param grouping_variables A string or vector of strings with the column
#'     name(s) (if any) of the grouping variable(s). Defaults to `NULL`.
#' @param filter_class A string or vector of strings with the name(s) of the
#'     class(es) for which to count the words. Defaults to
#'     `NULL` (all classes).
#' @param filter_main_group A string with the name(s) of the organization(s)
#'     for which to count the words. Defaults to `NULL` (all
#'     organizations).
#'
#' @return A data frame with three columns: word; sentiment ("positive" or
#'     "negative"); and count.
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
