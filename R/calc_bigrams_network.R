#' Create and count bigrams
#'
#' For a given labeled text, create and calculate the most frequently occurring
#' bigrams for the class(es) and organization(s).
#'
#' @param x A data frame with three columns: the column with the classes; the
#'     column with the text; and the column(s) with the group(s).
#' @param target_col_name A string with the column name of the target variable.
#' @param text_col_name A string with the column name of the text variable.
#' @param grouping_variables A string or vector of strings with the column
#'     name(s) (if any) of the grouping variable(s). Defaults to `NULL`.
#' @param filter_class A string or vector of strings with the name(s) of the
#'     class(es) for which bigrams are to be created and counted. Defaults to
#'     `NULL` (all classes).
#' @param filter_main_group A string with the name(s) of the organization(s)
#'     for which to create and count bigrams. Defaults to `NULL` (all
#'     organizations).
#' @param bigrams_prop A numeric in (0, 100] indicating the percentage of the
#'     most frequent bigrams to keep.
#'
#' @note When supplying more than one organization and/or class, the returned
#'     data frame will NOT separate the results for the different organizations
#'     and/or classes, i.e. the function sees multiple organizations and/or
#'     classes as one.
#'
#' @return A data frame with three columns: first word of bigram;
#'     second word of bigram; and bigram count.
#' @export
#'
#' @examples

calc_bigrams_network <- function(x, target_col_name, text_col_name,
                                 grouping_variables = NULL,
                                 filter_class = NULL,
                                 filter_main_group = NULL, bigrams_prop) {

  aux <- experienceAnalysis::prep_colnames_and_filters(
    x, grouping_variables,
    target_col_name, filter_class,
    filter_main_group,
    column_names = NULL)

  filter_class <- aux$filter_class
  filter_main_group <- aux$filter_main_group
  main_group_col_name <- aux$main_group_col_name

  bigrams_table <- x %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(main_group_col_name),
        ~ . %in% filter_main_group)
      ) %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ . %in% filter_class
      )
    ) %>%
    tidytext::unnest_tokens(bigram, !! rlang::sym(text_col_name),
                            token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    dplyr::filter(
      dplyr::across(
        dplyr::starts_with("word"),
        ~ !. %in% tidytext::stop_words$word
      )
    ) %>%
    dplyr::count(word1, word2, sort = TRUE) %>%
    dplyr::filter(
      dplyr::across(dplyr::starts_with("word"), ~ !is.na(.)),
      n > 1
    ) %>%
    dplyr::slice_max(prop = bigrams_prop / 100, order_by = n)

  return(bigrams_table)
}
