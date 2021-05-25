#' Title
#'
#' @param x
#' @param target_col_name
#' @param filter_class
#' @param filter_organization
#' @param ngrams_type
#'
#' @return
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
    tidytext::unnest_tokens(bigram, !! text_col_name,
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
