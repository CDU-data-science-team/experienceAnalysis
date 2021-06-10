#' Calculate "net sentiment" in a text
#'
#' For a given text, translate all words into NRC sentiments and count sentiment
#' occurrences.
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
#'
#' @return
#' @export
#'
#' @examples

calc_net_sentiment_nrc <- function(x, target_col_name, text_col_name,
                                   grouping_variables = NULL,
                                   filter_class, filter_main_group = NULL) {

  nrc_sentiments <- experienceAnalysis::prep_sentiments_nrc()

  aux <- experienceAnalysis::prep_colnames_and_filters(
    x, grouping_variables,
    target_col_name = NULL, filter_class,
    filter_main_group,
    column_names = NULL)

  filter_class <- aux$filter_class
  filter_main_group <- aux$filter_main_group
  main_group_col_name <- aux$main_group_col_name

  text_data_filtered <- x %>%
    #dplyr::filter(super != "Couldn't be improved") %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        # .data[[target_col_name]],
        ~ . %in% filter_class
      ),
      dplyr::across(
        dplyr::all_of(main_group_col_name),
        ~ . %in% filter_main_group
      )
    )

  net_sentiment_nrc <- text_data_filtered %>%
    tidytext::unnest_tokens(word, !! rlang::sym(text_col_name)) %>%
    dplyr::left_join(tidytext::get_sentiments("nrc"), by = "word") %>% # We want a left join so as not to lose comments with no sentiment
    dplyr::count(linenumber, sentiment, name = "sentiment_count") %>%
    dplyr::mutate(
      sentiment_count =
        dplyr::case_when(
          is.na(sentiment) ~ NA_integer_,
          TRUE ~ sentiment_count
        )
    ) %>%
    dplyr::select(linenumber, sentiment, sentiment_count) %>%
    tidyr::pivot_wider(names_from = sentiment,
                       values_from = sentiment_count,
                       values_fill = 0,
                       names_sort = TRUE
    ) %>%
    dplyr::left_join(text_data_filtered, by = "linenumber") %>%
    dplyr::select(feedback, dplyr::everything(), -`NA`) %>%
    dplyr::select(feedback, dplyr::everything())

    return(net_sentiment_nrc)
}
