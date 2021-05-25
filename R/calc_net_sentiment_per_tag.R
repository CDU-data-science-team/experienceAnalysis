#' Title
#'
#' @param x
#' @param target_col_name
#' @param filter_organization
#'
#' @return
#' @export
#'
#' @examples
calc_net_sentiment_per_tag <- function(x, target_col_name, text_col_name,
                                       grouping_variables = NULL,
                                       filter_main_group = NULL) {

  tidy_feedback <- experienceAnalysis::prep_tidy_feedback(x, target_col_name,
                                                          text_col_name)

  aux <- experienceAnalysis::prep_colnames_and_filters(
    x, grouping_variables,
    target_col_name, filter_class = NULL,
    filter_main_group,
    column_names = NULL)

  filter_main_group <- aux$filter_main_group
  main_group_col_name <- aux$main_group_col_name

  # Find net sentiment in each tag
  net_sentiment_afinn <- tidy_feedback %>%
    dplyr::inner_join(tidytext::get_sentiments("afinn"), by = "word") %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(target_col_name, main_group_col_name))
      )
    ) %>%
    dplyr::summarise(sentiment = sum(value)) %>%
    dplyr::mutate(method = "AFINN")

  net_sentiment_bing_and_nrc <- dplyr::bind_rows(

    tidy_feedback %>%
      dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
      dplyr::filter(sentiment %in% c("positive", "negative")) %>%
      dplyr::mutate(method = "Bing et al."),

    tidy_feedback %>%
      dplyr::inner_join(tidytext::get_sentiments("nrc"), by = "word") %>%
      dplyr::filter(sentiment %in% c("positive", "negative")) %>%
      dplyr::mutate(method = "NRC")
  ) %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(c(target_col_name, main_group_col_name))),
      method, sentiment
    ) %>%
    tidyr::spread(sentiment, n, fill = 0) %>%
    dplyr::mutate(sentiment = positive - negative) %>%
    dplyr::ungroup()

  net_sentiment_all_dicts <- net_sentiment_afinn %>%
    dplyr::bind_rows(net_sentiment_bing_and_nrc) %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(main_group_col_name),
        ~ . %in% filter_main_group
      )
    )

  return(net_sentiment_all_dicts)
}
