#' Title
#'
#' @param text_data
#' @param class_col_name
#' @param org_col_name
#' @param filter_class
#' @param filter_organization
#'
#' @return
#' @export
#'
#' @examples

calc_net_sentiment_nrc <- function(x, class_col_name, org_col_name,
                                 filter_class, filter_organization) {

  nrc_sentiments <- experienceAnalysis::prep_sentiments_nrc()

  text_data_filtered <- x %>%
    #dplyr::filter(super != "Couldn't be improved") %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(class_col_name),
        # .data[[class_col_name]],
        ~ . %in% {{filter_class}}
      ),
      dplyr::across(
        dplyr::all_of(org_col_name),
        ~ . %in% {{filter_organization}}
      )
    )

  net_sentiment_nrc <- text_data_filtered %>%
    tidytext::unnest_tokens(word, feedback) %>%
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
