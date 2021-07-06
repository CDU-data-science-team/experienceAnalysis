#' Calculate "net sentiment" in a text
#'
#' For a given text, translate all words into NRC sentiments and count sentiment
#' occurrences.
#'
#' @param x A data frame with two columns: the column with the classes; and the
#'     column with the text. Any other columns will be ignored.
#' @param target_col_name A string with the column name of the target variable.
#'     Defaults to `NULL`.
#' @param text_col_name A string with the column name of the text variable.
#' @param filter_class A string or vector of strings with the name(s) of the
#'     class(es) for which bigrams are to be created and counted. Defaults to
#'     `NULL` (all rows).
#'
#' @note When supplying more than one class in `filter_class`, the returned data
#'     frame will NOT separate the results for the different classes. If
#'     separation is desired, then do something like this:
#'
#'     # Assuming that the class and text text columns are called "label" and
#'     # "feedback" respectively
#'     x %>%
#'         split(.$label) %>%
#'         purrr::map(
#'             ~ calc_net_sentiment_nrc(., target_col_name = NULL,
#'                                    text_col_name = "feedback",
#'                                    filter_class = NULL)
#'         )
#'
#' @return A data frame with 12 or 13 columns: the text column; the line number;
#'     the 10 NRC sentiments (anger, anticipation disgust, fear, joy, negative,
#'     positive, sadness, surprise, trust- see Mohammad & Turney, 2013); and the
#'     column with the classes (if any).
#' @export
#'
#' @examples
#' @references Mohammad S.M. & Turney P.D. (2013). Crowdsourcing a
#'     Word–Emotion Association Lexicon. Computational Intelligence,
#'     29(3):436-465.

calc_net_sentiment_nrc <- function(x, target_col_name = NULL, text_col_name,
                                   filter_class = NULL) {

  nrc_sentiments <- experienceAnalysis::prep_sentiments_nrc()

  text_data_filtered <- x %>%
    #dplyr::filter(super != "Couldn't be improved") %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ experienceAnalysis::tidy_class_filter(., filter_class)
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
    dplyr::select(
      dplyr::all_of(text_col_name),
      dplyr::everything(),
      -`NA`
    )

    return(net_sentiment_nrc)
}
