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

get_bing_word_counts <- function(x, target_col_name,
                                 filter_organization, filter_class) {

  tidy_feedback <- experienceAnalysis::get_tidy_feedback(x, target_col_name)

  # Most common positive and negative words
  bing_word_counts <- tidy_feedback %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ . %in% {{filter_class}}
      ),
      organization %in% {{filter_organization}}
    ) %>%
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
    dplyr::count(word, sentiment, sort = TRUE)

  return(bing_word_counts)
}
