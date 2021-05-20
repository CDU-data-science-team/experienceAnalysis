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

calc_bigrams_network <- function(x, target_col_name, filter_class,
                         filter_organization, bigrams_prop) {

  bigrams_table <- x %>%
    dplyr::filter(organization %in% {{filter_organization}}) %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ . %in% {{filter_class}}
      )
    ) %>%
    tidytext::unnest_tokens(bigram, feedback, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ") %>%
    dplyr::filter( # Do this because some stop words make it through the TF-IDF filtering that happens below.
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
