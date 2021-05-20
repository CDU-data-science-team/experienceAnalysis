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

calc_tfidf_ngrams <- function(x, target_col_name, filter_class,
                         filter_organization, ngrams_type) {

  ngrams_n <- ifelse(ngrams_type == "Unigrams", 1, 2)

  tfidf_ngrams <- x %>%
    #dplyr::filter(organization %in% {{filter_organization}}) %>%
    tidytext::unnest_tokens(ngram, feedback, token = "ngrams", n = ngrams_n) %>%
    tidyr::separate(ngram, paste0("word", 1:ngrams_n), sep = " ") %>%
    dplyr::filter( # Do this because some stop words make it through the TF-IDF filtering that happens below.
      dplyr::across(
        dplyr::starts_with("word"),
        ~ !. %in% tidytext::stop_words$word
      )
    ) %>%
    tidyr::unite(col = "ngram", paste0("word", 1:ngrams_n), sep = " ") %>%
    dplyr::count(dplyr::across(dplyr::all_of(target_col_name)), ngram, sort = TRUE) %>%
    tidytext::bind_tf_idf(ngram, !! target_col_name, n) %>% # {{}} doesn't work here. Use !! instead.
    dplyr::group_by(dplyr::across(dplyr::all_of(target_col_name))) %>%
    dplyr::slice_max(tf_idf, n = 15) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ . %in% {{filter_class}}
      )
    )

  return(tfidf_ngrams)
}
