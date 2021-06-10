#' Calculate TF-IDFs for unigrams or bigrams
#'
#' For a given labeled text, return the unigrams or bigrams with the 15
#' largest TF-IDFs for the given class(es) and organization(s).
#'
#' @param x A data frame with three columns: the column with the classes; the
#'     column with the text; and the column(s) with the group(s).
#' @param target_col_name A string with the column name of the target variable.
#' @param text_col_name A string with the column name of the text variable.
#' @param grouping_variables A string or vector of strings with the column
#'     name(s) (if any) of the grouping variable(s). Defaults to `NULL`.
#' @param filter_class A string or vector of strings with the name(s) of the
#'     class(es) for which TF-IDFs are to be calculated. Defaults to
#'     `NULL` (all classes).
#' @param filter_main_group A string with the name(s) of the organization(s)
#'     for which to calculate TF-IDFs. Defaults to `NULL` (all
#'     organizations).
#' @param ngrams_type A string. Should be "Unigrams" for unigrams and "Bigrams"
#'     for bigrams.
#'
#' @note When supplying more than one organization and/or class, the returned
#'     data frame will NOT separate the results for the different organizations
#'     and/or classes, i.e. the function sees multiple organizations and/or
#'     classes as one.
#'
#' @return A data frame with six columns: class; n-gram (word or bigram); count;
#'     term-frequency; inverse document frequency; and TF-IDF.
#' @export
#'
#' @examples

calc_tfidf_ngrams <- function(x, target_col_name, text_col_name,
                              grouping_variables = NULL,
                              filter_class = NULL, filter_main_group = NULL,
                              ngrams_type = c("Unigrams", "Bigrams")) {

  aux <- experienceAnalysis::prep_colnames_and_filters(
    x, grouping_variables,
    target_col_name, filter_class,
    filter_main_group,
    column_names = NULL)

  filter_class <- aux$filter_class
  filter_main_group <- aux$filter_main_group
  main_group_col_name <- aux$main_group_col_name

  ngrams_n <- ifelse(ngrams_type == "Unigrams", 1, 2)

  tfidf_ngrams <- x %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(main_group_col_name),
        ~ . %in% filter_main_group
      )
    ) %>%
    tidytext::unnest_tokens(ngram, !! rlang::sym(text_col_name),
                            token = "ngrams", n = ngrams_n) %>%
    tidyr::separate(ngram, paste0("word", 1:ngrams_n), sep = " ") %>%
    dplyr::filter( # Do this because some stop words make it through the TF-IDF filtering that happens below.
      dplyr::across(
        dplyr::starts_with("word"),
        ~ !. %in% tidytext::stop_words$word
      )
    ) %>%
    tidyr::unite(col = "ngram", paste0("word", 1:ngrams_n), sep = " ") %>%
    dplyr::count(dplyr::across(dplyr::all_of(target_col_name)), ngram, sort = TRUE) %>%
    tidytext::bind_tf_idf(ngram, !! rlang::sym(target_col_name), n) %>% # {{}} doesn't work here. Use !! instead.
    dplyr::group_by(dplyr::across(dplyr::all_of(target_col_name))) %>%
    dplyr::slice_max(tf_idf, n = 15) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ . %in% filter_class
      )
    )

  return(tfidf_ngrams)
}
