#' Counts of words with a positive or negative sentiment
#'
#' Count the number of times a word with a positive or negative sentiment occurs
#' in a given text.
#'
#' @param x A data frame with one or more columns: the column with the classes
#'     (if `target_col_name` is not `NULL`); and the column with the text. Any
#'     other columns will be ignored.
#' @param target_col_name A string with the column name of the target variable.
#'     Defaults to `NULL`.
#' @param text_col_name A string with the column name of the text variable.
#' @param filter_class A string or vector of strings with the name(s) of the
#'     class(es) for which to count the words. Defaults to
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
#'             ~ calc_bing_word_counts(., target_col_name = NULL,
#'                                    text_col_name = "feedback",
#'                                    filter_class = NULL)
#'         )
#'
#' @return A data frame with three columns: word; sentiment ("positive" or
#'     "negative"- see Hu & Liu, 2004); and count.
#' @export
#'
#' @examples
#' @references Hu M. & Liu B. (2004). Mining and summarizing customer
#'     reviews. Proceedings of the ACM SIGKDD International Conference on
#'     Knowledge Discovery & Data Mining (KDD-2004), Seattle, Washington, USA,
#'     Aug 22-25, 2004.

calc_bing_word_counts <- function(x, target_col_name = NULL, text_col_name,
                                  filter_class = NULL) {

  tidy_feedback <- experienceAnalysis::prep_tidy_feedback(x, target_col_name,
                                                          text_col_name)

  # Most common positive and negative words
  bing_word_counts <- tidy_feedback %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ experienceAnalysis::tidy_class_filter(., filter_class)
      )
    ) %>%
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
    dplyr::count(word, sentiment, sort = TRUE)

  return(bing_word_counts)
}
