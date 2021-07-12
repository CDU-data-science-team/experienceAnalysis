#' Create and count bigrams
#'
#' For a given labelled text, create and calculate the most frequently occurring
#' bigrams (no stop words) for the given class(es).
#'
#' @param x A data frame with one or more columns: the column with the classes
#'     (if `target_col_name` is not `NULL`); and the column with the text. Any
#'     other columns will be ignored.
#' @param target_col_name A string with the column name of the target variable.
#'     Defaults to `NULL`.
#' @param text_col_name A string with the column name of the text variable.
#' @param filter_class A string or vector of strings with the name(s) of the
#'     class(es) for which bigrams are to be created and counted. Defaults to
#'     `NULL` (all rows).
#' @param bigrams_prop A numeric in (0, 100] indicating the percentage of the
#'     most frequent bigrams to keep.
#'
#' @note When supplying more than one class in `filter_class`, the returned data
#'     frame will NOT separate the results for the different classes. If
#'     separation is desired, then run the function for each class separately or
#'     do something like this:
#'
#'     # Assuming that the class and text columns are called "label" and
#'     # "feedback" respectively
#'     x %>%
#'         split(.$label) %>%
#'         purrr::map(
#'             ~ calc_bigrams_network(., target_col_name = NULL,
#'                                    text_col_name = "feedback",
#'                                    filter_class = NULL, bigrams_prop = 50)
#'         )
#'
#' @return A data frame with three columns: first word of bigram;
#'     second word of bigram; and bigram count.
#' @export
#'
#' @examples
#' library(experienceAnalysis)
#' books <- janeaustenr::austen_books() # Jane Austen books
#' emma <- paste(books[books$book == "Emma", ], collapse = " ") # String with whole book
#' pp <- paste(books[books$book == "Pride & Prejudice", ], collapse = " ") # String with whole book
#'
#' # Make data frame with books Emma and Pride & Prejudice
#' x <- data.frame(
#'   text = c(emma, pp),
#'   book = c("Emma", "Pride & Prejudice")
#' )
#'
#' # Bigrams for both books
#' calc_bigrams_network(x, target_col_name = "book", text_col_name = "text",
#'                      filter_class = NULL, bigrams_prop = 3)
#'
#' # Bigrams for Emma
#' calc_bigrams_network(x, target_col_name = "book", text_col_name = "text",
#'                      filter_class = "Emma", bigrams_prop = 3)
#'
#' # Bigrams for Pride & Prejudice
#' calc_bigrams_network(x, target_col_name = "book", text_col_name = "text",
#'                      filter_class = "Pride & Prejudice", bigrams_prop = 3)

calc_bigrams_network <- function(x, target_col_name, text_col_name,
                                 filter_class = NULL, bigrams_prop) {

  bigrams_table <- x %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ experienceAnalysis::tidy_class_filter(., filter_class)
      )
    ) %>%
    tidytext::unnest_tokens(
      bigram,
      !! rlang::sym(text_col_name),
      token = "ngrams",
      n = 2
    ) %>%
    tidyr::separate(
      bigram,
      c("word1", "word2"),
      sep = " "
    ) %>%
    dplyr::filter(
      dplyr::across(
        dplyr::starts_with("word"),
        ~ !. %in% tidytext::stop_words$word
      )
    ) %>%
    dplyr::count(word1, word2, sort = TRUE) %>%
    dplyr::filter(
      dplyr::across(
        dplyr::starts_with("word"),
        ~ !is.na(.)
      ),
      n > 1
    ) %>%
    dplyr::slice_max(prop = bigrams_prop / 100, order_by = n)

  return(bigrams_table)
}
