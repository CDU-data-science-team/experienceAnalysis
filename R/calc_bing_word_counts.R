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
#'     separation is desired, then run the function for each class separately or
#'     do something like this:
#'
#'     # Assuming that the class and text columns are called "label" and
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
#' # Word counts for both books
#' calc_bing_word_counts(x, target_col_name = "book", text_col_name = "text",
#'                       filter_class = NULL) %>%
#'   head()
#'
#' # Word counts for Emma
#' calc_bing_word_counts(x, target_col_name = "book", text_col_name = "text",
#'                       filter_class = "Emma") %>%
#'   head()
#'
#' # Word counts for Pride & Prejudice
#' calc_bing_word_counts(x, target_col_name = "book", text_col_name = "text",
#'                       filter_class = "Pride & Prejudice") %>%
#'   head()
#'
#' @references Hu M. & Liu B. (2004). Mining and summarizing customer
#'     reviews. Proceedings of the ACM SIGKDD International Conference on
#'     Knowledge Discovery & Data Mining (KDD-2004), Seattle, Washington, USA,
#'     Aug 22-25, 2004.

calc_bing_word_counts <- function(x, target_col_name = NULL, text_col_name,
                                  filter_class = NULL) {

  # check and prep bing dictionary

  bing <- get_dictionary("bing")

  tidy_text <- experienceAnalysis::prep_tidy_text(x, target_col_name,
                                                          text_col_name)

  # Most common positive and negative words
  bing_word_counts <- tidy_text %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ experienceAnalysis::tidy_filter_null(., filter_class,
                                               filter_how = "in")
      )
    ) %>%
    dplyr::inner_join(bing, by = "word") %>%
    dplyr::count(word, sentiment, sort = TRUE)

  return(bing_word_counts)
}
