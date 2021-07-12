#' Calculate TF-IDFs for unigrams or bigrams
#'
#' For a given labelled text, return the unigrams or bigrams with the
#' largest TF-IDFs for the given class(es).
#'
#' @param x A data frame with two columns: the column with the classes; and the
#'     column with the text.
#' @param target_col_name A string with the column name of the target variable.
#'     It is equivalent to argument `document` in `bind_tf_idf{tidytext}`.
#' @param text_col_name A string with the column name of the text variable.
#' @param filter_class A string or vector of strings with the name(s) of the
#'     class(es) for which TF-IDFs are to be calculated. Defaults to
#'     `NULL` (all classes).
#' @param ngrams_type A string. Should be "Unigrams" for unigrams and "Bigrams"
#'     for bigrams.
#' @param number_of_ngrams Integer. Number of ngrams to return. Defaults to all.
#'
#' @note Unlike other functions in `experienceAnalysis` (e.g.
#'     \code{\link{calc_net_sentiment_nrc}}), here it does not make
#'     sense to have `target_col_name` set to `NULL`- the TF-IDF of an n-gram
#'     depends on the number of "documents" containing it (see Silge and
#'     Robinson, 2017), so there must be at least two classes (or "documents")
#'     to use in the calculations. \cr\cr
#'     When `filter_class` is not `NULL`, the TF-IDFs will _still_ be
#'     calculated using _all_ classes/documents and _then_ filtered by
#'     `filter_class`.
#'
#' @return A data frame with six columns: class; n-gram (word or bigram); count;
#'     term-frequency; inverse document frequency; and TF-IDF.
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
#' # Get a few of the bigram counts, TFs, IDFs and highest TF-IDFs for each book
#' calc_tfidf_ngrams(x, target_col_name = "book", text_col_name = "text",
#'                   filter_class = NULL,
#'                   ngrams_type = "Bigrams",
#'                   number_of_ngrams = 30
#' ) %>%
#' split(.$book)
#' @references Silge J. & Robinson D. (2017). Text Mining with R: A Tidy
#'     Approach. Sebastopol, CA: O’Reilly Media. ISBN 978-1-491-98165-8.

calc_tfidf_ngrams <- function(x, target_col_name, text_col_name,
                              filter_class = NULL,
                              ngrams_type = c("Unigrams", "Bigrams"),
                              number_of_ngrams = NULL) {

  ngrams_n <- ifelse(ngrams_type == "Unigrams", 1, 2)

  number_of_ngrams <- ifelse(
    is.null(number_of_ngrams),
    nrow(x),
    number_of_ngrams
  )

  tfidf_ngrams <- x %>%
    tidytext::unnest_tokens(
      ngram,
      !! rlang::sym(text_col_name),
      token = "ngrams",
      n = ngrams_n
    ) %>%
    tidyr::separate(
      ngram,
      paste0("word", 1:ngrams_n),
      sep = " "
    ) %>%
    dplyr::filter( # Do this because some stop words make it through the TF-IDF filtering that happens below.
      dplyr::across(
        dplyr::starts_with("word"),
        ~ !. %in% tidytext::stop_words$word
      )
    ) %>%
    tidyr::unite(
      col = "ngram", paste0("word", 1:ngrams_n),
      sep = " "
    ) %>%
    dplyr::count(
      dplyr::across(
        dplyr::all_of(target_col_name)
      ),
      ngram,
      sort = TRUE
    ) %>%
    tidytext::bind_tf_idf(
      ngram,
      !! rlang::sym(target_col_name), # {{}} doesn't work here. Use !! instead.
      n
    ) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(target_col_name)
      )
    ) %>%
    dplyr::slice_max(tf_idf, n = 15) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      dplyr::across(
        dplyr::all_of(target_col_name),
        ~ experienceAnalysis::tidy_filter_null(., filter_class,
                                               filter_how = "in")
      )
    ) %>%
    dplyr::slice(1:number_of_ngrams)

  return(tfidf_ngrams)
}
