#' Unnest tokens for each label in a labelled text
#'
#' Creates table with tokens for each class (if any).
#'
#' @param x x A data frame with one or more columns: the column with the classes
#'     (if `target_col_name` is not `NULL`); and the column with the text. Any
#'     other columns will be ignored.
#' @param target_col_name A string with the column name of the target variable.
#'     Defaults to `NULL`.
#' @param text_col_name A string with the column name of the text variable.
#'
#' @return A data frame with two or three columns: classes (if `target_col_name`
#'     is not `NULL`); line numbers; and tokens.
#' @export
#'
#' @examples
#' #' library(experienceAnalysis)
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
#' # Tokens for both books, without the `target_col_name` column
#' prep_tidy_text(x, target_col_name = NULL, text_col_name = "text") %>%
#'     head()
#'
#' # Tokens for both books, with the `target_col_name` column this time
#' prep_tidy_text(x, target_col_name = "book", text_col_name = "text") %>%
#'     split(.$book) %>%
#'     purrr::map(~ head(.))
#'
#' # Tokens for Pride & Prejudice
#' prep_tidy_text(x, target_col_name = "book", text_col_name = "text") %>%
#'     dplyr::filter(book == "Pride & Prejudice") %>%
#'     head()


prep_tidy_text <- function(x, target_col_name = NULL, text_col_name) {

  tidy_text <- x %>%
    dplyr::select(
      dplyr::all_of(c(target_col_name, text_col_name))
    ) %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>%
    tidytext::unnest_tokens(word, !! rlang::sym(text_col_name)) %>%
    dplyr::ungroup()

  return(tidy_text)
}
