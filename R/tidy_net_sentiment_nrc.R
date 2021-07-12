#' Order sentiment occurrence table by sentiment counts
#'
#' Order result of \code{\link{calc_net_sentiment_nrc}} by the
#' count of one or more NRC sentiments, and convert to "tidy" format.
#'
#' @param net_sentiment_wide_nrc A data frame/tibble derived from
#'     \code{\link{calc_net_sentiment_nrc}}.
#' @param sorting_sentiments A string of vector of strings with the NRC
#'     sentiments (Mohammad & Turney, 2013) by which to sort the returned table.
#'     (To see all NRC sentiments run \code{\link{prep_sentiments_nrc}}). Order
#'     matters: `c("anger", "fear")` will sort by "anger" first. Conversely,
#'     `c("fear", "anger")` will sort by "fear" first.
#' @param num_of_docs The number of document IDs for which to return results.
#'     See details.
#'
#' @details The returned data frame will have, for each document (or class- see
#'     `filter_class` in \code{\link{calc_net_sentiment_nrc}}), as many rows
#'     as the NRC sentiments with non-zero counts (remember- this function
#'     returns the result in _long_ format). If all sentiments have non-zero
#'     counts in a given document, then there will be 10 rows (as many as all
#'     the NRC sentiments) in the returned data frame for this document.
#'     Argument `num_of_docs` returns the first `num_of_docs` documents
#'     (or classes, see `filter_class` in \code{\link{calc_net_sentiment_nrc}}),
#'     with the top one being the one that has the highest count(s) of the NRC
#'     sentiment(s) specified in `sorting_sentiments`.
#'
#' @return A data frame in long format with four columns: the text; the line
#'     number; the NRC sentiment; and the count of the NRC sentiment.
#' @export
#'
#' @references Mohammad S.M. & Turney P.D. (2013). Crowdsourcing a Word–Emotion
#'     Association Lexicon. Computational Intelligence, 29(3):436-465.
#'
#' @examples
#' library(experienceAnalysis)
#' books <- janeaustenr::austen_books() # Jane Austen books
#' emma <- paste(books[books$book == "Emma", ], collapse = " ") # String with whole book
#' pp <- paste(books[books$book == "Pride & Prejudice", ], collapse = " ") # String with whole book
#'
#' # Make data frame with books Emma and Pride & Prejudice
#' x <- data.frame(
#'   text = c(pp, emma),
#'   book = c("Pride & Prejudice", "Emma")
#' )
#'
#' # Net sentiment in each book
#' net_sentiment_wide_nrc <- calc_net_sentiment_nrc(x, target_col_name = "book",
#'                                                  text_col_name = "text",
#'                                                  filter_class = NULL)
#'
#' net_sentiment_wide_nrc
#'
#' # Tidy net_sentiment_wide_nrc and place the most "angry" book at the top
#' tidy_net_sentiment_nrc(net_sentiment_wide_nrc,
#'                        sorting_sentiments = "anger",
#'                        num_of_docs = 2) %>%
#'   dplyr::select(-text)
#'
#' # Can sort by multiple sentiments too: as above, but for most "angry" and then for most "negative"
#' tidy_net_sentiment_nrc(net_sentiment_wide_nrc,
#'                        sorting_sentiments = c("anger", "negative"),
#'                        num_of_docs = 2) %>%
#'   dplyr::select(-text)

tidy_net_sentiment_nrc <- function(net_sentiment_wide_nrc,
                                       sorting_sentiments = "anger",
                                       num_of_lines = 60) {

  nrc_sentiments <- experienceAnalysis::prep_sentiments_nrc()

  net_sentiment_long_nrc <- net_sentiment_wide_nrc %>%
    dplyr::arrange(
      dplyr::across(sorting_sentiments, dplyr::desc)
    ) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(nrc_sentiments)) %>%
    dplyr::filter(value != 0) %>%
    dplyr::filter(linenumber %in%
                    unique(.$linenumber)[1:num_of_lines]) %>%
    dplyr::mutate(
      name = factor(name, levels = sort(nrc_sentiments, decreasing = TRUE)),
      linenumber = factor(linenumber, levels = unique(.$linenumber))
    )

  return(net_sentiment_long_nrc)
}
