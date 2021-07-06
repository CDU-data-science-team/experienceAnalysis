#' Order sentiment occurrence table by sentiment counts
#'
#' Order result of \code{\link{calc_net_sentiment_per_tag}} by the
#' count of one or more NRC sentiments, and convert to "tidy" format.
#'
#' @param net_sentiment_wide_nrc A data frame/tibble derived from
#'     \code{\link{calc_net_sentiment_per_tag}}.
#' @param sorting_sentiments A string of vector of strings with the NRC
#'     sentiments by which to sort the returned table. (To see all NRC
#'     sentiments run \code{\link{prep_sentiments_nrc}}). Order matters:
#'     `c("anger", "fear")` will sort by "anger" first. Conversely,
#'     `c("fear", "anger")` will sort by "fear" first.
#' @param num_of_lines The number of records to return.
#'
#' @return A data frame in long format with four columns: the text; the line
#'     number; the NRC sentiment; and the count of the NRC sentiment.
#' @export
#'
#' @examples

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
