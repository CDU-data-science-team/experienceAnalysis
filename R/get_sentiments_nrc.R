#' Pulls NRC Sentiments
#'
#' @return Character vector of all NRC sentiments
#' @export
#'
#' @examples

get_sentiments_nrc <- function() {

  nrc_sentiments <- tidytext::get_sentiments("nrc") %>%
    dplyr::select(sentiment) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    sort()

  return(nrc_sentiments)
}
