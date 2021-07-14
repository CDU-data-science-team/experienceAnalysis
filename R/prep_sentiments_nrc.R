#' Pulls NRC Sentiments
#'
#' @return Character vector of all NRC sentiments
#' @export
#'
#' @examples prep_sentiments_nrc()

prep_sentiments_nrc <- function() {

  nrc <- get_dictionary("nrc")

  nrc_sentiments <- nrc %>%
    dplyr::select(sentiment) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    sort()

  return(nrc_sentiments)
}
