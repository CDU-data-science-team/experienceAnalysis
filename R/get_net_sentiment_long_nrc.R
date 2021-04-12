#' Title
#'
#' @return
#' @export
#'
#' @examples

get_net_sentiment_long_nrc <- function(net_sentiment_wide_nrc,
                                       sorting_sentiments = "anger",
                                       num_of_facets = 60) {

  nrc_sentiments <- get_sentiments_nrc()

  net_sentiment_long_nrc <- net_sentiment_wide_nrc %>%
    dplyr::arrange(
      dplyr::across(sorting_sentiments, dplyr::desc)
    ) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(nrc_sentiments)) %>%
    dplyr::filter(value != 0) %>%
    dplyr::filter(linenumber %in%
                    unique(.$linenumber)[1:num_of_facets]) %>%
    dplyr::mutate(
      name = factor(name, levels = sort(nrc_sentiments, decreasing = TRUE)),
      linenumber = factor(linenumber, levels = unique(.$linenumber))
    )

  return(net_sentiment_long_nrc)
}
