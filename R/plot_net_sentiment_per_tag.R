#' Plot "net sentiment" in a text
#'
#' Plot the difference between "net positive" and "net negative" sentiment in
#' a text for each sentiment dictionary and class (if any).
#'
#' @param net_sentiment_all_dicts A data frame from
#'     \code{\link{calc_net_sentiment_per_tag}}.
#' @param target_col_name A string with the column name of the target variable.
#' @param title Plot title. Defaults to `NULL`.
#'
#' @return A `ggplot` (`ggplot::geom_col`).
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
# Net sentiment in each book for each dictionary, sorted in descending order
#' calc_net_sentiment_per_tag(x, target_col_name = "book",
#'                            text_col_name = "text") %>%
#'   plot_net_sentiment_per_tag(
#'       target_col_name = "book",
#'       title = "Net sentiment per book for each dictionary"
#'   )
#'
#' calc_net_sentiment_per_tag(x, target_col_name = NULL,
#'                            text_col_name = "text") %>%
#'   plot_net_sentiment_per_tag(
#'       target_col_name = NULL,
#'       title = "Net sentiment in text for each dictionary"
#'   )

plot_net_sentiment_per_tag <- function(net_sentiment_all_dicts, target_col_name) {

  p <- net_sentiment_all_dicts %>%
    ggplot2::ggplot(
      ggplot2::aes(
        sentiment,
        reorder(.data[[target_col_name]], sentiment) # Don't know how to do it with non-standard evaluation.
      )
    ) +
    ggplot2::geom_col(fill = 'blue', alpha = 0.6) +
    ggplot2::facet_wrap(~ method, ncol = 1, scales = "free") +
    ggplot2::labs(
      x = "Net sentiment",
      y = NULL,
      title = "Net sentiment per tag"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.text.y = ggplot2::element_text(size = 12)
    )

  return(p)
}
