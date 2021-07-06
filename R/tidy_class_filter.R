#' Filter data frame by the supplied string vector
#'
#' For use inside `dplyr::filter()` operations in different `experienceAnalysis`
#' functions.
#'
#' @param x
#' @param filter_class
#'
#' @return
#' @export
#'
#' @examples

tidy_class_filter <- function(x, filter_class = NULL) {
  if (!all(is.null(filter_class))) {
    x %in% filter_class
  } else {
    TRUE
  }
}
