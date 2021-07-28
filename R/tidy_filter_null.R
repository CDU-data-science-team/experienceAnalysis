#' Filter data frame when filter can be `NULL`
#'
#' For use inside `dplyr::filter()` operations where sometimes it is desired to
#' filter nothing.
#'
#' @param x A string or vector of strings with the column name(s) to apply the
#'     filter on.
#' @param filter_value A value (or vector when `filter_how == "in"`) to filter
#'     by.
#' @param filter_how A string with the filtering operation: "==", ">=", ">",
#'     "<=", "<" or "in", where "in" is for applying vector filters with `%in%`.
#'
#'@details This function is useful when there is a `dplyr::filter()` operation
#'   on a data frame but it is not always desirable to filter the data frame.
#'   A good example is a `Shiny` dashboard that returns a plot or table of the
#'   filtered data or of the whole dataset, depending on the input from the
#'   user.
#'
#' @return The filtered data frame or the original data frame when
#'     `filter_value` is `NULL` or `NA`.
#' @export
#'
#' @examples
#' library(experienceAnalysis)
#' # Filter gear and carb to be 2 or 3
#' mtcars %>%
#'   dplyr::filter(
#'     dplyr::across(
#'       c("gear", "carb"),
#'       ~ experienceAnalysis::tidy_filter_null(., filter_value = 2:3,
#'                                               filter_how = "in")
#'     )
#' )
#'
#' # Filter nothing (returns original data frame)
#' mtcars %>%
#'   dplyr::filter(
#'     dplyr::across(
#'       c("gear", "carb"),
#'       ~ experienceAnalysis::tidy_filter_null(., filter_value = NULL)
#'     )
#' )

tidy_filter_null <- function(x, filter_value = NULL,
                              filter_how = c("==", ">=", ">", "<=", "<", "in"))
  {
  if (!all(is.null(filter_value)) & !all(is.na(filter_value))) {

    if (filter_how == "in") {
      x %in% filter_value
    } else if (filter_how == "==") {
      x == filter_value
    } else if (filter_how == ">=") {
      x >= filter_value
    } else if (filter_how == ">") {
      x > filter_value
    } else if (filter_how == "<=") {
      x <= filter_value
    } else if (filter_how == "<") {
      x < filter_value
    }
  } else {
    TRUE
  }
}
