#' Title
#'
#' @param x
#' @param target_col_name
#'
#' @return
#' @export
#'
#' @examples

get_tidy_feedback <- function(x, target_col_name) {

  tidy_feedback <- x %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(target_col_name, "organization")))
    ) %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>%
    tidytext::unnest_tokens(word, feedback) %>%
    dplyr::ungroup()

  return(tidy_feedback)
}
