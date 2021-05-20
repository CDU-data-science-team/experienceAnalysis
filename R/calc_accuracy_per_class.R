#' Title
#'
#' @param x
#' @param target_col_name
#' @param target_pred_col_name
#' @param grouping_variable
#'
#' @return
#' @export
#'
#' @examples

calc_accuracy_per_class <- function(x, target_col_name, target_pred_col_name,
                                    grouping_variable = NULL) {

  accuracy_per_class <- x %>%
    dplyr::mutate(actual_vs_predicted =
                    .[[target_col_name]] == .[[target_pred_col_name]]) %>%
    dplyr::filter(!is.na(actual_vs_predicted)) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(grouping_variable, target_col_name))
      )
    ) %>%
    dplyr::summarise(accuracy = sum(actual_vs_predicted) /
                       length(actual_vs_predicted)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      class = {{target_col_name}},
      organization = {{grouping_variable}}
    )

  return(accuracy_per_class)
}
