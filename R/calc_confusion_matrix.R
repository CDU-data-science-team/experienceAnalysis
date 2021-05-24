#' Title
#'
#' @param x
#' @param target_col_name
#' @param target_pred_col_name
#' @param grouping_variables
#'
#' @return
#' @export
#'
#' @examples

calc_confusion_matrix <- function(x, target_col_name, target_pred_col_name,
                                  grouping_variables = NULL) {


  if (!is.null(grouping_variables)) {

    cm <- x %>%
      dplyr::select(dplyr::all_of(c(grouping_variables, target_col_name,
                           target_pred_col_name))) %>%
      dplyr::mutate(
        dplyr::across(
          -dplyr::all_of(grouping_variables),
          ~ as.factor(.)
        )
      ) %>%
      split(f = .[[grouping_variables]]) %>%
      purrr::map(
        ~ yardstick::conf_mat(., {{target_col_name}}, {{target_pred_col_name}})
      )
  } else {

    cm <- x %>%
      dplyr::select(dplyr::all_of(c(target_col_name, target_pred_col_name))) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ as.factor(.)
        )
      ) %>%
      yardstick::conf_mat(., {{target_col_name}}, {{target_pred_col_name}}) %>%
      list() # Convert to list to ensure returned object class is consistent.
  }

  return(cm)
}
