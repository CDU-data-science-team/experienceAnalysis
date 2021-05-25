#' Prepare table with the best (hyper)parameter tunings for each learner
#'
#' Internal function
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

prep_best_estimators <- function(x) {

  best_estimators <- x %>%
    dplyr::mutate(learner = sub("\\(.*", "", param_clf__estimator)) %>%
    dplyr::group_by(learner) %>%
    dplyr::arrange(
      dplyr::desc(tidyselect::all_of("mean_test_Class Balance Accuracy"))
    ) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(tidyselect::starts_with(c("mean_test"))) %>%
    dplyr::mutate(
      name = sub("mean_test.", "", name),
      # Auxiliary column to order learners by CBA in bar plots
      aux = dplyr::case_when(
        name %in% "Class Balance Accuracy" ~ value,
        TRUE ~ -1
      )
    )

  return(best_estimators)
}
