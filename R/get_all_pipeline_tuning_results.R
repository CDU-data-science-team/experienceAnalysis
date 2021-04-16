#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

get_all_pipeline_tuning_results <- function(x) {

  pipeline_tuning_results <- x %>%
    dplyr::select(
      -tidyselect::any_of("X1"),
      -dplyr::starts_with(c("split", "rank")),
      -params,
      -param_clf__estimator,
    ) %>%
    dplyr::select(
      learner,
      dplyr::starts_with("param"),
      dplyr::contains(c("class balance", "balanced", "matthews"),
                      ignore.case = TRUE),
      dplyr::ends_with("_Accuracy")
    ) %>%
    dplyr::arrange(
      dplyr::desc(tidyselect::all_of("mean_test_Class Balance Accuracy"))
    ) %>%
    dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric),
                                ~ round(., 2)))

  return(pipeline_tuning_results)
}
