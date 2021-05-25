#' Prepare column names and filtering vectors for use in tables
#'
#' Internal function
#'
#' @param x
#' @param grouping_variables
#' @param target_col_name
#' @param filter_class
#' @param filter_main_group
#' @param column_names
#'
#' @return
#' @export
#'
#' @examples

prep_colnames_and_filters <- function(x = NULL, grouping_variables = NULL,
                                      target_col_name, filter_class = NULL,
                                      # main_group_col_name = NULL,
                                      filter_main_group = NULL,
                                      column_names = NULL) {

  main_group_col_name <- grouping_variables[1]

  # testthat::test_that(
  #   "With column_names == 'px', grouping_variables must be non-empty.",
  #   {
  #     testthat::expect_false(column_names == "px" & is.null(main_group_col_name))
  #   }
  # )
  #
  # testthat::test_that(
  #   "Argument filter_main_group cannot be NULL when main_group_col_name is
  #   non-empty.",
  #   {
  #     testthat::expect_false(is.null(filter_main_group) & !is.null(main_group_col_name))
  #   }
  # )

  if (all(is.null(column_names))) {

    column_names <- c(grouping_variables, target_col_name, "accuracy")
  }
  column_names <- column_names[!is.null(column_names)] # grouping_variables can be NULL.

  if (is.null(filter_class)) {
    filter_class <- sort(unique(x[[target_col_name]]))
  }

  if (is.null(filter_main_group) & !is.null(main_group_col_name) & !is.null(x)) {
    filter_main_group <- sort(unique(x[[main_group_col_name]]))
  }

  re <- list(
    column_names = column_names,
    filter_class = filter_class,
    filter_main_group = filter_main_group,
    main_group_col_name = main_group_col_name
  )

  return(re)
}
