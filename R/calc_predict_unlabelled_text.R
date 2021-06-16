#' Predict unlabelled text using a fitted Scikit-learn pipeline
#'
#' For internal use only!
#'
#' @param x A data frame with the text data to predict classes for.
#' @param python_setup A `logical` whether to set up the `Python` version,
#'     virtual environment etc. that can be controlled with arguments
#'     `sys_setenv`, `which_python`, `which_venv` and `venv_name`. These
#'     arguments will be ignored when `python_setup` is `FALSE`. The purpose of
#'     `python_setup` is that users may wish to control the `Python` parameters
#'     outside the actual function, for the session in general.
#' @param sys_setenv A string in the form "path_to_python/python.exe",
#'     indicating which Python to use (e.g. from a virtual environment).
#' @param which_python Same as `sys_setenv`.
#' @param which_venv A string that can be "conda", "miniconda" or "python".
#' @param venv_name String. The name of the virtual environment.
#' @param text_col_name A string with the column name of the text variable.
#' @param pipe_path A string in the form "path_to_fitted_pipeline/pipeline.sav,"
#'     where "pipeline" is the name of the SAV file with the fitted
#'     `Scikit-learn` pipeline.
#' @param preds_column A string with the user-specified name of the column that
#'     will have the predictions. If `NULL` (default), then the name will be
#'     `paste0(text_col_name, "_preds")`.
#' @param column_names A vector of strings with the names of the columns of the
#'     supplied data frame (incl. `text_col_name`) to be added to the returned data
#'     frame. If `NULL`, then the only column in the returned data frame will be
#'     `preds_column.` Defaults to "__all__".
#'
#' @return
#' @export
#'
#' @examples

calc_predict_unlabelled_text <- function(x, python_setup = TRUE, sys_setenv,
                                         which_python, which_venv, venv_name,
                                         text_col_name, pipe_path,
                                         preds_column = NULL,
                                         column_names = NULL) {

  if (python_setup) {
    Sys.setenv(RETICULATE_PYTHON = sys_setenv)
    reticulate::use_python(which_python)

    if (which_venv == 'conda') {
      reticulate::use_condaenv(venv_name, required = TRUE)
    } else if (which_venv == 'miniconda') {
      reticulate::use_miniconda(venv_name, required = TRUE)
    } else if (which_venv == 'python') {
      reticulate::use_virtualenv(venv_name, required = TRUE)
    }

  if (all(column_names == '__all__')) {
    column_names <- names(x)
  }

  factory_predict_unlabelled_text_r <-
    reticulate::py_run_string(
      "from pxtextmining.factories.factory_predict_unlabelled_text import factory_predict_unlabelled_text"
    )$
    factory_predict_unlabelled_text

  predictions <- factory_predict_unlabelled_text_r(
    dataset=reticulate::r_to_py(x),
    predictor=text_col_name,
    pipe_path=pipe_path,
    preds_column=preds_column,
    column_names=reticulate::r_to_py(column_names)
  )

  return(predictions)
}
