#' Predict unlabelled text using a fitted Scikit-learn pipeline
#'
#' For internal use only!
#'
#' @param x A data frame with the text data to predict classes for. If
#'     `file_path` (see below) is not `NULL`, then `x` should be `NULL`.
#' @param sys_setenv A string in the form "path_to_python/python.exe",
#'     indicating which Python to use (e.g. from a virtual environment).
#' @param which_python Same as `sys_setenv`.
#' @param which_venv A string that can be "conda", "miniconda" or "python".
#' @param venv_name String. The name of the virtual environment.
#' @param file_path A string in the form "path_to_data/filename.csv", where
#'     "filename" is the name of the CSV file with the data. If `NULL`, the
#'     function uses x.
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
#'     `preds_column.`
#'
#' @return
#' @export
#'
#' @examples

calc_predict_unlabelled_text <- function(x, sys_setenv, which_python, which_venv,
                                         venv_name,
                                         file_path = NULL,
                                         text_col_name, pipe_path,
                                         preds_column = NULL,
                                         column_names = NULL) {

  Sys.setenv(RETICULATE_PYTHON = sys_setenv)
  reticulate::use_python(which_python)

  if (which_venv == 'conda') {
    reticulate::use_condaenv(venv_name, required = TRUE)
  } else if (which_venv == 'miniconda') {
    reticulate::use_miniconda(venv_name, required = TRUE)
  } else if (which_venv == 'python') {
    reticulate::use_virtualenv(venv_name, required = TRUE)
  }

  factory_predict_unlabelled_text_r <-
    reticulate::py_run_string(
      "from pxtextmining.factories.factory_predict_unlabelled_text import factory_predict_unlabelled_text"
    )$
    factory_predict_unlabelled_text

  if (is.null(file_path)) {
    x_py <- reticulate::r_to_py(x)

    predictions <- factory_predict_unlabelled_text_r(
      file_path=file_path, dataset=x_py, predictor=text_col_name,
      pipe_path=pipe_path, preds_column=preds_column,
      column_names=reticulate::r_to_py(column_names))
  } else {
    predictions <- factory_predict_unlabelled_text_r(
      file_path=file_path, dataset=NULL, predictor=text_col_name,
      pipe_path=pipe_path, preds_column=preds_column,
      column_names=reticulate::r_to_py(column_names))
  }

  return(predictions)
}
