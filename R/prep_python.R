#' Set up a Python virtual environment
#'
#' Set up a Python virtual environment to run R functions that use `reticulate`
#' to run Python in the background.
#'
#' @param sys_setenv A string in the form "path_to_python/python.exe",
#'     indicating which Python to use (e.g. from a virtual environment).
#' @param which_python Same as `sys_setenv`.
#' @param which_venv A string that can be "conda", "miniconda" or "python".
#' @param venv_name String. The name of the virtual environment.
#'
#' @return Nothing- it just sets up the Python virtual environment invisibly.
#' @export
#'
#' @examples
#'
prep_python <- function(sys_setenv, which_python, which_venv, venv_name) {

  Sys.setenv(RETICULATE_PYTHON = sys_setenv)
  reticulate::use_python(which_python)

  if (which_venv == 'conda') {
    reticulate::use_condaenv(venv_name, required = TRUE)
  } else if (which_venv == 'miniconda') {
    reticulate::use_miniconda(venv_name, required = TRUE)
  } else if (which_venv == 'python') {
    reticulate::use_virtualenv(venv_name, required = TRUE)
  }
}
