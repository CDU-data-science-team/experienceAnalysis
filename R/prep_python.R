#' Title
#'
#' @param sys_setenv
#' @param which_python
#' @param which_venv
#' @param venv_name
#'
#' @return
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
