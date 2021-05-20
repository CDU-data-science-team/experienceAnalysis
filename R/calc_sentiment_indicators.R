#' Title
#'
#' @param x
#' @param sys_setenv
#' @param which_python
#' @param which_venv
#' @param venv_name
#' @param make_table
#'
#' @return
#' @export
#'
#' @examples

calc_sentiment_indicators <- function(x, sys_setenv, which_python, which_venv,
                                     venv_name, make_table = TRUE) {

  Sys.setenv(RETICULATE_PYTHON = sys_setenv)
  reticulate::use_python(which_python)

  if (which_venv == 'conda') {
    reticulate::use_condaenv(venv_name, required = TRUE)
  } else if (which_venv == 'miniconda') {
    reticulate::use_miniconda(venv_name, required = TRUE)
  } else if (which_venv == 'python') {
    reticulate::use_virtualenv(venv_name, required = TRUE)
  }

  sentiment_scores <- reticulate::py_run_string(
    "from pxtextmining.helpers.sentiment_scores import sentiment_scores"
  )

  polarity_textblob <- x %>%
    dplyr::rename(predictor = feedback) %>%
    sentiment_scores$sentiment_scores() %>%
    dplyr::select(text_blob_polarity) %>%
    dplyr::rename(polarity = text_blob_polarity) %>%
    dplyr::mutate(row_index = x$row_index)

  if (make_table) {

    polarity_textblob <- polarity_textblob %>%
      dplyr::left_join(x) %>%
      dplyr::filter(
        dplyr::across(dplyr::any_of("super"), ~ . != "Couldn't be improved")
      ) %>%
      dplyr::select(feedback, polarity, organization, label, criticality) %>%
      dplyr::mutate(
        polarity = round(polarity, 2),
        criticality = dplyr::case_when(
          !criticality %in% -5:5 ~ "Unassigned",
          TRUE ~ criticality
        )
      )
  }

  return(polarity_textblob)
}
