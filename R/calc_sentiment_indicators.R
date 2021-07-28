#' Calculate text sentiment indicators using Python packages
#'
#' Uses Python's `TextBlob` and `vaderSentiment` packages to calculate several
#' indicators of sentiment in a text.
#'
#' @param python_setup A `logical` whether to set up the `Python` version,
#'     virtual environment etc. that can be controlled with arguments
#'     `sys_setenv`, `which_python`, `which_venv` and `venv_name`. These
#'     arguments will be ignored when `python_setup` is `FALSE`. The purpose of
#'     `python_setup` is that users may wish to control the `Python` parameters
#'     outside the actual function, for the session in general.
#' @param x A data frame with a column that has the text. Any other columns will
#'     be ignored.
#' @param sys_setenv A string with the path to the Python version to use in this
#'     session.
#' @param which_python A string with the path to the Python version to use in
#'     this session.
#' @param which_venv A string that should be "conda", "miniconda" or "python".
#'     See `use_python{reticulate}`.
#' @param venv_name A string with the name of the Python virtual environment to
#'     use in this session.
#' @param text_col_name A string with the column name of the text variable.
#'
#' @details `TextBlob` calculates two indicators:
#'     \itemize{
#'         \item{_Polarity_, which ranges from -1 (most negative)
#'             to 1 (most positive). Neutral text gets polarity of 0 or near it;}
#'         \item{_Subjectivity_, which ranges from 0 (not subjective) to 1 (very
#'              subjective);}
#'      }
#'    `vaderSentiment` calculates four indicators:
#'    \itemize{
#'        \item{_Compound_ is like `TextBlob`'s _polarity_;}
#'        \item{Indicators of the proportion of positivity, negativity and
#'            neutrality in the text. The three indicators sum to 1;}
#'    }
#'    This function is an implementation of Python function `sentiment_scores`
#'    in package [`pxtextmining`](https://github.com/CDU-data-science-team/pxtextmining/tree/main).
#'
#' @return A data frame with the five indicators described in Details.
#' @export
#'
#' @examples

calc_sentiment_indicators <- function(x, python_setup = TRUE, sys_setenv,
                                      which_python, which_venv,
                                      venv_name, text_col_name) {

  if (python_setup) {
    experienceAnalysis::prep_python(sys_setenv, which_python, which_venv,
                                    venv_name)
  }

  sentiment_scores <- reticulate::py_run_string(
    "from pxtextmining.helpers.sentiment_scores import sentiment_scores"
  )

  sentiments_table <- x %>%
    dplyr::select(dplyr::all_of(text_col_name)) %>%
    sentiment_scores$sentiment_scores()

  return(sentiments_table)
}
