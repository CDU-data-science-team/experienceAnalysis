% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_accuracy_per_class.R
\name{calc_accuracy_per_class}
\alias{calc_accuracy_per_class}
\title{Calculate classifier accuracy for each class and group}
\usage{
calc_accuracy_per_class(
  x,
  target_col_name,
  target_pred_col_name,
  column_names = NULL
)
}
\arguments{
\item{x}{A data frame with two columns: the column with the actual classes;
and the column with the predicted classes. Any other columns will be
ignored.}

\item{target_col_name}{A string with the column name of the target variable.}

\item{target_pred_col_name}{A string with the column name of the predictions
for the target variable.}

\item{column_names}{A vector of strings or \code{NULL}, used to specify the
names of the returned data frame/tibble. See Details.}
}
\value{
A data frame/tibble with as many rows as the number of unique labels.
}
\description{
Calculates the accuracy of a predictive model for each class.
}
\details{
This function was originally designed for use with package
\href{https://github.com/CDU-data-science-team/pxtextminingdashboard}{\code{{pxtextminingdashboard}}},
in which case \code{column_names} is set to \code{c("class", "accuracy")}. It can,
however, be used outside the context of\code{{pxtextminingdashboard}}, by
controlling the \code{column_names} argument:
\itemize{
\item{When \code{column_names} is \code{NULL}, then the returned data frame names
are \code{c(target_col_name, "accuracy")}.}
\item{When \code{column_names} is a vector of strings, the returned data
frame names are as in the vector.}
}
}
\examples{
library(experienceAnalysis)
mtcars \%>\%
  dplyr::mutate(carb_pred = sample(carb, size = nrow(.))) \%>\%  # Mock predictions column
  calc_accuracy_per_class(
    target_col_name = "carb",
    target_pred_col_name = "carb_pred"
  )

# Custom column names
mtcars \%>\%
  dplyr::mutate(carb_pred = sample(carb, size = nrow(.))) \%>\%  # Mock predictions column
  calc_accuracy_per_class(
    target_col_name = "carb",
    target_pred_col_name = "carb_pred",
    column_names = c("class", "accuracy_per_class")
  )
}
