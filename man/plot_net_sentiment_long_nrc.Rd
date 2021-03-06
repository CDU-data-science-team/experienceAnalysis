% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_net_sentiment_long_nrc.R
\name{plot_net_sentiment_long_nrc}
\alias{plot_net_sentiment_long_nrc}
\title{Plot sentiment counts in a text}
\usage{
plot_net_sentiment_long_nrc(net_sentiment_long_nrc)
}
\arguments{
\item{net_sentiment_long_nrc}{A data frame from
\code{\link{tidy_net_sentiment_nrc}}.}
}
\value{
A \code{ggplot} (\code{ggplot::geom_col}).
}
\description{
Bar plots of NRC sentiment counts in one or more documents.
}
\examples{
library(experienceAnalysis)
books <- janeaustenr::austen_books() # Jane Austen books
emma <- paste(books[books$book == "Emma", ], collapse = " ") # String with whole book
pp <- paste(books[books$book == "Pride & Prejudice", ], collapse = " ") # String with whole book

# Make data frame with books Emma and Pride & Prejudice
x <- data.frame(
  text = c(emma, pp),
  book = c("Emma", "Pride & Prejudice")
)

# Net sentiment in each book
net_sentiment_wide_nrc <- calc_net_sentiment_nrc(x, target_col_name = "book",
                                                 text_col_name = "text",
                                                 filter_class = NULL)

net_sentiment_wide_nrc

# Tidy net_sentiment_wide_nrc and place the most "angry" book at the top
net_sentiment_long_nrc <- tidy_net_sentiment_nrc(net_sentiment_wide_nrc,
                                                 sorting_sentiments = "anger",
                                                 num_of_docs = 2) \%>\%
  dplyr::select(-text)

net_sentiment_long_nrc

plot_net_sentiment_long_nrc(net_sentiment_long_nrc)
}
