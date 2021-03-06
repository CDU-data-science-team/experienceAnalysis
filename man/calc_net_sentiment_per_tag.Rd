% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_net_sentiment_per_tag.R
\name{calc_net_sentiment_per_tag}
\alias{calc_net_sentiment_per_tag}
\title{Calculate "net positive" and "net negative" sentiment in a text}
\usage{
calc_net_sentiment_per_tag(x, target_col_name = NULL, text_col_name)
}
\arguments{
\item{x}{A data frame with two columns: the column with the classes; and the
column with the text. Any other columns will be ignored.}

\item{target_col_name}{A string with the column name of the target variable.
Defaults to \code{NULL}.}

\item{text_col_name}{A string with the column name of the text variable.}
}
\value{
A data frame with four or five columns: the column with the classes
(if any); the net sentiment; the method (dictionary) used; the total
negative sentiment; and the total positive sentiment. The last two
columns are NA for AFINN (see Note).
}
\description{
For a given text and class, calculate indicators of "net positive" and
"net negative" sentiment using different sentiment dictionaries.
}
\details{
The dictionaries of Minging and Liu (2004) and Mohammad and Turney
(2013; known as NRC) assign sentiment characterizations to words, e.g.
"negative" or "positive". The "net" sentiment is therefore calculated as
"sum of words with a positive sentiment minus sum of words with a
negative sentiment". On the other hand, AFINN, the dictionary of Nielsen
(2013), works with sentiment scores and so the net sentiment is their sum.
See Silge and Robinson (2017).
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

# Net sentiment in each book for each dictionary, sorted in descending order
calc_net_sentiment_per_tag(x, target_col_name = "book",
                           text_col_name = "text")

# Net sentiment in each book for each dictionary, by dictionary and book name
calc_net_sentiment_per_tag(x, target_col_name = "book",
                           text_col_name = "text") \%>\%
    dplyr::arrange(method, book)
}
\references{
Hu M. & Liu B. (2004). Mining and summarizing customer
reviews. Proceedings of the ACM SIGKDD International Conference on
Knowledge Discovery & Data Mining (KDD-2004), Seattle, Washington, USA,
Aug 22-25, 2004. \cr\cr
Mohammad S.M. & Turney P.D. (2013). Crowdsourcing a Word–Emotion
Association Lexicon. Computational Intelligence, 29(3):436-465. \cr\cr
Nielsen F.A. (2013). A new ANEW: Evaluation of a word list for
sentiment analysis in microblogs. Proceedings of the ESWC2011 Workshop
on 'Making Sense of Microposts': Big things come in small packages 718
in CEUR Workshop Proceedings 93-98. https://arxiv.org/abs/1103.2903. \cr\cr
Silge J. & Robinson D. (2017). Text Mining with R: A Tidy Approach.
Sebastopol, CA: O’Reilly Media. ISBN 978-1-491-98165-8.
}
