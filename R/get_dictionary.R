#' Check for sentiment dictionaries
#'
#' On a server, the sentiment dictionaries have to be manually uploaded. On a
#' local machine they can be loaded using `{tidytext}`. Check to see if they
#' are already loaded (by a previous function run), if they are on disk
#' (which would indicate the code is on a server), or just load using
#' `{tidytext}`.
#'
#' @param dictionary A string. One of "afinn" (Nielsen, 2013), "nrc" (Mohammad &
#'     Turney, 2013) or "bing" (Hu & Liu, 2004), indicating the dictionary to be
#'     loaded.
#' @return A data frame with two columns: the word and its sentiment according
#'     to the requested sentiment dictionary.
#' @export
#'
#' @examples
#'     get_dictionary("afinn")
#'     get_dictionary("nrc")
#'     get_dictionary("bing")
#' @references Hu M. & Liu B. (2004). Mining and summarizing customer
#'     reviews. Proceedings of the ACM SIGKDD International Conference on
#'     Knowledge Discovery & Data Mining (KDD-2004), Seattle, Washington, USA,
#'     Aug 22-25, 2004. \cr\cr
#'     Mohammad S.M. & Turney P.D. (2013). Crowdsourcing a Word–Emotion
#'       Association Lexicon. Computational Intelligence, 29(3):436-465. \cr\cr
#'     Nielsen F.A. (2013). A new ANEW: Evaluation of a word list for
#'       sentiment analysis in microblogs. Proceedings of the ESWC2011 Workshop
#'       on 'Making Sense of Microposts': Big things come in small packages 718
#'       in CEUR Workshop Proceedings 93-98. https://arxiv.org/abs/1103.2903. \cr\cr
#'     Silge J. & Robinson D. (2017). Text Mining with R: A Tidy Approach.
#'       Sebastopol, CA: O’Reilly Media. ISBN 978-1-491-98165-8.

get_dictionary <- function(dictionary){

  if(exists(dictionary)) {

    return(get(dictionary))

  } else {

    return(tidytext::get_sentiments(dictionary))
  }
}
