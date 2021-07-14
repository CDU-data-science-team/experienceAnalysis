#' Check for sentiment dictionaries
#'
#' On a server, the sentiment dictionaries have to be manually uploaded. On a
#' local machine they can be loaded using {tidytext}. Check to see if they
#' are already loaded (by a previous function run), if they are on disk
#' (which would indicate the code is on a server), or just load using
#' {tidytext}
#'
#' @param dictionary string. One of "afinn", "nrc", or "bing", indicating
#' the dictionary to be loaded
#' @return a dataframe containing the requested sentiment dictionary
#' @export

get_dictionary <- function(dictionary){

  if(exists(dictionary)){

    return(get(dictionary))

  } else {

    return(tidytext::get_sentiments(dictionary))
  }
}
