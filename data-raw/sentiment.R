## code to prepare `sentiment` dataset goes here

library(textdata)

afinn <- lexicon_afinn("afinn")
bing <- lexicon_afinn("bing")
nrc <- lexicon_afinn("nrc")

usethis::use_data(afinn, overwrite = TRUE)
usethis::use_data(bing, overwrite = TRUE)
usethis::use_data(nrc, overwrite = TRUE)
