## code to prepare `sentiment` dataset goes here

library(textdata)

afinn <- lexicon_afinn()
bing <- lexicon_bing()
nrc <- lexicon_nrc()

usethis::use_data(afinn, overwrite = TRUE)
usethis::use_data(bing, overwrite = TRUE)
usethis::use_data(nrc, overwrite = TRUE)
