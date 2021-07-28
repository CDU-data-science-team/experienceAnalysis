test_that("Dictionary retrieved", {

  df1 <- get_dictionary("nrc")

  df2 <- get_dictionary("afinn")

  df3 <- get_dictionary("bing")

  expect_gt(nrow(df1), 0)

  expect_gt(nrow(df2), 0)

  expect_gt(nrow(df3), 0)
})
