context("test-unufuns.R")

test_that("we can get unu information", {
  skip_no_unu()
  expect_match(paste(unu('env', intern=TRUE), collapse = ""),
               'environment variables')
})
