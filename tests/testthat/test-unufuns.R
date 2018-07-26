context("test-unufuns.R")
library(nat)

testim=im3d(1:24, dims=c(2,3,4))
write.nrrd(testim, testimf <- tempfile(fileext = '.nrrd'), dtype = 'byte')
on.exit(unlink(testimf))

test_that("we can get unu information", {
  skip_no_unu()
  expect_match(paste(unu('env', intern=TRUE), collapse = ""),
               'environment variables')
})

test_that("unu hist", {
  skip_no_unu()
  imh=hist(testim, breaks = 0:256-0.5)
  nh=NrrdHisto(testimf, outfile = F)
  # don't test setting the name
  imh$xname=nh$xname
  expect_equal(imh, nh)
})
