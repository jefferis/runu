context("test-nrrd-header.R")

test_that("manipulating nrrd headers", {
  library(nat)
  im=nat::im3d(1:24, dims = 2:4, voxdims = c(2,2,4))
  tf=tempfile(fileext = '.nrrd')
  on.exit(unlink(tf))
  write.im3d(im, file=tf, dtype='byte')
  h=read.nrrd.header(tf)
  AddOrReplaceNrrdHeaderField(tf, tf, Force=TRUE,
                              newfields=c(`space directions`="(1,0,0) (0,1,0) (0,0,1)"))
  h2 <- read.nrrd.header(tf)
  expect_equal(h$sizes, h2$sizes)
  expect_equal(h2$`space directions`, diag(1, nrow=3))
})
