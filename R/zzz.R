.onAttach <- function(libname, pkgname) {
  unuloc <- try(find_unu(), silent = TRUE)
  if(inherits(unuloc, 'try-error'))
    packageStartupMessage(
      "Cannot find unu executable!\n  ",
      as.character(unuloc),
      "Please ensure that unu is installed and",
      " in your path!\nSee ?unu and http://teem.sourceforge.net/unrrdu for",
      " further details")

  invisible()
}
