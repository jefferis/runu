#' Run the unu command line tool
#'
#' @param cmd The name of the unu command to run
#' @param args Additional arguments to unu
#' @param DryRun When \code{TRUE} show what would happen rather running command
#' @param ... Additional arguments passed to \code{\link{system}}
#'
#' @return \code{unu} returns the value of \code{\link{system}} after running
#'   unu.
#' @export
#'
#' @examples
#' \donttest{
#' find_unu()
#'
#' # show help
#' unu()
#' }
unu<-function(cmd='', args=NULL, DryRun=FALSE, ...){
  unu=find_unu()
  fullcmd=paste(unu, cmd, paste(args, collapse=" "))
  if(DryRun) {
    cat(fullcmd, "\n", sep="")
    return(0)
  }
  else system(fullcmd, ...)
}

#' @export
#' @param location - location of the unu executable
#' @details If \code{find_unu} cannot find your unu executable then either add
#'   the directory to your path or set the package option.
#' @section Installation: You will normally need to compile unu, but it is
#'   cross-platform (Windows/Mac/Linux) and has minimal dependencies. It is
#'   installed as part of the \bold{teem} libraries available at
#'   \url{http://teem.sourceforge.net/}. See in particular the
#'   \href{http://teem.sourceforge.net/download/index.html}{download} and
#'   \href{http://teem.sourceforge.net/build.html}{build} pages for details.
#' @rdname unu
#'
#' @examples
#' \dontrun{
#' # manually set unu location if required
#' options(runu.unu="/path/to/unu")
#' }
find_unu <- function(location = getOption("runu.unu")) {
  if(is.null(location)){
    location=unname(Sys.which('unu'))
    if(!nzchar(location))
      stop("unu is not in path!")
  }
  if(file.access(location, mode=0)<0)
    stop("unu is not at ", unu)

  if(file.access(location, mode=1)<0)
    stop("unu is not executable!")

  options("runu.unu"=location)

  location
}

skip_no_unu <- function() {
  u <- try(find_unu(), silent = TRUE)
  testthat::skip_if(inherits(u, 'try-error'),
                    message = "teem/unu is not installed or visible!")
}
