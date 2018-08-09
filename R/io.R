
#' @importFrom nat read.nrrd
ReadHistogramFromNrrd<-function(filename,...){
  # because of uint32 warning
  d=suppressWarnings(read.nrrd(filename, AttachFullHeader = TRUE, ...))
  h=attr(d, "header")
  # clear the header attributes
  attributes(d)<-NULL
  if(is.na(pmatch("histo",h$content)) || h$dimension!=1) {
    warning ("This does not appear to be a 1d nrrd histogram")
    return(d)
  }
  density=d/sum(as.numeric(d))
  mids=seq(from=h$axismins, to=h$axismax, len=h$sizes)
  halfwidth=(h$axismaxs-h$axismins)/(h$sizes-1)/2
  breaks=seq(from=h$axismins-halfwidth, to=h$axismaxs+halfwidth, length.out=h$sizes+1)

  # return it as an R histogram
  structure(
    list(
      breaks = breaks,
      counts = d,
      density = density,
      mids = mids,
      xname = h$content,
      equidist = TRUE
    ),
    .Names = c("breaks", "counts",
               "density", "mids", "xname", "equidist"),
    class = "histogram"
  )
}

#' Make a detached header for a specified nrrd file
#'
#' If nhdr is not supplied defaults to <nrrd>.nhdr.
#' If nhdr=NA then new header is returned but not written.
#' @param nrrd Full path to a nrrd file
#' @param nhdr Full path nhdr file to be written
#' @return invisibly returned character vector with new header
#' @export
NrrdMakeDetachedHeaderForNrrd<-function(nrrd,nhdr=paste(nrrd,sep='.','nhdr')){
  h=read.nrrd.header(nrrd)
  # drop the directory if the nhdr will be next to the nrrd
  if(is.na(nhdr) || dirname(nrrd)==dirname(nhdr))
    nrrd=basename(nrrd)
  oht=attr(h,'headertext')
  # line skip should be length of old header + 1 for the blank line before data
  nht=c(oht,paste("line skip:",length(oht)+1))
  nht=c(nht,paste("datafile:",nrrd))
  if(!is.na(nhdr)) writeLines(nht,nhdr)
  invisible(nht)
}

