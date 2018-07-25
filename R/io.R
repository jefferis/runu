
#' @importFrom nat read.nrrd
ReadHistogramFromNrrd<-function(filename,...){
  d=read.nrrd(filename, AttachFullHeader = TRUE, ...)
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
      intensities = density,
      density = density,
      mids = mids,
      xname = h$content,
      equidist = TRUE
    ),
    .Names = c("breaks", "counts", "intensities",
               "density", "mids", "xname", "equidist"),
    class = "histogram"
  )
}
