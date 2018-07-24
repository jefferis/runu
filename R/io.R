
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
  breaks=seq(from=h$axismins,to=h$axismax,len=h$sizes+1)
  halfwidth=(h$axismaxs-h$axismins)/h$sizes/2
  density=d/sum(d)
  mids=seq(h$axismins+halfwidth,h$axismaxs-halfwidth,len=h$sizes)

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
