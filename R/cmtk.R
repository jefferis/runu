#' Resample a CMTK deformation field
#'
#' @inheritParams NrrdResample
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' resample_cmtk_dfield("hemiBrain_JRC2018F_Warp_cmtk.nrrd", size=c(NA, rep(1/3, 3)))
#' # This currently fails because of https://github.com/jefferis/nat/issues/385
#' resample_cmtk_dfield("hemiBrain_JRC2018F_Warp_cmtk.nrrd", voxdims=c(NA, 2,2,2))
#' }
resample_cmtk_dfield <- function(infile, outfile=NULL, size=NULL, voxdims=NULL, ...) {
  h <- read.nrrd.header(infile)
  check_dfield(h)
  infile=normalizePath(infile)
  outfile <- NrrdResample(infile=infile, outfile=outfile, size=size, voxdims=voxdims, ...)
  if(is.logical(outfile) && !outfile) return(FALSE)
  # want to keep the original header to use later
  inh=read.nrrd.header(outfile)
  inht=attr(inh, 'headertext')
  oldkinds=grep("^kinds: ", inht, value = T)
  newkinds=strsplit(sub("^kinds: ","", oldkinds), " ")[[1]]
  if(newkinds[1]=="vector") {
    # nothing to do
  } else if(newkinds[1]=="???"){
    newkinds[1]='vector'
    newkindline=paste(newkinds, collapse = ' ')
    AddOrReplaceNrrdHeaderField(infile = outfile, newfields = c(kinds=newkindline))

  } else stop("Unexpected state of kinds field in header\n",oldkinds)

}

check_dfield <- function(x) {
  h <- if(is.list(x)) x else read.nrrd.header(x)
  if(!isTRUE(h$dimension==4))
    stop("Must have dimension 4!")
  if(!isTRUE(h$sizes[1]==3))
    stop("First dimension must have size 3!")
  if(!isTRUE(h$kinds[1]=="vector"))
    stop("First dimension must have kind 'vector' not ", h$kinds[1], '!')
  TRUE
}