
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

#' Add or replace lines in the header of a nrrd file
#'
#' Input is a named vector of new fields. Use unnamed fields for comments.
#' Quote field names with spaces (conventionally with back ticks).
#' Note that this function will error out for invalid field names.
#' See http://teem.sourceforge.net/nrrd/format.html for nrrd field details
#' @param infile Path to input file
#' @param outfile Path to output file
#' @param newfields Named vector of fields to replace
#' @param Force Overwrite existing file (default FALSE)
#' @param Detached Write a detached header insted of a nrrd (default FALSE)
#' @param action addreplace (Default) addonly or replaceonly
#' @return TRUE or FALSE depending on success
#' @export
#' @seealso \code{\link{read.nrrd.header}}
#' @examples
#' \dontrun{
#' AddOrReplaceNrrdHeaderField(lhmaskfile,outfile=file.path(tmpdir,"LHMask.nrrd"),
#'   c("# My interesting comment",`space origin`="(2,2,2)"),Force=TRUE)
#' }
AddOrReplaceNrrdHeaderField<-function(infile,outfile,newfields,Force=FALSE,
  Detached=FALSE,action=c("addreplace","addonly","replaceonly")){
  # see if a given field exists and add or replace its value
  saveontop=ifelse(infile==outfile,TRUE,FALSE)
  if(!Force && file.exists(outfile)) stop("Use Force=TRUE to replace existing files")
  if(saveontop && Detached) stop("Unable to save a detached header on top of its nrrd file")
  action=match.arg(action)

  if(Detached){
    # make a detached header for the original file but don't write it
    tempheader=tempfile(tmpdir=dirname(outfile))
    oht=NrrdMakeDetachedHeaderForNrrd(infile,tempheader)
    inh=read.nrrd.header(tempheader)
    unlink(tempheader)
  } else {
    inh=read.nrrd.header(infile)
    oht=attr(inh,"headertext")
  }

  if(is.null(names(newfields))) names(newfields) <- rep("",length(newfields))
  for(i in seq(newfields)){
    field=names(newfields)[i]
    value=newfields[i]
    if(field==""){
      # this is a comment
      newFieldLine=value
    } else {
      if(!.validateNrrdFieldName(field))
        stop("Invalid nrrd field name: ",field)
      newFieldLine=paste(field,": ",value,sep="")
    }

    if(field%in%names(inh)) {
      # replace existing field
      if(action=="addonly") {
        warning("Unable to replace field in addonly mode")
        return(FALSE)
      }
      oht=sub(paste(field,": .*",sep=""),newFieldLine,oht)
    } else {
      if(action=="replaceonly") {
        warning("Unable to add field in replaceonly mode")
        return(FALSE)
      }
      # just append
      oht=c(oht,newFieldLine)
    }
  }

  # add a blank line
  if(Detached){
    writeLines(oht,outfile)
    return(TRUE)
  }
  oht=c(oht,"")
  headerfile=tempfile()

  writeLines(oht,headerfile)
  if(saveontop){
    outfile=tempfile(pattern=basename(infile),tmpdir=dirname(infile))
  }
  rval=system(paste("unu data",shQuote(infile),"| cat",headerfile,"- >",shQuote(outfile)))
  unlink(headerfile)
  if(rval!=0){
    if(saveontop) unlink(outfile) # cleanup temporary nrrd
    stop("Error ",rval," saving file to: ",outfile)
  }
  # else success
  if(saveontop) file.rename(outfile,infile)
  return(TRUE)
}

.validateNrrdFieldName<-function(fieldname) {
  fieldname=.standardNrrdFieldName(fieldname)

  all(fieldname %in% c("space", "space dimension", "space units", "space origin",
  "space directions", "measurement frame", "dimension", "type",
  "blocksize", "encoding", "endian", "content", "min", "max", "oldmin",
  "oldmax", "datafile", "lineskip", "byteskip", "number", "sampleunits",
  "sizes", "spacings", "thicknesses", "axismins", "axismaxs", "centers",
  "labels", "units", "kinds"))
}

.standardNrrdFieldName<-function(fieldname,Validate=FALSE)
{
  if(length(fieldname)>1) return(sapply(fieldname,.standardNrrdFieldName,Validate=Validate))
  if(!fieldname%in%c("space dimension","space units","space origin","space directions","measurement frame"))
    fieldname=gsub(" ","",fieldname,fixed=TRUE)
  if(Validate){
    # check that we have been given a valid field
    if(!.validateNrrdFieldName(fieldname))
      stop("Invalid nrrd field: ",fieldname)
  }
  fieldname
}
