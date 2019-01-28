# Some Image Processing Functions making use of the unu command line tool
# unu = Utah Nrrd Utilities - ie they operate on nrrd files
# See http://teem.sourceforge.net/unrrdu/index.html


#' Binary operation on two nrrds, or on a nrrd and a constant
#'
#' @param infiles Input files
#' @param outfile Output files
#' @param fun Binary function or operator
#' @param gzip Whether to gzip the resultant file
#' @param CreateDirs Whether to create implied output directories
#' @param Verbose Print status messages
#' @param Force Whether to overwrite an existing file
#'
#' @return The number of input files
#' @export
#' @importFrom nat.utils RunCmdForNewerInput
Nrrd2op<-function(infiles,outfile,fun=c("max","min","+", "-", "x", "/"),
  gzip=FALSE,CreateDirs=TRUE,Verbose=TRUE,Force=FALSE){
  if(length(infiles)<1) return(NULL)
  # Do nothing if inputs are older than output unless Force=T
  if(!Force && !RunCmdForNewerInput(NULL,infiles,outfile)) return (NULL)
  if(CreateDirs && !file.exists(dirname(outfile))) dir.create(dirname(outfile))
  fun=match.arg(fun)

  # 1 input: f1 -> output
  if(length(infiles)==1) file.copy(infiles[1],outfile)
  else if(length(infiles)>1) {
    # 2 or more inputs: f1 x f2 -> output
    cmd=paste("unu 2op",fun,
      shQuote(infiles[1]),shQuote(infiles[2]),"-o",shQuote(outfile))
    system(cmd)
    if(Verbose) cat(".")
  }
  # 3 or more inputs: f3 ... x output -> output
  if(length(infiles)>2) {
    for (f in infiles[-(1:2)]){
      cmd=paste("unu 2op",fun,
        shQuote(f),shQuote(outfile),"-o",shQuote(outfile))
      system(cmd)
      if(Verbose) cat(".")
    }
  }
  if(length(infiles)>1 && gzip)
    system(paste("unu save --format nrrd --encoding gzip","-i",shQuote(outfile),"-o",shQuote(outfile)))
  return(length(infiles))
}

#' Return range of values in nrrd file (interface to unu minmax command)
#' @param filename nrrd file containing data
#' @param blind8 Assume 8 bit data has range 0-255 (or -127-127), default F.
#' @param ... passed to \code{\link{system}} function
#' @return c(min,max) or c(NA,NA) like R's \code{\link{range}} function
#' @author jefferis
#' @export
NrrdMinMax<-function(filename, blind8=FALSE, ...){
  args=shQuote(filename)
  if(blind8) args=c(args,"-blind8 true")
  minmax=unu("minmax", args, intern=TRUE, ...)
  if(length(minmax)==0) return(c(NA,NA))
  as.numeric(sub("(min|max): ","",minmax[1:2]))
}

#' Resample a nrrd with simple or complex filters
#'
#' @description wraps unu resample
#'
#'   When size is explicitly an integer (e.g. c(512L,512L,100L) then it is
#'   assumed to represent the size of the desired output in pixels.
#'
#'   unu resample defaults to cell centering in the asbence of information, but
#'   I prefer node.
#'
#'   * downsampling a **cell** image (e.g. x2) will result in \itemize{
#'
#'   \item an origin shift **iff** an origin was specified in the input file
#'
#'   \item a change in the space directions that is not an exact multiple of the
#'   downsampling factor } Downsampling a node image (e.g. x2) will result in:
#'   \itemize{
#'
#'   \item no change in origin
#'
#'   \item a doubling in the space directions field }
#' @param infile,outfile Input/output files
#' @param size Numeric vector of scale factors (NA=> don't touch) or integer
#'   vector of pixel dimensions.
#' @param voxdims Target voxel dimensions
#' @param centering Whether to assume node or cell centering if not specified in
#'   input file.
#' @param otherargs Passed to unu resample
#' @param gzip Whether or not to gzip encode the output file?
#' @param suffix A suffix added to the input file to construct an output
#'   filename
#' @param UseLock Whether to use file-based locking to enable simple
#'   parallelisation
#' @param ... Additional params passed to unu
#' @return The output file
#' @author jefferis
#' @export
#' @importFrom nat.utils makelock
#' @inheritParams Nrrd2op
NrrdResample<-function(infile,outfile,size,voxdims=NULL,
    centering=c("cell","node"),otherargs=NULL,gzip=TRUE, suffix=NULL,
    CreateDirs=TRUE,Verbose=TRUE,Force=FALSE,UseLock=FALSE,...){

  centering=match.arg(centering)
  if(!file.exists(infile)) stop("infile: ",infile," does not exist")
  if(!is.null(voxdims)){
    # we have a target voxel size instead of a standard size specification
    # first fetch existing voxel size
    size=nat::nrrd.voxdims(infile)/voxdims
  }
  if(is.integer(size)) size=paste("--size",paste(size,collapse=" "))
  else {
    size=paste("--size",paste("x",size,sep="",collapse=" "))
    # eg size=c(0.5,0.5,NA) => "--size x0.5 x0.5 ="
    size=sub("xNA","=",size)
  }

  if (missing(outfile)) {
    # we only want to add a default suffix if we are putting the output
    # file into the same directory
    if(is.null(suffix))
      suffix=gsub(" ","",sub("--size","",size))
    outfile=sub("\\.nrrd$",paste(suffix,"\\.nrrd",sep=""),infile)
  } else {
    # here we have been passed an output directory rather than a file
    if(file.exists(outfile) && file.info(outfile)$isdir)
      outfile=file.path(outfile,
        sub("\\.nrrd$",paste(suffix,"\\.nrrd",sep=""),basename(infile)))
  }

  # return outfile to signal output exists (we just didn't make it now)
  if(!Force && !RunCmdForNewerInput(NULL,infile,outfile)) return (outfile)
  # NB createdirs only makes sense if we have directly specified outfile
  if(CreateDirs && !file.exists(dirname(outfile))) dir.create(dirname(outfile),recursive = TRUE)
  lockfile=paste(outfile,sep=".","lock")
  # return FALSE to signal output doens't exist
  if(UseLock){
    if(makelock(lockfile))
      on.exit(unlink(lockfile))
    else
      return(FALSE)
  }
  otherargs=c(otherargs,"--center",centering)
  cmdargs=paste(size, paste(otherargs,collapse=" "),"-i",shQuote(infile))
  if(gzip)
    cmdargs=paste(cmdargs,"| unu save -f nrrd -e gzip")
  rval=unu("resample",paste(cmdargs,"-o",shQuote(outfile)),...)
  if(rval!=0) stop("error ",rval," in unu resample")
  return(outfile)
}

#' Make a histogram from a nrrd file (as a new nrrd)
#'
#' @details \code{bins} defaults to 1000 for float/double image data, to the
#'   number of levels between \code{min} and \code{max} for integer data. If
#'   \code{min} or \code{max} is not specified, they will be set to the minimum
#'   (maximum) value found in the input data.
#' @param outfile Path to an output NRRD \emph{or} \code{FALSE} in which case an
#'   R \code{\link{histogram}} object will be returned.
#' @param maskfile Path to another image file defining which pixels in the input
#'   file to include.
#' @param bins Number of bins for the histogram (see details)
#' @param min,max Minimum and maximum values for the histogram (see details)
#' @param blind8 Whether to assume the full range (0,255) for 8 bit images
#'   without checking
#' @param ... Additional arguments passed to \code{\link{unu}} function.
#'
#' @return Output file or an R \code{\link{histogram}} object.
#' @inheritParams NrrdResample
#' @importFrom nat read.nrrd.header
#' @export
NrrdHisto<-function(infile,outfile=sub("\\.([^.]+)$",".histo.\\1",infile),
  maskfile,bins,min,max,blind8=TRUE,...){
  readhisto <- is.logical(outfile) && !outfile
  if(readhisto) {
    outfile <- tempfile(fileext = ".nrrd")
    on.exit(unlink(outfile))
  } else {
    outfile=path.expand(outfile)
  }
  infile=path.expand(infile)

  h=read.nrrd.header(infile)
  nrrdType=nat:::.standardNrrdType(h$type)
  unuhistooptions=''
  if(nrrdType%in%c("uint8","int8") && blind8 && missing(min) && missing(max)){
    # this is an 8 bit image
    # and we're going to use unu histo's blind8 option (default) to
    # assume uchar [0,255], signed char is [-128,127]
    bins=256
  } else {
      if (missing(min) || missing(max)) {
      # calculate the minimum and maximum
      r=NrrdMinMax(infile)
      if(any(is.na(r))) stop("Unable to find min and max from: ",infile)
      if(missing(min)) min=r[1]
      if(missing(max)) max=r[2]
      }
    unuhistooptions=paste("-min",min,"-max",max)
  }
  if(missing(bins)){
    # check if this is a float data type
    if(nrrdType%in%c("float","double"))
      bins=1000
    else {
      bins=as.integer((max-min)+1)
      if(bins>2^16) bins=1000
    }
  }
  unuhistooptions=paste(unuhistooptions,"-b",bins)
  if(!missing(maskfile)) unuhistooptions=paste(unuhistooptions,"-w",shQuote(maskfile))
  unu("histo",paste(unuhistooptions,"-i",shQuote(infile),"-o",shQuote(outfile)),...)

  if(readhisto) ReadHistogramFromNrrd(outfile) else outfile
}

NrrdQuantize<-function(infile,outfile,min,max,bits=c("8","16","32"),
  gzip=FALSE,CreateDirs=TRUE,Verbose=TRUE,Force=FALSE,UseLock=FALSE){

  # Do nothing if inputs are older than output unless Force=T
  if(!Force && !RunCmdForNewerInput(NULL,infile,outfile)) return (FALSE)
  if(CreateDirs && !file.exists(dirname(outfile))) dir.create(dirname(outfile),recursive = TRUE)

  bits=match.arg(bits)

  lockfile=paste(outfile,sep=".","lock")
  if(UseLock && !makelock(lockfile)) return (FALSE)
  # unu quantize -b 8 -min 7397.386 -max 65535 -i AL-PNs_mask_MF_final_IS2_SAIA24-1_02.nrrd \
  # | unu save -f nrrd -e gz -o AL-PNs_mask_MF_final_IS2_SAIA24-1_02-quant.nrrd

  minmaxopts=paste(ifelse(missing(min),"",paste("-min",min)),
    ifelse(missing(max),"",paste("-max",max)))
  cmd=paste("unu quantize -b",bits,minmaxopts,"-i",shQuote(infile))
  if(gzip) cmd=paste(cmd,"| unu save -f nrrd -e gzip -o",shQuote(outfile))
  else cmd=paste(cmd,"-o",shQuote(outfile))
  system(cmd)
  if(Verbose) cat(".")
  if(UseLock) unlink(lockfile)
  return(TRUE)
}

#' Test integrity of a NRRD file using gzip/bzip2 checksum
#'
#' @param defaultReturnVal The value to return when file cannot be checked
#'
#' @return A logical indicating if the check passed
#' @export
#'
#' @inheritParams NrrdResample
#' @seealso \code{\link{NrrdTestDataLength}} which is much faster
NrrdTestIntegrity<-function(infile,defaultReturnVal=TRUE){
  # Tests integrity of a compressed nrrd file using crc check
  if(!file.exists(infile)) return(NA)
  h=nat::read.nrrd.header(infile)
  if(tolower(h$encoding)%in%c("gz","gzip")) testprog='gzip'
  else if(tolower(h$encoding)%in%c("bz2","bzip2")) testprog='bzip2'
  else {
    warning("Unable to test integrity of nrrd file with encoding",h$encoding)
    return(defaultReturnVal)
  }
  testval=system(paste("unu data",infile," | ",testprog,"-t"),ignore.stderr=TRUE)
  return(testval==0)
}

#' Quick test that gzipped NRRD has correct data length
#'
#' @details Tests integrity of a nrrd file by checking that the data block is as
#'   long as it should be. For a gzip file, it checks that the last 4 bytes
#'   encode the length of the uncompressed data. This says nothing about the
#'   contents but does ensure that the file has not been truncated which is much
#'   the most common problem.
#' @return A logical indicating if the check passed
#' @export
#' @inheritParams NrrdTestIntegrity
#' @seealso \code{\link{NrrdTestIntegrity}} which is more comprehensive but much
#'   slower
NrrdTestDataLength<-function(infile,defaultReturnVal=TRUE){
  if(!file.exists(infile)) return(NA)
  # need to read this full header in order to get the size in bytes of
  # the uncompressed data
  fullh=nat::read.nrrd(infile,ReadData=F,AttachFullHeader=T)
  h=attr(fullh,'header')
  if(tolower(h$encoding)%in%c("gz","gzip")) enc='gzip'
  else if(tolower(h$encoding)=='raw') enc='raw'
  else {
    warning("Unable to test data length of nrrd file with encoding",h$encoding)
    return(defaultReturnVal)
  }
  # figure out how many bytes we are expecting
  dataLength=attr(fullh,"datablock")$n*attr(fullh,"datablock")$size
  datafile=nat::nrrd.datafiles(h)
  if(length(datafile)>1){
    warning("Don't yet know how to handle detached headers with more than one data file")
    return(defaultReturnVal)
  }
  if(enc=='gzip'){
    # Check gzip file
    # last 4 bytes of gzip contain length of compressed stream
    con=file(datafile,open='rb')
    on.exit(close(con))
    seek(con,-4,origin='end')
    # how do we handle endianess?
    uncompressedBytes=readBin(con,what=integer(),size=4,n=1)
    if(uncompressedBytes>(2^32-1)) {
      warning("Don't know how to check integrity of files >= 2^32 bytes")
      return(defaultReturnVal)
    }
    return(dataLength==uncompressedBytes)
  } else {
    # TODO fix handling of detached nrrd files with raw encoding
    start=attr(fullh,'datablock')$datastartpos
    end=file.info(datafile)$size
    # TODO decide what we should say if we have MORE than enough data
    return(dataLength>=(end-start))
  }
}

#' Find the CRC (hash) of a gzip encoded NRRD file
#'
#' @param UseGzip Whether to use the gzip command line tool to extract CRC
#'   (default \code{FALSE}, see details)
#' @param FastHeader Parse only essential parts of NRRD header to save time.
#'
#' @details: Defaults to a quick method based on knowledge of gzip file format
#' from: \url{http://www.gzip.org/zlib/rfc-gzip.html} and assumption that there
#' is only one member in gzip data. Can also use gzip but this is much slower
#' since have to copy unu data to temporary file.
#' @inheritParams NrrdResample
#' @export
NrrdCrc<-function(infile, UseGzip=FALSE, FastHeader=TRUE){
  if(length(infile)>1) {
    return(sapply(infile, NrrdCrc, UseGzip=UseGzip, FastHeader=FastHeader))
  }
  if(!file.exists(infile)) return(NA)

  ext=tolower(sub(".*\\.([^.]+)$","\\1",basename(infile)))
  if(ext=='nhdr') FastHeader=FALSE

  h=NULL
  if(FastHeader){
    # quick and dirty reading of header
    con<-file(infile,'rb')
    on.exit(close(con))
    magic=readBin(con,what=raw(),5)
    if(!isTRUE(all.equal(magic,as.raw(c(0x4e, 0x52, 0x52, 0x44, 0x30))))){
      warning("This is not a nrrd")
      return(NA)
    }
    while( length(l<-readLines(con, 1))>0 && l!="" ){
      if(nchar(l)>=12 && substr(l, 1, 9)=="encoding:"){
        if(substr(l, 11, 12)!="gz"){
          warning("This is not a gzip encoded nrrd")
          return(NA)
        }
      }
    }
  } else {
    h=nat::read.nrrd.header(infile)
    if(tolower(h$encoding)%in%c("gz","gzip")) {
      # testprog='gzip'
    } else {
      warning("This is not a gzip encoded nrrd")
      return(NA)
    }
  }

  if(UseGzip){
    tmp=tempfile()
    on.exit(unlink(tmp),add=TRUE)
    system(paste("unu data ",shQuote(infile)," > ",shQuote(tmp)))
    x=system(paste("gzip -lv",shQuote(tmp)),intern=TRUE)
    crc=try(strsplit(x[2],"[ ]+")[[1]][[2]])
    if(inherits(crc,'try-error')) crc=NA
  } else {
    # TODO Fix handling of nhdr files
    if(!is.null(h) && !is.null(h$datafile)){
      if(length(h$datafile)>1)
        stop("Don't know how to handle more than 1 datafile")
      if(dirname(h$datafile)==".")
        h$datafile=file.path(dirname(infile),h$datafile)
      con=file(h$datafile,open='rb')
      on.exit(close(con),add=TRUE)
    } else if(!FastHeader){
      con=file(infile,open='rb')
      on.exit(close(con),add=TRUE)
    }
    seek(con,-8,origin='end')
    # TODO check endian issues (what happens if CRC was from opposite endian platform?)
    crc=readBin(con,integer(),size=4)
    crc=format(as.hexmode(crc),width=8)
  }
  crc
}


#' Make projections along a nrrd axis, typically also rescaling
#'
#' @details gamma: Just as in xv, the gamma value here is actually the
#'   reciprocal of the exponent actually used to transform the values. Note also
#'   that for \code{cropmin,cropmax} the special value M can be used to indicate
#'   the maximum index for that axis (i.e. n-1 when there are n samples).
#'
#'   scale: For each axis, information about how many samples in output:
#'   \itemize{
#'
#'   \item "=": leave this axis completely untouched: no resampling whatsoever
#'   \item "x<float>": multiply the number of input samples by <float>, and
#'   round to the nearest integer, to get the number of output samples. Use "x1"
#'   to resample the axis but leave the number of samples unchanged
#'
#'   \item "/<float>": divide number of samples by <float>
#'
#'   \item "+=<uint>", "-=<uint>": add <uint> to or subtract <uint> from number
#'   input samples to get number output samples
#'
#'   \item "<uint>": exact number of output samples
#'
#'   \item "a": resample this axis to whatever number of samples preserves the
#'   aspect ratio of other resampled axes. Currently needs to be used on all but
#'   one of the resampled axes, if at all. (1 or more sampling specifications) }
#'
#'   Kernel: The kernel to use for resampling. Kernels logically live in the
#'   input index space for upsampling, and in the output index space for
#'   downsampling. Possibilities include: \itemize{ \item "box": nearest
#'   neighbor interpolation on upsampling, and uniform averaging on downsampling
#'   \item "cheap": nearest neighbor interpolation for upsampling, and
#'   non-blurring sub-sampling (pick subset of input samples) on downsampling
#'
#'   \item "tent": linear interpolation
#'
#'   \item "cubic:B,C": Mitchell/Netravali BC-family of cubics: "cubic:1,0":
#'   B-spline; maximal blurring "cubic:0,0.5": Catmull-Rom; good interpolating
#'   kernel
#'
#'   \item "c4h": 6-sample-support, C^4 continuous, accurate
#'
#'   \item "c4hai": discrete pre-filter to make c4h interpolate
#'
#'   \item "bspl3", "bspl5", "bspl7": cubic (same as cubic:1,0), quintic, and
#'   7th order B-spline
#'
#'   \item "bspl3ai", "bspl5ai", "bspl7ai": discrete pre-filters to make bspl3,
#'   bspl5, bspl7 interpolate
#'
#'   \item "hann:R": Hann (cosine bell) windowed sinc, radius R
#'
#'   \item "black:R": Blackman windowed sinc, radius R
#'
#'   \item "gauss:S,C": Gaussian blurring, with standard deviation S and cut-off
#'   at C standard deviations
#'
#'   \item "dgauss:S,C": Lindeberg's discrete Gaussian. default: "cubic:0,0.5" }
#'
#'   Crop max, min: \itemize{
#'
#'   \item <int> gives 0-based index
#'
#'   \item M, M+<int>, M-<int> give index relative to the last sample on the
#'   axis (M == #samples-1).
#'
#'   \item m+<int> give index relative to minimum axis (only relevant for max)
#'
#'   }
#' @param scale How to scale each axis (see details)
#' @param kernel Type of smoothing kernel to use (see details)
#' @param gamma Exponent for the mapping of input to output values (see
#'   details)
#' @param outfile Optional path to output file (constructed automatically when
#'   missing)
#' @param axis Number indicating 0-indexed axis or character "x", "y" or "z"
#' @param measure Character vector indicating summary function to apply to
#'   values in each column. Choose from \code{c("max", "min", "mean", "median",
#'   "mode", "variance", "skew", "intc", "slope", "error", "sd", "product",
#'   "sum", "L1", "L2", "Linf")}
#' @param cropmin,cropmax triples defining the lower and higher corners of
#'   bounding box
#' @return Logical indicating success
#' @export
#' @seealso \code{\link{NrrdSave}}, \code{\link{NrrdMerge}}
#' @inheritParams NrrdResample
#' @examples
#' \dontrun{
#' # NB cropping from 50th slice to 50 before final slice
#' NrrdProject(infile,axis='z',cropmin='0 0 50',cropmax='0 0 M-50')
#' }
NrrdProject<-function(infile,outfile,axis,
  measure=c("max", "min", "mean", "median", "mode", "variance", "skew",
  "intc", "slope", "error", "sd", "product", "sum", "L1", "L2", "Linf"),
  scale="x0.3333 x0.333",kernel='cheap',gamma=NA,cropmin=NULL,cropmax=NULL,
  suffix=NULL,
  CreateDirs=TRUE,Verbose=TRUE,Force=FALSE,UseLock=FALSE){
  measure=match.arg(measure)
  if (missing(outfile)) {
    if(is.null(suffix)) suffix=paste("-",axis,measure,sep="")
    outfile=sub("\\.nrrd$",paste(suffix,".png",sep=""),infile)
  }
  if(!file.exists(infile)) stop("infile: ",infile," does not exist")
  if(is.character(axis)) {
    axis=match(tolower(axis),c("x",'y','z'))-1
    if(is.na(axis)) stop("Unable to recognise nrrd axis")
  }
  # return TRUE to signal output exists (we just didn't make it now)
  if(!Force && !RunCmdForNewerInput(NULL,infile,outfile)) return (TRUE)
  if(CreateDirs && !file.exists(dirname(outfile))) dir.create(dirname(outfile),recursive = TRUE)
  lockfile=paste(outfile,sep=".","lock")
  # return FALSE to signal output doens't exist
  if(UseLock && !makelock(lockfile)) return (FALSE)
  if(is.numeric(scale)) scale=paste(scale,collapse=" ")
  if(!is.null(cropmax)||!is.null(cropmin)){
    cmd=paste("unu crop",
      ifelse(is.null(cropmax),'',paste("-max",cropmax)),
      ifelse(is.null(cropmin),'',paste("-min",cropmin)),
      "-i",shQuote(infile),
      "| unu resample -s",scale," = -k ",kernel,
      "| unu project -a",axis,"-m ",measure," | unu quantize -b 8 ")
  } else cmd=paste("unu resample -s",scale," = -k ",kernel," -i",shQuote(infile),
    "| unu project -a",axis,"-m ",measure," | unu quantize -b 8 ")

  if(!is.na(gamma)){
    cmd=paste(cmd,"| unu gamma --gamma",gamma)
  }
  cmd=paste(cmd,"| unu save -f png -o",shQuote(outfile))
  rval = system(cmd)
  if(Verbose) cat(".")
  if(UseLock) unlink(lockfile)
  if(rval!=0) stop("unu error ",rval," in NrrdProject")
  return(TRUE)
}

#' Merge multiple NRRD images into a multichannel PNG
#'
#' @param outdir Output directory
#' @param axis 0-indexed axis to join (merge) along (defaults to 0)
#' @param ... Currently ignored
#'
#' @inheritParams Nrrd2op
#' @inheritParams NrrdResample
#' @return Logical value indicating whether output exists
#' @export
NrrdMerge<-function(infiles,outdir=NULL,outfile=NULL,axis=0,
  CreateDirs=TRUE,Verbose=TRUE,Force=FALSE,UseLock=FALSE,...){
  if(is.null(outfile) && is.null(outdir)) {
    outfile=sub("\\.[^.]+$","-merge.png",infiles[1])
  } else if (is.null(outfile)){
    outfile=file.path(outdir,basename(infiles[1]))
    outfile=sub("\\.[^.]+$",".png",outfile)
  } else {
    # make sure this is a png
    outfile=sub("\\.[^.]+$",".png",outfile)
  }

  if(!all(file.exists(infiles)))
    stop("one or more infiles: ",infiles," do not exist")
  # return TRUE to signal output exists (we just didn't make it now)
  if(!Force && !RunCmdForNewerInput(NULL,infiles,outfile)) return (TRUE)
  if(CreateDirs && !file.exists(dirname(outfile)))
    dir.create(dirname(outfile),recursive = TRUE)
  lockfile=paste(outfile,sep=".","lock")
  # return FALSE to signal output doens't exist
  if(UseLock && !makelock(lockfile)) return (FALSE)

  cmd=paste("unu join -a", axis, "-incr -i",
            paste(shQuote(infiles),collapse=" "),
            "-o",shQuote(outfile))
  cat(cmd, "\n")
  rval = system(cmd)
  if(Verbose) cat(".")
  if(UseLock) unlink(lockfile)
  if(rval!=0) stop("unu error ",rval," in NrrdProject")
  return(TRUE)
}

#' Flip a NRRD image along one axis
#'
#' @param axes Which axes to flip (0-indexed integer)
#' @param suffix Suffix to add to input file to construct output file.
#' @param flip_space_directions Whether to multiply the space directions of the
#'   selected \code{axes} by -1. This is the default behaviour of unu, but
#'   sometimes you just want to flip the image data without touching the
#'   metadata - if so set \code{flip_space_directions=FALSE}.
#' @param endian Whether output image should be big or little endian for
#'   multibyte data types (essentially all other than 8 bit). Defaults to
#'   endianness of current machine.
#' @param OverWrite Whether to overwrite (yes or no) or update output image.
#'
#' @return \code{TRUE} if output exists
#' @export
#'
#' @inheritParams NrrdResample
#' @inheritParams NrrdProject
#' @examples
#' \dontrun{
#' # flip the first (x) axis
#' # will make input-flip.nrrd
#' NrrdFlip('input.nrrd', axes=0)
#'
#' # same but specify outfile name
#' NrrdFlip('input.nrrd', outfile='output.nrrd', axes=0)
#'
#' # flip both axes (nb will do so from left to right)
#' NrrdFlip('input.nrrd', axes=0:1)
#'
#' # flip y axis image data but leave space directions untouched
#' NrrdFlip('input.nrrd', axes=1, flip_space_directions=FALSE)
#' }
NrrdFlip<-function(infile, outfile, axes, suffix=NULL,
                   flip_space_directions=TRUE,
                   OverWrite=c("no","update","yes"),
                   endian=.Platform$endian,
                   CreateDirs=TRUE,UseLock=FALSE, Verbose=TRUE){
  # TODO would be nice if we could
  # a) have an absolute flip mode that checks the nrrd content field
  # b) similarly checks whether output image has been flipped accordingly

  if(is.logical(OverWrite)) OverWrite=ifelse(OverWrite,"yes","no")
  else OverWrite=match.arg(OverWrite)

  endian=match.arg(endian, c("big","little"))
  if (missing(outfile)) {
    if(is.null(suffix)) suffix=paste("-flip",paste(axes,collapse=""),sep="")
    outfile=sub("\\.nrrd$",paste(suffix,".nrrd",sep=""),infile)
  }

  if(!file.exists(infile)) stop("infile: ",infile," does not exist")

  if(!isTRUE(flip_space_directions)) {
    # want to keep the original header to use later
    inh=read.nrrd.header(outfile)
    inht=attr(inh, 'headertext')
    oldspacedirs=grep("^space directions: ", inht, value = T)
    if(!length(oldspacedirs))
      stop("Unable to find old space directions, so don't know to fix spatial ",
           "metadata after flip!")
  }

  # return TRUE to signal output exists (whether or not we made it)
  if(file.exists(outfile)){
    if(OverWrite=="no"){
      if(Verbose) cat("Output",outfile,"already exists; use OverWrite=\"yes\" or \"update\" to overwrite or update\n")
      return(TRUE)
    } else if(OverWrite=="update"){
      # check modification times
      if(!RunCmdForNewerInput(NULL,infile,outfile)) return (TRUE)
    } else if(Verbose) cat("Overwriting",outfile,"because OverWrite=\"yes\"\n")
  }

  if(CreateDirs && !file.exists(dirname(outfile))) dir.create(dirname(outfile),recursive = TRUE)
  lockfile=paste(outfile,sep=".","lock")
  # return FALSE to signal output doesn't (yet) exist
  if(UseLock && !makelock(lockfile)) return (FALSE)
  on.exit(unlink(lockfile))

  # First axis
  cmd=paste("unu flip -a",axes[1],"-i",shQuote(infile))
  # any additional axes
  for(axis in axes[-1]){
    cmd=paste(cmd,"| unu flip -a",axis)
  }
  # save
  cmd=paste(cmd," | unu save -f nrrd -e gz -en",endian,"-o",shQuote(outfile))

  rval = system(cmd)
  if(Verbose) cat(".")
  if(rval!=0) stop("unu error ",rval," in NrrdProject")

  if(!isTRUE(flip_space_directions)) {
    # need to do some surgery on the nrrd header
    # FIXME - should initially save as detached nrrd if we need to do this
    # to avoid excessive file copies
    attr(inh,"headertext")
    AddOrReplaceNrrdHeaderField(infile = outfile,
                                newfields = c(`space directions`=oldspacedirs))
  }
  return(TRUE)
}

#' Save a nrrd file in a different encoding / format
#'
#' In outfile is missing, will overwrite infile
#' @param infile,outfile Paths to input and output nrrds
#' @param format (one of 6 supported by unu save, default nrrd)
#' @param encoding (one of 5 supported by unu save, default gz when nrrd)
#' @param CreateDirs Whether to make missing direcories implied by output path
#' @param UseLock Whether to make a lockfile (useful for parallel processing)
#' @param DryRun Return command instead of running it
#' @return path to ouput file
#' @export
#' @seealso \code{\link{NrrdCrc}}
NrrdSave<-function(infile,outfile,format=c("nrrd","pnm","text","vtk","png","eps"),
  encoding=ifelse(format=='nrrd','gzip','raw'),
  CreateDirs=TRUE,UseLock=FALSE,DryRun=FALSE){
  format=match.arg(format)
  encodings=c("raw","ascii","hex","gzip","bzip2")
  encoding=encodings[pmatch(encoding,encodings)]
  if(is.na(encoding)) stop("Invalid encoding")

  if(missing(outfile)) {
    outfile=infile
    samefile=TRUE
  } else samefile=FALSE

  cmd=paste('unu save','--format',format,'-e',encoding,'-i',shQuote(infile),'-o',shQuote(outfile))
  if(DryRun) return(cmd)

  if(CreateDirs && !file.exists(dirname(outfile))) dir.create(dirname(outfile),recursive = TRUE)
  lockfile=paste(outfile,sep=".","lock")
  # return FALSE to signal output doesn't (yet) exist
  if(UseLock && !nat.utils::makelock(lockfile)) return (FALSE)
  on.exit(unlink(lockfile))

  rval=system(cmd)==0
  attr(rval,'outfile')=outfile
  rval
}
