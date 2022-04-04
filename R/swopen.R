setClass("swStream",representation(name = "character",folder="character",content = "vector",Sweave.sty="vector",functions="list",arguments="list",auxfiles="vector",envir="environment"),prototype = list(name = "",folder="",content = c("",""),Sweave.sty=c("",""),functions=list(),arguments=list(),auxfiles=c("",""),envir=.GlobalEnv))

#' Open an swStream
#' 
#' Function to create a swStream object, that can be used to create a pdf
#' including R output
#' 
#' 
#' @param outfile name for the output pdf. Can be specified with or w/o the
#' .pdf extension. Can also contain a path to a differetn directory.
#' @param folder Path where the output shall be produced.
#' @param template A template which should be used for the PDF. Either a path
#' to a .tex-file or a vector containing the tex template code or a name of a
#' template which already comes with the library. Currently the following
#' templates are part of the library: "default","default_landscape" and
#' "david".
#' @param style Style information which should be used for the PDF. Either a
#' path to a .sty-file or a vector containing the tex style code or a name of a
#' style which already comes with the library. Currently the following styles
#' are part of the library: "default"
#' @param orientation [DEPRECATED] Please do not use this argument. It is just
#' left in the function for compatibility reasons.
#' @param envir The environment in which the object should be saved. A new
#' environment by default.
#' @return An environment containing a \code{"\linkS4class{swStream}"} object
#' named "stream" with a header for the Rnw file and the content of a
#' stylefile.
#' @author Markus Bonsch, Jan Philipp Dietrich
#' @export
#' @importFrom utils Sweave data packageVersion
#' @importFrom methods is new
#' @seealso
#' \code{"\linkS4class{swStream}"},\code{\link{swclose}},\code{\link{swlatex}},\code{\link{swR}},\code{\link{swtable}},\code{\link{swfigure}}
#' @examples
#' \dontrun{
#' test<-swopen()
#' str(test)
#' 
#' testtwo<-swopen(outfile="test.pdf")
#' str(testtwo)
#' 
#' }
# class to produce pdf's with embedded R tables, plots ...
# The 2 slots are "name", the name of the output pdf and "content", the stuff to write to the file (formatted according to the sweave syntax).

#Version 1.01 - Markus bonsch
# 1.01: Added printnames to swtable


swopen<- function(outfile = "out.pdf",folder="",template="default",style="default",orientation=NULL,envir=new.env()){
  #Check if outfile contains a path
  if(length(strsplit(outfile,split="/")[[1]])>1){
    if(folder!="")stop("ERROR: outfile contains a path and 'folder' is specified. Can not handle that")
    tmp<-strsplit(outfile,split="/")[[1]]
    outfile<-tmp[length(tmp)]
    folder<-paste(tmp[1:(length(tmp)-1)],collapse="/") 
  }
  outfile<-strsplit(outfile,".pdf", fixed=TRUE)[[1]]
  outfile<-gsub("\\.","-_-_-",outfile)
  if(!is.null(orientation)) {
    warning("Orientation argument is deprecated and will probably be removed soon. Please use an appropriate template instead (e.g. default_landscape)")
    if(orientation=="landscape") {
      if(template=="default") {
        template <- "default_landscape"
      } else {
        warning("There is no appropriate landscape landscape version of the template \"",template,"\"!")
      }
    }
  }
  
  #determine the auxiliary files that will be created during the process and which need to be deleted afterwards
  files<-paste(strsplit(outfile,"\\.pdf")[[1]],".",c("aux","tex","Rnw","toc","out"),sep="")
  files<-c(files,"Sweave.sty","Rplots.pdf")
lusweavedata <- NULL
  data("lusweavedata", envir=environment(), package = "lusweave")
  
  #Open the stream object and fill the content with a header for the latex document
  if(length(template)==1) {
    if(substr(template,nchar(template)-3,nchar(template))==".tex") {
      template <- readLines(template)
    } else {
      template <- lusweavedata$templates[[template]]
    }
  }
  sStream<-new("swStream",name = outfile,folder=folder,content = template,auxfiles=files,envir=envir)  

  if(length(style)==1) {
    if(substr(style,nchar(style)-3,nchar(style))==".sty") {
      style <- readLines(style)
    } else {
      style <- lusweavedata$styles[[style]]
    }
  }
  sStream@Sweave.sty<-style

  stream<-envir
  assign("stream",sStream,envir=stream)
  return(stream)
}
