#' swR
#' 
#' Function to write R code to a \code{"\linkS4class{swStream}"} object.
#' 
#' Method designed to write the output of R commands to a swStream object,
#' taking care that it is correctly embedded. As \code{func} argument, you can
#' pass either the function itself or its name as a string. User defined
#' functions will only work with option one, two is only for back
#' compatibility.
#' 
#' @usage swR(stream,func,...,option = "echo=FALSE")
#' @param stream The \code{swStream} object to be modified.
#' @param func The R command, that shall be ecexuted. Can be specified as a
#' string for back compatibility
#' @param ... Additional parameters passed to the function.
#' @param option Formatting options.
#' @return No return value.
#' @author Markus Bonsch
#' @export
#' @seealso
#' \code{"\linkS4class{swStream}"},\code{\link{swopen}},\code{\link{swclose}},\code{\link{swlatex}},\code{\link{swtable}},\code{\link{swfigure}}
#' @examples
#' \dontrun{
#' test<-swopen(outfile="test.pdf")
#' swR(test,print,ls)
#' swR(test,print,"bla_blubb")
#' x<-c(1,2,3)
#' swR(test,print,paste(x,"bla"))
#' swclose(test)
#' # Only for back compatibility
#' swR(test,"print","bla_blubb")
#' }
#' 
#' 
# write R code to a sweaveStream object:
# define the function and its arguments
# <<"option">>= 
# "func(...)" 
# @
swR<-function(stream,func,...,option = "echo=FALSE"){
  envir<-stream
  stream<-get("stream",envir=envir,inherits=FALSE)
  orig_stream <- stream
  stufftowrite <- NULL
  if(class(stream)=="swStream"){
    tryCatch({
      pos<-length(stream@functions)
      args<-list(...)
      
      #save func (necessary in case, it is user defined)
      if(is.function(func)){
        stream@functions[[pos+1]]<-func
      } else{ #needed for back compatibility
        stream@functions[[pos+1]]<-get(func,pos=1,inherits=TRUE)
      }
      #paste the command
      stufftowrite<-paste("stream@functions[[",pos+1,"]](",sep="")
      if(length(args)>0){
        #save all arguments as a list in the objects slot of the stream object.
        stream@arguments[[pos+1]]<-args
        #paste the arguments in the appropriate format
        for(i in 1:length(args)){
          if(any(names(args)!="")){
            if(names(args)[i]!=""){
              stufftowrite<-paste(stufftowrite,names(stream@arguments[[pos+1]])[i],"=","stream@arguments[[",pos+1,"]][[",i,"]],",sep="")
            }else{
              stufftowrite<-paste(stufftowrite,"stream@arguments[[",pos+1,"]][[",i,"]],",sep="")
            }
          }else{
            stufftowrite<-paste(stufftowrite,"stream@arguments[[",pos+1,"]][[",i,"]],",sep="")
          }
        }
      }
      #remove last comma
      if(substr(stufftowrite,nchar(stufftowrite),nchar(stufftowrite))==",") stufftowrite<-substr(stufftowrite,1,nchar(stufftowrite)-1)
      #add options and closing bracket
      stufftowrite<-paste(stufftowrite,")",sep="")
      stufftowrite<-c(paste("<<",option,">>=",sep=""),stufftowrite,"@")
    }, error=function(e){warning("Error occurred in swR, stream will be set back to previous value.\n   Error message: ",e$message,call. = FALSE);stream <- orig_stream})
    # Save the updated stream object
    assign("stream",stream,envir = envir)
    swlatex(envir,stufftowrite)
    }else{
      stop("Input is not a sweaveStream!")
    }
}
 