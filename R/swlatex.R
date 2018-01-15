#' swlatex
#' 
#' Function to write LaTeX code to a \code{"\linkS4class{swStream}"} object.
#' 
#' Writes plain text to the swStream object. Be careful to escape R special
#' characters like "\".
#' 
#' @usage swlatex(stream,...)
#' @param stream The \code{swStream} object to be modified.
#' @param ... The content, that is to be added.
#' @return No return value.
#' @author Markus Bonsch
#' @export
#' @seealso
#' \code{"\linkS4class{swStream}"},\code{\link{swopen}},\code{\link{swclose}},\code{\link{swR}},\code{\link{swtable}},\code{\link{swfigure}}
#' @examples
#' \dontrun{
#' test<-swopen(outfile="test.pdf")
#' swlatex(test,"This is a test text")
#' swclose(test)
#' }
#' 
#write latex code to a sweaveStream object 
#(plain text is added to the content of the sweaveStream object, latex formatting has to be done on your own)
swlatex<-function(stream,...){
  envir<-stream
  stream<-get("stream",envir=envir,inherits=FALSE)
  if(class(stream)=="swStream"){
    stream@content<-c(stream@content,...)
    #make sure, that the object stream is changed in the correct environment (saved in the sweave object)
    assign("stream",stream,envir = envir)
  }else{
    print("ERROR! Input is not a sweaveStream!")
  }
}