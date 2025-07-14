#' swtable
#' 
#' Function to add an R object to a \code{"\linkS4class{swStream}"} object.
#' 
#' This method provides the possibility to display the content of an array-like
#' object in a table. It has to be specified, which two dimensions should be
#' shown by choosing them before the object is sent to the function.  The
#' method is built on the \code{xtable} method. The table is then created with
#' the \code{print.xtable} method, that takes all kind of arguments like
#' placement etc. See that manuals for more information
#' 
#' @param stream The \code{swStream} object to be modified.
#' @param x An array or something that can be converted to an array, that shall
#' be shown as a table. X should only contain 2 dimensions with more than 1
#' element, otherwise it cannot be plotted as a matrix.
#' @param caption Description to be displayed in the pdf.
#' @param label Label for the tex file for referencing.
#' @param transpose Should the matrix be transposed before it is plotted?
#' @param digits How many digits shall be shown for each number? Either one
#' number or ncol()+1 numbers for each column seperately.
#' @param vert.lines 1 for vertical lines between the columns, 0 for no
#' vertical lines. If you want lines only somewhere, set this to 0 and use
#' align.
#' @param hor.lines 1 for horizontal lines between the rows, 0 for no
#' horizontal lines. A vector of length equal to nrow(x)+2 (for line above
#' first row also) with 1 and 0 indicates to put lines wherever a one is
#' mentioned, ohters will stay without lines. A vector of numbers between -1
#' and "nrow(x)", inclusive, indicating the rows after which a horizontal line
#' should appear.
#' @param align Alignment of the table content. You can specify a single
#' character: "c" for centered, "l" for left, "r" for right. If you want
#' different alignments for different columns, specify a vector of length equal
#' to the table columns plus one (for the row names) with the alignment for
#' each column. If you want vertical lines only between some columns or around
#' the table, set vert.lines to 0 and add "|" to the alignment vector, where
#' you want to have vertical lines.
#' @param display How shall the content be displayed? Use "d" (for integers),
#' "f", "e", "E", "g", "G", "fg" (for reals), or "s" (for strings).
#' @param colsplit Number of columns after which the table is split into two (Integer). 
#' Table will not be splitted up if set to NULL.
#' @param ... Further options passed to print.xtable.
#' @return No return value.
#' @author Markus Bonsch, Jan Philipp Dietrich
#' @importFrom xtable xtable
#' @export
#' @seealso
#' \code{"\linkS4class{swStream}"},\code{\link{swopen}},\code{\link{swclose}},\code{\link{swlatex}},\code{\link{swR}},\code{\link{swfigure}}
#' @examples
#' \dontrun{
#' sw <- swopen(outfile="test.pdf")
#' a <- array(1:45,c(5,2,3),dimnames=list(paste0("a",1:5),paste0("b",1:2),paste0("c",1:3)))
#' print(a)
#' swtable(sw, a[,,1], "test 1")
#' swtable(sw, a[,1,], "test 2")
#' swtable(sw, a[,1,], "transpose test", transpose=TRUE)
#' swtable(sw, a[,1,], "colsplit test", transpose=TRUE, colsplit=2)
#' swclose(sw)
#' }
swtable<-function(stream,x,caption=NULL, label=NULL,transpose=FALSE, digits=NULL, vert.lines=1, hor.lines=1, align="c", display=NULL, colsplit=NULL, ...){

  if(!is.null(caption)) caption <- gsub("$","\\$",caption,fixed=TRUE)
  
  if(is.data.frame(x)) x <- as.matrix(x)
  
  x <- as.array(x)
  if(length(dim(x))>2) {
    #create matrix from data which has the wrong dimensionality
    idim <- (dim(x)!=1)
    if(sum(idim)>2) stop("Input has more than 2 dimensions which contain more than 1 element. Unable to convert this data to a xtable matrix")
    if(sum(idim)==0) idim[1:2] <- TRUE 
    if(sum(idim)==1) idim[which(!idim)[1]] <- TRUE  
    x <- array(x,dim=dim(x)[idim],dimnames=list(dimnames(x)[[which(idim)[1]]],dimnames(x)[[which(idim)[2]]]))
  }
  #Starting from here x is 2D
  if(transpose) x <- aperm(x)

  envir<-stream
  stream<-get("stream",envir=envir,inherits=FALSE)
  if (inherits(stream, "swStream")) {
    # Take care of alignment, vertical and horizontal lines
    if(length(hor.lines)==1){
      if(hor.lines==0){
        hor.lines=NULL   
      } else if (hor.lines==1){
        hor.lines=-1:dim(x)[1]
      } else {
        warning("hor.lines not specified correctly, no horizontal lines will be plotted in the table")
        hor.lines<-NULL
      }
    } else if (is.vector(hor.lines)){
      if((length(hor.lines)==dim(x)[1]+2)&&(min(hor.lines)>=0)&&(max(hor.lines<=1))){
        tmp<-vector()
        for(i in -1:(length(hor.lines)-2)){
          if(hor.lines[i+2]==1) tmp<-c(tmp,i)
        }
        hor.lines<-tmp
      } else if((min(hor.lines)>=-1)&&(max(hor.lines)<=dim(x)[1])){
        print("yresy")
        #nothing to be done
      } else{
        warning("hor.lines not specified correctly, no horizontal lines will be plotted in the table")
        hor.lines<-NULL        
      }
    } else{
      warning("hor.lines not specified correctly, no horizontal lines will be plotted in the table")
      hor.lines<-NULL      
    }
    
    if(length(align)==1){
      if(vert.lines==0){
        align<-rep(align,dim(x)[2]+1)
      } else if (vert.lines==1){
        align<-c(rep(paste("|",align,sep=""),dim(x)[2]),paste("|",align,"|",sep=""))
      }
    } else if(length(align)==dim(x)[2]+1){
      if(vert.lines==1){
        if(length(grep("\\|",align))==0){
          align<-c(paste("|",align[1:length(align)-1],sep=""),paste("|",align[length(align)],"|",sep=""))
        } else{
          tmp<-vector()
          for(i in 1:length(align)){
            if(length(grep("\\|",align[1]))==0){
              tmp<-c(tmp,paste("|",align[i],sep=""))
            } else{
              tmp<-c(tmp,align[i])
            }
          }
          #align <- tmp
        }
      } else{
        #nothing to be done
      }
    } else{
      warning("Alignment is wrongly specified. Standard alignment will be used")
      align<-c(rep("|c",dim(x)[2]),"|c|")
    }
  out <- list()
  i <- 1
   if(!is.null(colsplit)) {
     parts <- dim(x)[2] %/% colsplit + 1
     while(dim(x)[2]>colsplit) {
       out[[i]] <-xtable(x[,1:colsplit, drop=FALSE],caption=paste0(caption," [PART ", i, "/",parts,"]"),label=label,align=align[1:(colsplit+1)],digits=digits,display=display) 
       x <- x[,-(1:colsplit), drop=FALSE]
       align <- align[-(2:(colsplit+1))]
       align[1] <- sub("^\\|","",align[1])
       i <- i + 1 
     }
   } 
   if(i>1) {
     caption <- paste0(caption," [PART ", i, "/",parts,"]")
   }
   out[[i]] <- xtable(x,caption=caption,label=label,align=align,digits=digits,display=display)
   for(o in out) {
     if("hline.after" %in% names(list(...))){
       if(Sys.info()[['sysname']]=="Linux"){
         swlatex(envir,print(o,file='/dev/null/',...))
       } else{
         swlatex(envir,print(o,file='NUL',...))
       }
     } else {
       if(Sys.info()[['sysname']]=="Linux"){
         swlatex(envir,print(o,hline.after=hor.lines,file='/dev/null',...))
       } else{
         swlatex(envir,print(o,hline.after=hor.lines,file='NUL',...))
       }
     }
   }
  }else{
    stop("Input is not a sweaveStream!")
  }
}
