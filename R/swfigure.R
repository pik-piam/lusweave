#' swfigure
#' 
#' Function to add R plots to a \code{"\linkS4class{swStream}"} object.
#' 
#' Method designed to add an R plot to a swStream object, taking care that it
#' is correctly embedded.
#' 
#' @param stream The \code{swStream} object to be modified.
#' @param plot_func The R command, that produces the plot.
#' @param ... The arguments of the plotting command.
#' @param tex_caption caption of the plot.
#' @param tex_label label for the plot in .tex file for referencing.
#' @param fig.placement Where to put the figure in the pdf. Typical LaTeX
#' allocation like "h","ht","b" allowed.
#' @param fig.width Width of the figure in the pdf as fraction of textwidth.
#' @param fig.orientation landscape or portrait. If wrongly specified, parts of
#' the figure will be cut (e.g. necessary for maps).
#' @param sw_option Sweave options. See
#' http://www.stat.uni-muenchen.de/~leisch/Sweave/Sweave-manual.pdf for details
#' @param sw_label label for the plot in the Sweave file. (Not very likely
#' necessary to be changed)
#' @return No return value.
#' @author Markus Bonsch
#' @export
#' @seealso
#' \code{"\linkS4class{swStream}"},\code{\link{swopen}},\code{\link{swclose}},\code{\link{swlatex}},\code{\link{swR}},\code{\link{swtable}}
#' @examples
#' \dontrun{
#' test<-swopen(outfile="test.pdf")
#' swfigure(test,"plot",0,0,tex_caption="test figure",fig.width=0.5)
#' swclose(test)
#' }
swfigure<-function(stream,plot_func,...,tex_caption="",tex_label="",fig.placement="H",fig.width="",fig.orientation="portrait",sw_option="",sw_label="AUTO"){
  envir<-stream
  stream<-get("stream",envir=envir,inherits=FALSE)
  if (inherits(stream, "swStream")) {

    #each plot has to have a different label. AUTO will take care of that
    if(sw_label=="") stop("Each plot needs a different label to make things work. If you don't specify any label, it will be taken care of automatically")
    if(sw_label=="AUTO") sw_label=length(stream@functions)+1
    
    #remember the names of the plot files to delete them afterwards.
    stream@auxfiles<-c(stream@auxfiles,paste(strsplit(stream@name,"\\.pdf")[[1]],"-",sw_label,".",c("eps","pdf"),sep=""))
    assign("stream",stream,envir = envir)    
    #Add the orientation and the options
    if(fig.orientation=="landscape") sw_label<-paste(sw_label,",width=11.69",sep="")
    if(sw_option!="")sw_label<-paste(sw_label,",",sw_option,sep="")
    
    # change the figure width
    if(fig.width!="") swlatex(envir,stufftowrite=paste("\\setkeys{Gin}{width=",fig.width,"\\textwidth}"))
    
    #build figure environment and put the plot command
    startplot<-"\\begin{figure}"
    if(fig.placement!="") startplot<-paste(startplot,"[",fig.placement,"]",sep="")
    startplot<-c(startplot,"\\begin{center}")
    swlatex(envir,startplot)
    swR(envir,plot_func,...,option=paste(sw_label,",fig=TRUE,echo=FALSE",sep=""))
    endplot<-"\\end{center}"
    if(tex_caption!="") endplot<-c(endplot,paste("\\caption{",gsub("$","\\$",tex_caption,fixed=TRUE),"}",sep=""))
    if(tex_label!="") endplot<-c(endplot,paste("\\label{",gsub("$","\\$",tex_label,fixed=TRUE),"}",sep=""))
    endplot<-c(endplot,"\\end{figure}")
    swlatex(envir,endplot)
    
    # change the figure width back to normal again
    if(fig.width!="") swlatex(envir,stufftowrite=paste("\\setkeys{Gin}{width=1\\textwidth}"))
  }else{
    print("ERROR! Input is not a sweaveStream!")
  }
}