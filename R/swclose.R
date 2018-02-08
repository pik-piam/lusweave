#' Close a swStream
#' 
#' Function to create a pdf from a \code{"\linkS4class{swStream}"} object
#' 
#' Creates a pdf with of the name specified in \code{stream@name}, with the
#' content specified in \code{stream@content}, using \code{stream@Sweave.sty}
#' as the style file.
#' 
#' @param stream The \code{swStream} object to be processed.
#' @param outfile Name of the pdf to be produced. Overwrites the name given in
#' swopen
#' @param latexpath Path of the LaTeX distribution (only necessary if not part
#' of the PATH variable).
#' @param clean_output If true, all auxiliary files will be deleted. If False,
#' they remain, including each plot in its own pdf.
#' @param engine Engine to use for conversion. Currently available: Sweave and
#' knitr.
#' @param save_stream If true (default) stream is saved to .rda file.
#' @param knitquiet If false (default) progressbar and messages are printed
#' otherwise suppressed.
#' @return No return value.
#' @author Markus Bonsch, David Klein
#' @export
#' @seealso
#' \code{"\linkS4class{swStream}"},\code{\link{swopen}},\code{\link{swlatex}},\code{\link{swR}},\code{\link{swtable}},\code{\link{swfigure}}
#' @examples
#' \dontrun{
#' test<-swopen(outfile="test.pdf")
#' swlatex(test,"tttteeeesssstttt")
#' swclose(test)
#' #Change the name
#' swclose(test,outfile="test_2.pdf")
#' }
#' 
#method to create a pdf from the swStream object
swclose<-function(stream,outfile="",latexpath="",clean_output=TRUE, engine="knitr", save_stream=TRUE, knitquiet = TRUE){
  
  #Write "content" (extended by "\end{document}")to an Rnw file, 
  #sweave this file and produce the pdf from the .tex file. Then delete the sweaveStream
  if(is.environment(stream)) {
    envir<-stream
    stream<-get("stream",envir=envir,inherits=FALSE)
  } else {
    envir <- new.env()
    assign("stream",stream,envir=envir)
  }
  if(class(stream)=="swStream"){
     #update the name if necessary
     if(outfile!=""){
       if(length(strsplit(outfile,split="/")[[1]])>1){
         tmp<-strsplit(outfile,split="/")[[1]]
         outfile<-tmp[length(tmp)]
         folder<-paste(tmp[1:(length(tmp)-1)],collapse="/") 
       } else{
         folder<-""
       }
       outfile<-strsplit(outfile,".pdf", fixed=TRUE)[[1]]
       outfile<-gsub("\\.","-_-_-",outfile)
       stream@auxfiles<-gsub(strsplit(stream@name,".pdf", fixed=TRUE)[[1]],strsplit(outfile,".pdf", fixed=TRUE)[[1]],stream@auxfiles)
       stream@name<-outfile
       stream@folder<-folder
       assign("stream",stream,envir = envir)
     }
     
  	 stream@content<-c(stream@content,"\\end{document}")
     
     this_dir<-getwd()
     if(stream@folder!="") setwd(stream@folder)
     
     if(save_stream) save(stream,file=paste0(stream@name,".rda"),compress = "xz")
     
		 writeLines(stream@content,paste(strsplit(stream@name,".pdf", fixed=TRUE)[[1]],".Rnw",sep=""))
		 #create the style file if no external one is specified
		 writeLines(stream@Sweave.sty,"Sweave.sty")
		 #execute the Sweave routine which executes the R code in the Rnw file and produces a tex file

     
     #Attach the stream to the global environment because this is where Sweave does the evaluation
		 globalenv = .GlobalEnv
     tmp<-try(get("stream",envir=globalenv),silent=TRUE)
     assign("stream",stream,envir=globalenv)
     #now run Sweave or knitr
		 rnwfile <- paste(strsplit(stream@name,".pdf", fixed=TRUE)[[1]],".Rnw",sep="")
     if(engine=="Sweave") {
       Sweave(rnwfile,encoding="bytes")
     } else if(engine=="knitr"){
       if (!requireNamespace("knitr", quietly = TRUE)) stop("The package knitr is not available!")
       message("Start processing pdf with knitr, this may take a while...")
       start <- Sys.time()
       suppressMessages(knitr::Sweave2knitr(rnwfile,output = rnwfile))
       suppressWarnings(knitr::knit(rnwfile,quiet = knitquiet))
       diff <- difftime(Sys.time(),start)
       unit <- attr(diff,"units")
       message(paste0("...processing with knitr finished in ",round(diff,2)," ",unit,"!"))
     } else {
       stop("Unknown engine ",engine)
     }
 
     #restore stream object in the global environment
     if(!is(tmp,"try-error")) assign("stream",tmp,envir=globalenv)
     
     #check for command pdflatex and change latexpath in the case that it cannot be found
     latexpaths <- c(latexpath,"/iplex/01/sys/applications/texlive/bin/x86_64-linux/","","not found")
     for(latexpath in latexpaths) {
       if(Sys.which(paste0(latexpath,"pdflatex"))!="") break
     }     
     if(latexpath=="not found") stop("Executable pdflatex could not be found. Please check your latexpath setting")
    #compile to pdf, needed two times for generation of table of contents
    for(i in 1:2) {
      error_code<-suppressWarnings(system(paste0(latexpath,"pdflatex --interaction=nonstopmode ",paste0(strsplit(stream@name,".pdf", fixed=TRUE)[[1]],".tex")),intern=TRUE))
    }
    if(!is.null(attr(error_code,"status"))) warning("Execution of pdflatex reported an error (code ",attr(error_code,"status"),") please check the corresponding log file for further information.")
    file.rename(paste(strsplit(stream@name,".pdf", fixed=TRUE)[[1]],".pdf",sep=""),paste(gsub("-_-_-","\\.",strsplit(stream@name,".pdf", fixed=TRUE)[[1]]),".pdf",sep=""))
    file.rename(paste(strsplit(stream@name,".pdf", fixed=TRUE)[[1]],".log",sep=""),paste(gsub("-_-_-","\\.",strsplit(stream@name,".pdf", fixed=TRUE)[[1]]),".log",sep=""))
    #remove unnecessary compilation files
    if(clean_output==TRUE) log<-suppressWarnings(file.remove(stream@auxfiles))
    setwd(this_dir)
  }else{
	  stop("Input is not a sweaveStream!")
	 }
}