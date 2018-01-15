#' Class "swStream" ~~~
#' 
#' The swStream class provides an interface to the Sweave method and allows for
#' easy creation of pdf's from R output.
#' 
#' The Sweave method is an interface between R and LaTeX. It processes .Rnw
#' files, that can contain LaTeX code as well as R code into proper .tex files,
#' replacing the R commands by their output. See
#' \code{http://www.stat.uni-muenchen.de/~leisch/Sweave/} for details.\cr An
#' additional style file is needed to compile the .tex file into a pdf.
#' 
#' @name swStream-class
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("swStream", name,folder, content, Sweave.sty, arguments,
#' functions, auxfiles, envir)}.
#' @author Markus Bonsch
#' @seealso
#' \code{\link{swopen}},\code{\link{swclose}},\code{\link{swlatex}},\code{\link{swR}},\code{\link{swtable}},\code{\link{swfigure}}
#' @keywords classes
#' @examples
#' \dontrun{
#' showClass("swStream")
#' }
setClass("swStream",representation(name = "character",content = "vector",Sweave.sty="vector",functions="list",arguments="list",auxfiles="vector",envir="environment"),prototype = list(name = "",content = c("",""),Sweave.sty=c("",""),functions=list(),arguments=list(),auxfiles=c("",""),envir=.GlobalEnv))
