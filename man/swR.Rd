% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swR.R
\name{swR}
\alias{swR}
\title{swR}
\usage{
swR(stream, func, ..., option = "echo=FALSE")
}
\arguments{
\item{stream}{The \code{swStream} object to be modified.}

\item{func}{The R command, that shall be ecexuted. Can be specified as a
string for back compatibility}

\item{...}{Additional parameters passed to the function.}

\item{option}{Formatting options.}
}
\value{
No return value.
}
\description{
Function to write R code to a \code{"\linkS4class{swStream}"} object.
}
\details{
Method designed to write the output of R commands to a swStream object,
taking care that it is correctly embedded. As \code{func} argument, you can
pass either the function itself or its name as a string. User defined
functions will only work with option one, two is only for back
compatibility.
}
\examples{
\dontrun{
test <- swopen(outfile = "test.pdf")
swR(test, print, ls)
swR(test, print, "bla_blubb")
x <- c(1, 2, 3)
swR(test, print, paste(x, "bla"))
swclose(test)
# Only for back compatibility
swR(test, "print", "bla_blubb")
}


}
\seealso{
\code{"\linkS4class{swStream}"},\code{\link{swopen}},\code{\link{swclose}},\code{\link{swlatex}},\code{\link{swtable}},\code{\link{swfigure}}
}
\author{
Markus Bonsch
}
