% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swlatex.R
\name{swlatex}
\alias{swlatex}
\title{swlatex}
\usage{
swlatex(stream, ...)
}
\arguments{
\item{stream}{The \code{swStream} object to be modified.}

\item{...}{The content, that is to be added.}
}
\value{
No return value.
}
\description{
Function to write LaTeX code to a \code{"\linkS4class{swStream}"} object.
}
\details{
Writes plain text to the swStream object. Be careful to escape R special
characters like "\".
}
\examples{
\dontrun{
test <- swopen(outfile = "test.pdf")
swlatex(test, "This is a test text")
swclose(test)
}

}
\seealso{
\code{"\linkS4class{swStream}"},\code{\link{swopen}},\code{\link{swclose}},\code{\link{swR}},\code{\link{swtable}},\code{\link{swfigure}}
}
\author{
Markus Bonsch
}
