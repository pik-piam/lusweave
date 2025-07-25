% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swfigure.R
\name{swfigure}
\alias{swfigure}
\title{swfigure}
\usage{
swfigure(
  stream,
  plot_func,
  ...,
  tex_caption = "",
  tex_label = "",
  fig.placement = "H",
  fig.width = "",
  fig.orientation = "portrait",
  sw_option = "",
  sw_label = "AUTO"
)
}
\arguments{
\item{stream}{The \code{swStream} object to be modified.}

\item{plot_func}{The R command, that produces the plot.}

\item{...}{The arguments of the plotting command.}

\item{tex_caption}{caption of the plot.}

\item{tex_label}{label for the plot in .tex file for referencing.}

\item{fig.placement}{Where to put the figure in the pdf. Typical LaTeX
allocation like "h","ht","b" allowed.}

\item{fig.width}{Width of the figure in the pdf as fraction of textwidth.}

\item{fig.orientation}{landscape or portrait. If wrongly specified, parts of
the figure will be cut (e.g. necessary for maps).}

\item{sw_option}{Sweave options. See
http://www.stat.uni-muenchen.de/~leisch/Sweave/Sweave-manual.pdf for details}

\item{sw_label}{label for the plot in the Sweave file. (Not very likely
necessary to be changed)}
}
\value{
No return value.
}
\description{
Function to add R plots to a \code{"\linkS4class{swStream}"} object.
}
\details{
Method designed to add an R plot to a swStream object, taking care that it
is correctly embedded.
}
\examples{
\dontrun{
test <- swopen(outfile = "test.pdf")
swfigure(test, "plot", 0, 0, tex_caption = "test figure", fig.width = 0.5)
swclose(test)
}
}
\seealso{
\code{"\linkS4class{swStream}"},\code{\link{swopen}},\code{\link{swclose}},\code{\link{swlatex}},\code{\link{swR}},\code{\link{swtable}}
}
\author{
Markus Bonsch
}
