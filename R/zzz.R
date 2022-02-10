#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (packageVersion("knitr") == "1.37") {
    packageStartupMessage("The lusweave package (for creating pdfs) does not work with knitr 1.37. ",
                          "You can get a fixed development version of knitr with\n",
                          "remotes::install_github('chroetz/knitr', ref = 'fix-Sweave2knitr')\n",
                          "or get knitr 1.36 with\n",
                          "remotes::install_version('knitr', '1.36')")
  }
}
