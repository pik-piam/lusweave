# Sweave/Knitr Utilities

R package **lusweave**, version **1.46.5**

[![CRAN status](https://www.r-pkg.org/badges/version/lusweave)](https://cran.r-project.org/package=lusweave) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1158594.svg)](https://doi.org/10.5281/zenodo.1158594) [![R build status](https://github.com/pik-piam/lusweave/workflows/check/badge.svg)](https://github.com/pik-piam/lusweave/actions) [![codecov](https://codecov.io/gh/pik-piam/lusweave/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/lusweave) [![r-universe](https://pik-piam.r-universe.dev/badges/lusweave)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Set of tools which simplify the usage of SWeave/Knitr in R
    and allow to easily create PDF files from within R.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("lusweave")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **lusweave** in publications use:

Bonsch M, Dietrich J, Klein D, Humpenoeder F (2025). "lusweave: Sweave/Knitr Utilities." doi:10.5281/zenodo.1158594 <https://doi.org/10.5281/zenodo.1158594>, Version: 1.46.5, <https://github.com/pik-piam/lusweave>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {lusweave: Sweave/Knitr Utilities},
  author = {Markus Bonsch and Jan Philipp Dietrich and David Klein and Florian Humpenoeder},
  doi = {10.5281/zenodo.1158594},
  date = {2025-07-16},
  year = {2025},
  url = {https://github.com/pik-piam/lusweave},
  note = {Version: 1.46.5},
}
```
