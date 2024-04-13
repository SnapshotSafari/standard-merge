# standardizeSnapshot

Snapshot Safari data comes in different standards, following the method that was used to classify pictures (Zooniverse, TrapTagger or Digikam). This R package allows to standardize all data sources to a unique file format.

**Warning: all outputs have not been thoroughly checked (in particular for the camera IDs).**

## Installation

You can install this package using the following commands in the R console (*NB: you need an internet connection*):

``` r
install.packages("devtools") # if devtools is not installed already
library(devtools)
devtools::install_github("https://github.com/SnapshotSafari/standard-merge",
                         build_vignettes = TRUE)
```

## Documentation

The [package website](https://snapshotsafari.github.io/standard-merge/) lists the documentation of all functions under the [reference](file:///home/lnicvert/Documents/PhD/Code/standardizeSnapshot/docs/reference/index.html) section. 

Two articles on the website also show the typical workflow for the package to standardize [one](file:///home/lnicvert/Documents/PhD/Code/standardizeSnapshot/docs/articles/standardize-file.html) or [several](file:///home/lnicvert/Documents/PhD/Code/standardizeSnapshot/docs/articles/standardize-files.html) file(s).

*NB: you can also read functions documentation in the file `standardizeSnapshot_xx.pdf` at the root of this directory and view the articles in R with `vignette("standardize-file")` or `vignette("standardize-files")`.*

## Contents

The `R/` folder contains all the package functions.

Unitary tests were run with the R package `testthat` and can be found in `tests/`. Additional tests run with files installed locally can also be found in `scripts_tests/`.
