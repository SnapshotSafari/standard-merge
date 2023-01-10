# standardizeSnapshot
This is a R package to standardize data across multiple classifying platforms (Zooniverse, TrapTagger or Digikam) and merge for complete datasets.

##  /!\ this is a work in progress /!\ 

**While the main functions are working, all outputs have not been thouroughly ckecked out (in particular for the camera IDs).**


## Purpose

Snapshot Safari data comes in different standards, following the method that was used to classify pictures (Zooniverse, TrapTagger or Digikam). The codes in this folder allow to standardize all data sources to a unique file format.


## Installation 

You can install this package using the following commands in the R console (NB: you need an internet connection)

```r 
install.packages("devtools") # if devtools is not installed already
library(devtools)
devtools::install_github("https://github.com/SnapshotSafari/standard-merge")
```

## Vignettes

There are 2 vignette showing the typical workflow for the package.

The vignette `standardize-file` shows the workflow to standardize one single file. To view the vignette, launch the following command in the R console:
```r 
vignette("standardize-file")
```

The vignette `standardize-files` shows the workflow to standardize multiple files:
```r 
vignette("standardize-files")
```

## Contents

### Codes

The `R/` folder contains all the package functions.

### Tests

Unitari tests were run with the R package `testthat`. These tests can be found in `tests/`.
Diverse tests run with files installed locally can also be found in `scripts_tests/`.

### Documentation

`standardizeSnapshot_xx.pdf` contains the package documentation. The .Rd files generated with `Roxygen2` can also be found in `man/`.




