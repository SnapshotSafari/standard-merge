# standardizeSnapshot
This is a R package to standardize data across multiple classifying platforms (Zooniverse, TrapTagger or Digikam) and merge for complete datasets.

##  /!\ this is a work in progress /!\ 

## Purpose

Snapshot Safari data comes in different standards, following the method that was used to classify pictures (Zooniverse, TrapTagger or Digikam). The codes in this folder allow to standardize all data sources to a unique file format.


## Installation 

For now, you can install the package by downloading the source code in zip format. Then, in R use:
```r 
devtools::install_local("standard-merge-main.zip")
```

Once the repository will be public: it will be possible to install it via:
```r 
devtools::install_github("https://github.com/SnapshotSafari/standard-merge")
```
## Vignettes

There is one vignette showing the typical workflow for the package.

The vignette `standardize-file` shows the workflow to standardize one single file:
```r 
vignette("standardize-file")
```


## Contents

### Codes

The R/ folder contains all the package functions.

### Tests

Tests were run with the R package testthat. These tests can be found in tests/.

### Documentation

standardizeSnapshot.pdf contains the documentation for the package's functions. The .Rd files generated with Roxygen2 can also be found in man/.




