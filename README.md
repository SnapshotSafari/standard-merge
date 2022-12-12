# standardizeSnapshot
This is a R package to standardize data across multiple classifying platforms (Zooniverse, TrapTagger or Digikam) and merge for complete datasets.

##  /!\ this is a work in progress /!\ 

## Purpose

Snapshot Safari data comes in different standards, following the method that was used to classify pictures (Zooniverse, TrapTagger or Digikam). The codes in this folder allow to standardize all data sources to a unique file format.


## Installation 

For now, you can install the package by downloading the source code in zip format. Then, in R use:
devtools::install_local("standard-merge-main.zip")

Once the repository will be public: it will be possible to install it via:
devtools::install_github("https://github.com/SnapshotSafari/standard-merge")


## Contents

### Codes

The R/ folder contains all the package functions.

### Tests

Tests were run with the R package testthat. These tests can be found in tests/.

### Documentation

standardizeSnapshot.pdf contains the documentation for the package's functions. The .Rd files generated with Roxygen2 can also be found in man/.




