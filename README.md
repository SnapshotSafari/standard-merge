# standard-merge
Code to standardize data across multiple classifying platforms (Zooniverse, TrapTagger or Digikam) and merge for complete datasets.

##  /!\ this code is a work in progress /!\ 

## Purpose

Snapshot Safari data comes in different standards, following the method that was used to classify pictures (Zooniverse, TrapTagger or Digikam). The codes in this folder allow to standardize all data sources to a unique file format.

## Contents

### Codes

The code/R folder contains all codes to standardize data.

+ code/R/functions contains useful functions.
+ code/R/01_standardize.R is the standardizing script. Change the input data and the destination folder to standardize data.
+ code/R/02_merge_files.R allows to merge all files into a big file.

### Tests

Tests were run with the R package testthat. These tests can be found in code/tests/.

### Snapshot standard

The standard column names used for the Shapshot data, and the corresponding older names, can be found in docs/. There are 2 files:

+ docs/standard.ods which is our working document (Spreadsheet)
+ docs/standard.csv which is the file used in the standardizing code to standardize column names.


