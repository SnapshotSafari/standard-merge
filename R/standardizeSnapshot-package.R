#' standardizeSnapshot is a R package to standardize camera trap records files from the Snapshot Safari project.
#'
#' Snapshot Safari data comes in different standards, 
#' following the method that was used to classify pictures 
#' (Zooniverse, TrapTagger or Digikam). This package allows to standardize 
#' all data sources to a unique file format and then cleans the files to 
#' homogenize records.
#' See Snapshot Safari: a large-scale collaborative to monitor Africa’s remarkable biodiversity (Pardo et al, 2021)
#' (https://www.sajs.co.za/article/view/8134)
#'
#' @docType package
#' @name PEcAn.standardizeSnapshot
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate rename arrange group_by
#' @importFrom stringr str_split str_extract str_match
#' @importFrom tidyselect all_of
#' @importFrom utils read.csv write.csv head
NULL