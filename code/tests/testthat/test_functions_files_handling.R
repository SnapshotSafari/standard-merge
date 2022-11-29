library(testthat)
library(here)
source(here("code/R/functions/functions_files_handling.R"))

test_that("Read csv", {
          f <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/APN/"

          csv_goal <- c("APN_S1_full_report_0-50__agreement_corrected_fin.csv",
                        "APN_S2_full_report_0-50__agreement_corrected_fin.csv",
                        "APN_S3_full_report_0-50__agreement_corrected_fin.csv")
          expect_equal(suppressWarnings(list_csv_in_folder(f)), csv_goal)
          
          w <- paste0("File(s) ", 
                      "APN_S3_full_report_0-50__agreement_corrected_fin.xlsx",
                      " will be ignored (they are not csv).")
          expect_message(list_csv_in_folder(f), w, fixed = TRUE)
})

test_that("Get files and folder (folder input)", {
  f <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH/"
  res <- get_files_and_folder(f)
  
  folders <- rep("/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH/", 4)
  files <- list_csv_in_folder(folder)
  expected <- data.frame(folders, files)
  
  expect_equal(res, expected)
})

test_that("Get files and folder (fle input)", {
  f <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH/ATH_Roll1_Snapshot_UPDATED.csv"
  res <- get_files_and_folder(f)
  
  folders <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH"
  files <- "ATH_Roll1_Snapshot_UPDATED.csv"
  expected <- data.frame(folders, files)
  
  expect_equal(res, expected)
})
