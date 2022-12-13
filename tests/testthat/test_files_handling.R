library(testthat)

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
  # --- One folder
  f <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH/"
  basepath <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data"
  res <- get_csv_files_and_folders(f, 
                                   basepath = basepath)
  
  folders <- rep(basepath, 4)
  files <- c("ATH/ATH_Roll1_Snapshot.csv",
             "ATH/ATH_Roll2_Snapshot.csv",
             "ATH/ATH_Roll3_Snapshot.csv",
             "ATH/ATH_Roll4_Snapshot.csv")
  expected <- data.frame(folders, files)
  
  expect_equal(res, expected)
  
  # --- Two folders
  f2 <- c("/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH/",
          "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/GON")
  basepath <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data"
  res2 <- get_csv_files_and_folders(f2, 
                                    basepath = basepath)
  
  folders2 <- rep(basepath, 5)
  files2 <- c("ATH/ATH_Roll1_Snapshot.csv",
              "ATH/ATH_Roll2_Snapshot.csv",
              "ATH/ATH_Roll3_Snapshot.csv",
              "ATH/ATH_Roll4_Snapshot.csv",
              "GON/GON_S1_full_report_0-50__agreement_corrected_fin.csv")
  expected2 <- data.frame(folders2, files2)
  colnames(expected2) <- c("folders", "files")
  
  expect_equal(res2, expected2)
})

test_that("Get files and folder (file input)", {
  # -- One file
  f <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH/ATH_Roll1_Snapshot.csv"
  basepath <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data"
  res <- get_csv_files_and_folders(f,
                                   basepath = basepath)
  
  folders <- basepath
  files <- "ATH/ATH_Roll1_Snapshot.csv"
  expected <- data.frame(folders, files)
  
  expect_equal(res, expected)
  
  # -- 2 files
  f2 <- c("/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH/ATH_Roll1_Snapshot.csv",
          "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH/ATH_Roll2_Snapshot.csv")
  
  res2 <- get_csv_files_and_folders(f2,
                                    basepath = basepath)
  
  folders2 <- rep(basepath, 2)
  files2 <- c("ATH/ATH_Roll1_Snapshot.csv",
              "ATH/ATH_Roll2_Snapshot.csv")
  expected2 <- data.frame(folders2, files2)
  colnames(expected2) <- c("folders", "files")
  
  expect_equal(res2, expected2)
})
