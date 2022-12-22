library(testthat)

test_that("Read csv", {
          f <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/APN/"

          csv_goal <- c(paste0(f, "APN_S1_full_report_0-50__agreement_corrected_fin.csv"),
                        paste0(f, "APN_S2_full_report_0-50__agreement_corrected_fin.csv"),
                        paste0(f, "APN_S3_full_report_0-50__agreement_corrected_fin.csv"))
          expect_equal(suppressWarnings(list_csv_in_folder(f)), csv_goal)
          
          w <- paste0("The following file(s) will be ignored (they are not csv):\n", 
                      paste0("\t", f, "APN_S3_full_report_0-50__agreement_corrected_fin.xlsx"))
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

test_that("Write standardized df", {
  data(zooniverse)
  zooniverse_std <- standardize_snapshot_df(zooniverse,
                                            standard,
                                            verbose = FALSE)
  
  to <- tempdir()
  path <- write_standardized_df(df = zooniverse_std,
                                to = to,
                                write = FALSE,
                                verbose = FALSE,
                                return_path = TRUE)
  expect_equal(path, file.path(to, "APN_S1_R1-2.csv"))
  
  expected_message <- paste0("Writing file ", to, "/APN_S1_R1-2.csv")
  expect_message(write_standardized_df(df = zooniverse_std,
                                       to = to,
                                       write = TRUE,
                                       verbose = TRUE,
                                       return_path = FALSE),
                 expected_message)
  expect_true(file.exists(file.path(to, "APN_S1_R1-2.csv")))
})

test_that("Write standardized list", {
  data(zooniverse)
  data(digikam)
  
  dat_list <- list("APN/zooniverse.csv" = zooniverse,
                   "roaming/MOK.csv" = digikam)
  
  list_std <- standardize_snapshot_list(dat_list,
                                        standard,
                                        verbose = FALSE)
  
  to <- tempdir()
  path <- write_standardized_list(df_list = list_std, 
                                  to = to,
                                  write = FALSE,
                                  verbose = FALSE,
                                  return_path = TRUE)
  expect_equal(path, c(file.path(to, "APN/APN_S1_R1-2.csv"),
                       file.path(to, "roaming/MOK_SNA_R1.csv")))
  
  messages <- c(paste0("Creating folder ", to, "/APN"),
                "Writing file APN/zooniverse.csv -> APN/APN_S1_R1-2.csv (1/2) ---",
                # paste0("Creating folder ", to, "/roaming"), 
                # Not tested because creates during the first run 
                # and can't find a way to test 2 messages...
                "Writing file roaming/MOK.csv -> roaming/MOK_SNA_R1.csv (2/2) ---")
  expect_message(write_standardized_list(df_list = list_std,
                                         to = to,
                                         write = TRUE,
                                         verbose = TRUE,
                                         return_path = FALSE),
                 messages[1], fixed = TRUE)
  expect_message(write_standardized_list(df_list = list_std,
                                         to = to,
                                         write = TRUE,
                                         verbose = TRUE,
                                         return_path = FALSE),
                 messages[2], fixed = TRUE)
  expect_message(write_standardized_list(df_list = list_std,
                                         to = to,
                                         write = TRUE,
                                         verbose = TRUE,
                                         return_path = FALSE),
                 messages[3], fixed = TRUE)
  expect_true(file.exists(file.path(to, "APN/APN_S1_R1-2.csv")))
  expect_true(file.exists(file.path(to, "roaming/MOK_SNA_R1.csv")))
})