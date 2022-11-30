library(testthat)
library(here)
source(here("code/R/functions/functions_standardize.R"))


test_that("Prepare Digikam", {
  prep <- prepare_digikam(dat_digikam)
  
  removed_cols <- colnames(dat_digikam)[!(colnames(dat_digikam) %in% colnames(prep))]
  expect_equal(removed_cols, c("X", "Directory", "FileName", "metadata_Behaviour")) 
  
  added_cols <- colnames(prep)[!(colnames(prep) %in% colnames(dat_digikam))]
  expect_equal(added_cols, c("file_path_1", "Moving", "Eating", "Standing", "Interacting", "Resting"))
  
  # Test recoding
  expect_equal(sort(unique(prep$Moving), na.last = TRUE), c(0, 1))
  expect_equal(sort(unique(prep$metadata_young_present), na.last = TRUE), 
               c(1, NA)) # Why NAs?
  
  # Test path
  expect_equal(file.path(dat_digikam$Directory, dat_digikam$FileName),
               prep$file_path_1)
})

test_that("Recode behaviors Digikam", {
  # Normal case
  b1 <- c("Eating_&_Resting", "Moving", "Eating_&_Interacting")
  b1_df <- data.frame(Eating = c(1,0,1),
                      Resting = c(1, 0, 0),
                      Moving = c(0, 1, 0),
                      Interacting = c(0, 0, 1))
  
  expect_equal(recode_behavior_digikam(b1), b1_df)
  
  # Inputs are NA
  b2 <- c(NA, NA, NA)
  expect_warning(recode_behavior_digikam(b2), "All behaviors are NA.")
  expect_equal(suppressWarnings(recode_behavior_digikam(b2)), data.frame())
  
  # Inputs are unexpected
  b3 <- c("Eating_&_Resting", "Plopping", "Eating_&_Interacting")
  b3_df <- data.frame(Eating = c(1,0,1),
                      Resting = c(1, 0, 0),
                      Plopping = c(0, 1, 0),
                      Interacting = c(0, 0, 1))
  
  expect_equal(suppressWarnings(recode_behavior_digikam(b3)), b3_df)
  expect_warning(recode_behavior_digikam(b3), "Unexpected behavior(s) detected: Plopping",
                 fixed = TRUE)
})

test_that("Prepare TrapTagger", {
  prep <- prepare_traptagger(dat_traptagger)
  
  # Real data case
  removed_cols <- colnames(dat_traptagger)[!(colnames(dat_traptagger) %in% colnames(prep))]
  expect_equal(removed_cols, "timestamp") 
  
  added_cols <- colnames(prep)[!(colnames(prep) %in% colnames(dat_traptagger))]
  expect_equal(added_cols, c("date", "time"))
  
  # Simulated data
  df <- data.frame(Capture_ID = c(1, 2, 3),
                   timestamp = c("2022-10-03 16:07:57", "2022-10-03 16:07:57", "2022-10-03 16:07:57"),
                   capture_label = c("foo", "bar", "baz"),
                   capture_url = c(1, 2, 3))
  test_caplab <- prepare_traptagger(df)
  expect_equal(test_caplab$capture_label, c("foo", "bar", "baz"))
  
  # Bad date-time
  df <- data.frame(Capture_ID = c(1, 2, 3),
                   timestamp = c("2022-10-03  16:07:57", "2022-10-0316:07:57", "2022-10-03 16:07:57 "),
                   capture_label = c("foo", "bar", "baz"),
                   capture_url = c(1, 2, 3))
  expect_warning(prepare_traptagger(df), "Some dates/times may be ill-parsed")
})

test_that("Keep only expected columns", {
  df <- data.frame(col1 = c(1, 2, 3), col2 = c(1, 0, 1),
                   col3 = c(1, 1, 1), colA = c("a", "b", "c"))
  expected <- c("col4", "col3", "col2", "col1")
  
  df_res <- df %>% mutate(col4 = NA) %>%
    select(c(col4, col3, col2, col1))
  
  df_test <- keep_only_expected_columns(df, 
                                        expected = expected, verbose = FALSE,
                                        approx = FALSE)
  expect_equal(df_test, df_res)
  expect_message(keep_only_expected_columns(df, expected = expected, 
                                            verbose = TRUE, approx = FALSE),
                 "Column(s) colA will be discarded.", fixed = TRUE)

})

test_that("Standardize zooniverse on real data",
          expect_equal(colnames(standardize_columns(dat_zooniverse, 
                                                    "zooniverse",
                                                    STANDARD)), 
                       goal_names))

test_that("Standardize digikam on real data",
          expect_equal(colnames(standardize_columns(dat_digikam, 
                                                    "digikam",
                                                    STANDARD)), 
                       goal_names))

test_that("Standardize traptagger on real data",
          expect_equal(colnames(standardize_columns(dat_traptagger, 
                                                    "traptagger",
                                                    STANDARD)), 
                       goal_names))

test_that("Standardize times", {
  expect_equal(standardize_time(c("07:00:00", "05:00:01", "22:00:04")),
               times(c("07:00:00", "05:00:01", "22:00:04")))
  expect_equal(standardize_time(c("7:00:00", "5:00:01", "22:00:04")),
               times(c("07:00:00", "05:00:01", "22:00:04")))
  expect_equal(standardize_time(c("7:00:00", "15:00:01", "22:00:04")),
               times(c("07:00:00", "15:00:01", "22:00:04")))
  expect_equal(standardize_time(c("7:00:00 AM", "3:00:01 PM", "10:00:04 PM")),
               times(c("07:00:00", "15:00:01", "22:00:04")))
  expect_equal(standardize_time(c("7:00:00 am", "3:00:01 pm", "10:00:04 pm")),
               times(c("07:00:00", "15:00:01", "22:00:04")))
  expect_error(standardize_time(c("7:00:00", "15:00", "22:00:04")),
               "Bad time formatting")
  expect_error(standardize_time(c("7:00:a", "15:00:00 ", "22:K:04")),
               "Could not cast times 7:00:a, 22:K:04 to numeric.")
  
})

test_that("Standardize dates", {
  dat <- c("2020/12/21", "2020/11/12", "2020/01/11")
  expect_equal(standardize_date(dat), as_date(c("2020-12-21", "2020-11-12", "2020-01-11")))
  
  dat <- c("12/21/2020", "11/12/2020", "01/11/2020")
  expect_equal(standardize_date(dat), as_date(c("2020-12-21", "2020-11-12", "2020-01-11")))
  
  dat <- c("21/12/2020", "12/11/2020", "11/01/2020")
  expect_equal(standardize_date(dat), as_date(c("2020-12-21", "2020-11-12", "2020-01-11")))
  
})

test_that("Fill capture info digikam", {
  df_test <- data.frame(id = rep(NA, 3),
                        locationID = rep("RES", 3),
                        cameraID = c("A01", "A02", "A02"),
                        season = rep(NA, 3),
                        roll = rep(1, 3),
                        captureID = rep(NA, 3),
                        eventDate = as.Date("2022-10-04", "2022-10-04", "2022-10-04"),
                        eventTime = times(c("00:00:00", "00:00:00", "00:00:01")),
                        filePath1 = c("foo/bar/baz_Roll1/foo/bar1.JPG",
                                        "foo/bar/baz_Roll1/foo/bar2.JPG",
                                        "foo/bar/baz_Roll1/foo/bar3.JPG"))
  
  df_fill <- fill_capture_info_digikam(df_test)
  
  expect_equal(df_fill$season, 
               rep(NA, 3))
  expect_equal(df_fill$captureID, 
               c("1", "1", "2"))
  expect_equal(df_fill$roll,
               rep("1", 3))
})

test_that("Fill capture info traptagger", {
  df_test <- data.frame(eventID = c("RES1#1#A01",
                                    "RES1#1#A02",
                                    "RES2#1#A02"),
                        season = rep(NA, 3),
                        cameraID = rep(NA, 3),
                        roll = rep(NA, 3),
                        captureID = rep(NA, 3),
                        locationID = rep(NA, 3))
  
  df_fill <- fill_capture_info_traptagger(df_test)
  
  expect_equal(df_fill$season, 
               rep(NA, 3))
  expect_equal(df_fill$captureID, 
               c("1", "1", "2"))
  expect_equal(df_fill$roll,
               rep("1", 3))
})

test_that("Change cameraID works well", {
  locationID <- c("MAD", "MAF", "MAD")
  
  # --- Not present (all)
  camera <- c("A01", "A01", "B01")
  cameraID <- get_cameraID(locationID, camera)
  expect_equal(cameraID, c("MAD_A01", "MAF_A01", "MAD_B01"))
  
  # --- Already present (all)
  camera <- c("MAD_A01", "MAF_A01", "MAD_B01")
  cameraID <- suppressMessages(get_cameraID(locationID, camera))
  expect_equal(cameraID, c("MAD_A01", "MAF_A01", "MAD_B01"))
  expect_message(get_cameraID(locationID, camera),
                 "Cameras MAD_A01, MAF_A01, MAD_B01 already begin with code_loc: not adding the location.")
  
  # --- Mix
  camera <- c("A01", "MAF_A01", "MAD_B01")
  cameraID <- suppressMessages(get_cameraID(locationID, camera))
  expect_equal(cameraID, c("MAD_A01", "MAF_A01", "MAD_B01"))
  expect_message(get_cameraID(locationID, camera),
                 "Cameras MAF_A01, MAD_B01 already begin with code_loc: not adding the location.")
})
