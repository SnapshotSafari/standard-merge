# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-09
#
# Script Description: functions to standardize Snapshot files.


# Libraries ---------------------------------------------------------------
# library(lubridate)
# library(stringr)
# library(chron)


# Prepare data ------------------------------------------------------------

#' Guess classifier
#' 
#' Guesses the classifier used to annotate the data based on the column names
#' given in `colnames_df`.
#'
#' @param colnames_df A character vector of column names.
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return The classifier: either `zooniverse`, `digikam` or `traptagger`.
#' 
#' @export
#' @examples
#' guess_classifier(colnames(zooniverse))
#' guess_classifier(colnames(traptagger))
#' guess_classifier(colnames(digikam))
guess_classifier <- function(colnames_df, logger = NA) {
  
  if ("question__species" %in% colnames_df) {
    classifier <- "zooniverse"
  } else if ("HierarchicalSubject" %in% colnames_df) {
    classifier <- "digikam"
  } else if ("timestamp" %in% colnames_df) {
    classifier <- "traptagger"
  } else {
    msg <- "Could not set classifier from column names: column names do not contain 'question__species', 'HierarchicalSubject' or 'timestamp'\n"
    write_log_message(msg, logger = logger, level = "error")
    stop(msg)
  }
  return(classifier)
}



# Standardize -------------------------------------------------------------


#' Standardize a list of dataframes
#'
#' Standardizes a list of dataframes to the Snapshot standard.
#'
#' @param df_list a list of dataframes.
#' @param standard_df The standard dataframe to match column names to the new standard.
#' A dataframe with >= 2 columns, one of which must be named `zooniverse`, `digikam` or `traptagger` 
#' and another one must be named `new`.
#' @param classifier Optional character or vector of characters for the classifier.
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' @param verbose Should a lot of details be given when executing function?
#' 
#' @return The list of standardized dataframes: they the same columns as specified in
#' `standard_df$new`, dates and times are standardized and some columns regarding
#' information on the capture are filled. Species names (`snapshotName`) and `cameraID` and `locationID`
#' are standardized (see details).
#' Columns are in the same order as provided in `standard_df$new`
#' and the rows are ordered by ascending camera, date and time.
#' 
#' @details 
#' Dates and times are standardized to `YYYY-MM-DD` and `HH:MM:SS`.
#' The columns `locationID`, `cameraID`, `roll`, `capture` 
#' and `season` (if classifier is `zooniverse`) are filled with meaningful information. 
#' Species names (`snapshotName`) are standardized to match the existing, known species names.
#' For column `locationID`:
#' 
#' + The location code `DHP` is replaced with `OVE` if the corresponding camera code starts with 'O'.
#' + The location code `KGA` is replaced with `KHO` if the corresponding camera code starts with 'KHO'.
#' 
#' For column `cameraID`:
#' 
#' + For TrapTagger data: will remove the leading location code part for all data 
#' (eg if `location` is `ATH`, will change `cameras` `ATH_A01` -> `A01`).
#' Also, if the location code is `KHO`, `SAM` or `TSW`: will remove the dash in 
#' the camera name (e.g `KHO_E_A01` -> `EA01`)
#' 
#' + For Zooniverse data: if the location code is `KHO`, will replace 
#' `KHOG` with `E` and  `KHOL` with `M` in `cameras`.
#' If the location code is `DHP`, will remove leading `D` in `cameraID`.
#' If the location code is `OVE`, will remove leading `O` in `cameraID`.
#' 
#' + For column `eventID`: the event ID formatted as season#cam_site#roll#event_no.
#' 
#' @export
#' 
#' @note If the file name is `Marinmane NR _record_table_0min_deltaT_2021-06-10.csv`, will
#' apply the location code `MAR`.
#' 
#' @examples 
#' df_list <- list(zooniverse, digikam, traptagger)
#' 
#' # Digikam data must be named with at least the locationID code
#' names(df_list)[2] <- "MOK" 
#' standardize_snapshot_list(df_list, standard)
#' 
#' # The name for Digikam data can also a filename starting with the locationID code
#' names(df_list)[2] <- "MOK_record_table_0min_deltaT_2021-05-07.csv"
#' standardize_snapshot_list(df_list, standard)
standardize_snapshot_list <- function(df_list, standard_df,
                                      classifier,
                                      logger = NA,
                                      verbose = TRUE) {
  
  # --- Check arguments
  if(!inherits(df_list, "list")) {
    msg <- paste("df_list must be a list, you provided an object of class", 
                 paste(class(df_list), collapse = ", "))
    write_log_message(msg, logger = logger, level = "error")
    stop(msg)
  }
  
  if(!missing(classifier)) {
    if((length(classifier)) != 1 | (length(classifier) != length(df_list))) {
      msg <- paste("classifier must be of length 1 or", length(df_list),
                   "it is of length", length(classifier))
      write_log_message(msg, logger = logger, level = "error")
      stop(msg)
    }
  }
  
  # --- Initialize empty list
  std_list <- vector(mode = "list", length = length(df_list))
  names(std_list) <- names(df_list)
  
  # --- Iterate through list
  for (i in 1:length(df_list)) {
    
    # --- Message
    if(!is.null(names(df_list)[i])) {
      iter <- names(df_list)[i]
    } else {
      iter <- i
    }
    msg <- paste("Standardizing file", iter, "---")
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
    
    df_i <- df_list[[i]]
    
    # Check object
    if(!inherits(df_i, "data.frame")) {
      msg <- paste0(deparse(quote(df_list)),"[[", i, "]] ", 
                    "must be a dataframe.")
      write_log_message(msg, logger = logger, level = "error")
      stop(msg)
    }
    
    # --- Get classifier
    if(missing(classifier)) {
      classifier_i <- guess_classifier(colnames(df_i), logger = logger)
    } else {
      if (length(classifier) != 1) {
        classifier_i <- classifier[i]
      } else {
        classifier_i <- classifier
      }
    }
    
    # --- If classifier is Digikam: get locationID
    if(classifier_i == "digikam") {
      if(!is.null(names(df_list)[i])) {
        df_name <- names(df_list)[i]
        locationID <- locationID_from_filename(df_name, logger = logger)
        
        std_dat_i <- standardize_snapshot_df(df_i, 
                                             standard_df = standard_df,
                                             locationID_digikam = locationID,
                                             classifier = classifier_i,
                                             logger = logger,
                                             verbose = verbose)
      } else {
        msg <- paste0(deparse(quote(df_list)),"[", i, "] ", 
                      "is not named and it is Digikam: provide a named list if you want to fill locationID.")
        write_log_message(msg, logger = logger, level = "info")
        message(msg)
        # --- Standardize (Digikam with no locationID)
        std_dat_i <- standardize_snapshot_df(df_i, 
                                             standard_df = standard_df,
                                             classifier = classifier_i,
                                             logger = logger,
                                             verbose = verbose)
      }
    } else {
      # --- Standardize (not Digikam)
      std_dat_i <- standardize_snapshot_df(df_i, 
                                           standard_df = standard_df,
                                           classifier = classifier_i,
                                           logger = logger,
                                           verbose = verbose)
    }
    
    # --- Add df to list
    std_list[[i]] <- std_dat_i
    
  }
  return(std_list)
}

#' Standardize dataframe
#'
#' Standardizes a dataframe to the Snapshot standard.
#' 
#' @param df The dataframe to standardize. It is expected to match the data format for either
#' Zooniverse, TrapTagger or Digikam processed data (i.e. have column names defined in standard_df).
#' @param standard_df The standard dataframe to match column names to the new standard.
#' A dataframe with >= 2 columns, one of which must be named `zooniverse`, `digikam` or `traptagger` 
#' and another one must be named `new`.
#' @param locationID_digikam Optional character `locationID` to use for Digikam data
#' (will display a warning if not provided for Digikam data.) Indeed, for Digikam data,
#' the `locationID` cannot be inferred from other columns.
#' @param classifier Optional character for the classifier.
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' @param verbose Should a lot of details be given when executing function?
#' 
#' @return The standardized dataframe: it has the same columns as specified in
#' `standard_df$new`, dates and times are standardized and some columns regarding
#' information on the capture are filled. Species names (`snapshotName`) and `cameraID` and `locationID`
#' are standardized (see details).
#' Columns are in the same order as provided in `standard_df$new`
#' and the rows are ordered by ascending camera, date and time.
#' 
#' @details 
#' Dates and times are standardized to `YYYY-MM-DD` and `HH:MM:SS`.
#' The columns `locationID`, `cameraID`, `roll`, `capture` 
#' and `season` (if classifier is `zooniverse`) are filled with meaningful information. 
#' Species names (`snapshotName`) are standardized to match the existing, known species names.
#' For column `locationID`:
#' 
#' + The location code `DHP` is replaced with `OVE` if the corresponding camera code starts with 'O'.
#' + The location code `KGA` is replaced with `KHO` if the corresponding camera code starts with 'KHO'.
#' 
#' For column `cameraID`:
#' 
#' + For TrapTagger data: will remove the leading location code part for all data 
#' (eg if `location` is `ATH`, will change `cameras` `ATH_A01` -> `A01`).
#' Also, if the location code is `KHO`, `SAM` or `TSW`: will remove the dash in 
#' the camera name (e.g `KHO_E_A01` -> `EA01`)
#' 
#' + For Zooniverse data: if the location code is `KHO`, will replace 
#' `KHOG` with `E` and  `KHOL` with `M` in `cameras`.
#' If the location code is `DHP`, will remove leading `D` in `cameraID`.
#' If the location code is `OVE`, will remove leading `O` in `cameraID`.
#' 
#' + For column `eventID`: the event ID formatted as season#cam_site#roll#event_no.
#' 
#' @export
#' 
#' @examples
#' standardize_snapshot_df(zooniverse, standard)
#' standardize_snapshot_df(traptagger, standard)
#' standardize_snapshot_df(digikam, standard, locationID_digikam = "MOK")
standardize_snapshot_df <- function(df, standard_df,
                                    locationID_digikam, classifier,
                                    logger = NA,
                                    verbose = TRUE) {
  
  ncol_init <- ncol(df)
  nrow_init <- nrow(df)
  if (verbose) {
    msg <- paste("Initial file:", ncol_init, "columns, ", 
                 nrow_init, "rows.")
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
  }
  if(missing(classifier)) {
    # Guess classifier
    colnames_df <- colnames(df)
    classifier <- guess_classifier(colnames_df, logger = logger)
  }
  
  # Standardize columns
  if (verbose) {
    msg <- "Standardizing columns"
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
  }
  std_dat <- standardize_columns(df, 
                                 classifier = classifier,
                                 standard_colnames = standard_df,
                                 logger = logger)
  
  # Standardize date/times 
  if (verbose) {
    msg <- "Standardizing dates/times"
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
  }
  std_dat <- std_dat %>% mutate(eventDate = standardize_date(eventDate,
                                                             logger = logger))
  std_dat <- std_dat %>% mutate(eventTime = standardize_time(eventTime,
                                                             logger = logger))
  
  # Get location code (Digikam)
  if(classifier == "digikam") { # Get locationID from filename
    if (verbose) {
      msg <- "Getting location code for Digikam data"
      write_log_message(msg, logger = logger, level = "info")
      message(msg)
    }
    if(missing(locationID_digikam)) {
      msg <- "Digikam data requires 'locationID_digikam' to be provided: without a it, locationID will be set to NA in the standardized data."
      write_log_message(msg, logger = logger, level = "warn")
      warning(msg)
      std_dat$locationID <- NA
    } else {
      # Add location code
      std_dat$locationID <- locationID_digikam
    }
  }
  
  # Fill capture info
  if (verbose) {
    msg <- "Fill capture info"
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
  }
  std_dat <- fill_capture_info(std_dat, classifier, logger = logger)
  
  # Add classifier 
  std_dat$classifier <- classifier
  
  # Reorder columns (to be sure)
  to_reorder <- standard_df$new[!is.na(standard_df$new)]
  std_dat <- std_dat %>% select(all_of(to_reorder))
  
  # Clean location/camera
  if (verbose) {
    msg <- "Cleaning location, camera and species"
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
  }
  
  if(any(is.na(std_dat$locationID))) {
    msg <- "NA present in locationID: will not clean locationID."
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
    std_dat <- clean_camera_location(std_dat, 
                                     location = FALSE,
                                     logger = logger)
  } else {
    std_dat <- clean_camera_location(std_dat,
                                     logger = logger)
  }
  
  # Clean species
  std_dat <- std_dat %>% 
    mutate(snapshotName = clean_species(snapshotName))
  
  # Reorder data (rows)
  std_dat <- std_dat %>% arrange(cameraID, eventDate, eventTime)
  
  # Remove capture column
  # std_dat <- std_dat %>% select(-capture)
  
  # Remove blanks
  std_dat <- clean_blanks(std_dat)
  
  # Check output
  ncol_fin <- ncol(std_dat)
  nrow_fin <- nrow(std_dat)
  
  if (nrow_init != nrow_fin) {
    msg <- paste("Different number of rows: initial data has", nrow_init, "rows and final data has",
                 nrow_fin, "rows.")
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  if ( has_empty_values(std_dat$locationID) ) {
    msg <- paste("Empty values in locationID.")
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  if ( has_empty_values(std_dat$cameraID) ) {
    msg <- paste("Empty values in cameraID")
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  if ( has_empty_values(std_dat$snapshotName) ) {
    msg <- paste("Empty values in snapshotName")
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  if ( has_empty_values(std_dat$eventDate) ) {
    msg <- paste("Empty values in eventDate")
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  if ( has_empty_values(std_dat$eventTime) ) {
    msg <- paste("Empty values in eventTime")
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  if (verbose) {
    msg <- paste("Final file:", ncol_fin, "columns, ", nrow_fin, "rows. Here is a sneak peek:")
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
    
    # Get first 8 columns and 5 rows
    msg <- head(std_dat[,1:8], 5)
    
    if (!all(is.na(logger))) {
      file_con <- file(logger$logfile, open = "a")
      write.table(msg, file = file_con,
                  row.names = FALSE,
                  quote = FALSE,
                  sep = "\t")
      writeLines("", file_con)
      close(file_con)
    }
    message(df_for_message(msg))
  }
  
  return(std_dat)
}


# Helpers -----------------------------------------------------------------

## Prepare data -------------------------------
#' Prepare TrapTagger data
#'
#' Prepares data processed with TrapTagger to be standardized by 
#' creating columns date and time from timestamp and formatting
#' capture_label_xx into a unique column.
#' 
#' @param df The dataframe to standardize (must have columns starting with capture_label
#' and column timestamp)
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return Returns the dataframe with 2 additional columns date and time, minus the timestamp column, and with merged capture_label
#' @noRd
#'
prepare_traptagger <- function(df, logger = NA){
  
  # --- Copy data
  df_res <- df
  
  # --- Split dates/times
  date_time <- strsplit(df_res$timestamp, " ")
  
  unique_length <- unique(sapply(date_time, length))
  
  if(unique_length != 2) {
    msg <- "Some dates/times may be ill-parsed"
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  df_res$date <- sapply(date_time, function(l) l[1])
  df_res$time <- sapply(date_time, function(l) l[2])
  
  # --- Delete intermediate columns
  df_res <- df_res %>% select(-timestamp)
  
  return(df_res)
}

#' Prepare Digikam data
#'
#' Standardizes Digikam data to be ready to be standardized: removes 'X' index column,
#' (if it exists), merges 'Directory' and 'Filename' to create a path, 
#' splits 'metadata_Behaviour' column into the relevant behaviors and 
#' recodes 'metadata_young_present' into 0 and 1.
#' 
#' @param df Dataframe to standardize (must have columns 'metadata_Behaviour' and 'metadata_young_present').
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return Returns the dataframe without 'X' column, 'Directory' and 'Filename' merged
#' into 'file_path_1' column, 'metadata_Behaviour' splitted into up to 6 columns
#' 'Eating', 'Drinking', 'Resting', 'Standing', 'Moving', 'Interacting' (depending on the
#' behavoirs that were present in the data) and the 'metadata_young_present' column recoded
#' so that 'Yes'/'No' becomes 0/1.
#' 
#' @noRd
prepare_digikam <- function(df, logger = NA){
  
  df_res <- df
  
  # --- Remove index column
  if ("X" %in% colnames(df_res)) {
    df_res <- df_res %>% select(-X) 
  }
  
  # --- Create file path from 2 columns
  if("Directory" %in% colnames(df_res) &  "FileName" %in% colnames(df_res)){
    df_res$file_path_1 <- file.path(df_res$Directory, df_res$FileName)
    df_res <- df_res %>% select(-c(Directory, FileName)) # Remove merged columns
  }else{
    msg <- "Could not create column file_path_1: missing columns 'Directory' and 'FileName'"
    write_log_message(msg, logger = logger, "warn")
    warning(msg)
  }
  
  # --- Format behaviors
  behaviors <- recode_behavior_digikam(df_res$metadata_Behaviour,
                                       logger = logger) 
  
  # Add rows to df_res
  df_res <- df_res %>% select(-metadata_Behaviour)
  
  if(nrow(behaviors) != 0) { # If not all behaviors are NA
    df_res <- cbind(df_res, behaviors)
  }
  
  
  # --- Recode young present column
  df_res <- df_res %>%
    mutate(metadata_young_present = ifelse(metadata_young_present == "Yes", 
                                           1, 0))
  df_res$metadata_young_present <- as.numeric(df_res$metadata_young_present)
  
  return(df_res)
}

#' Recode a behavior column
#'
#' @param behaviors a character vector of behaviors, expected to be separated by a '_&_'
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return A dataframe with one column per behavior initially present, with
#' 1 if the behavior was present and 0 otherwise.
#' 
#' @noRd
recode_behavior_digikam <- function(behaviors, logger = NA) {
  
  # Split according to separator
  res <- str_split(behaviors, pattern = "_&_")
  
  # Get all behaviors present
  new_behaviors <- unique(unlist(res))
  new_behaviors <- new_behaviors[!is.na(new_behaviors)]
  
  expected_behaviors <- c("Standing", "Resting", "Moving", "Eating", "Interacting", "Drinking")
  
  if(length(new_behaviors) != 0) { # If it's not all NA
    if (!all(new_behaviors %in% expected_behaviors) ) {
      unexpected <- unique(new_behaviors[!(new_behaviors %in% expected_behaviors)])
      msg <- paste("Unexpected behavior(s) detected:", 
                   paste(unexpected, collapse = ", "))
      write_log_message(msg, logger = logger, level = "warn")
      warning(msg)
    }
  } else {
    msg <- "All behaviors are NA."
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
    return(data.frame())
  }
  
  # Helper function to recode behaviors in lapply
  # behavior is a list of character vector of behaviors
  # (which are the splitted version of a row) 
  # (it corresponds to one row of the original dataframe)
  # new_behaviors is a character vector of new behaviors names.
  recode_behaviors <- function(behavior, new_behaviors){
    # Copy vector
    res <- behavior
    
    # Name vector as its content
    names(res) <- res
    
    # Reorder according to 'new_behavior' vector
    res <- res[new_behaviors]
    
    # Rename to replace NA names
    names(res) <- names(new_behaviors)
    
    # Recode as 0/1
    res[!is.na(res)] <- 1
    res[is.na(res)] <- 0
    res <- as.numeric(res)
    
    return(res)
  }
  
  # Recode behaviors in the list
  res <- lapply(res, 
                recode_behaviors, new_behaviors = new_behaviors)
  
  # Transform list to df
  res <- do.call(rbind, res)
  res <- as.data.frame(res)
  colnames(res) <- new_behaviors
  
  return(res)
}

## Standardize columns ----------------------------------
#' Standardize data
#'
#' Given a dataframe, standardizes column names to match a standard.
#' It removes all original columns not in the standard and adds missing columns
#' (which are NA)
#' 
#' @param df The dataframe to standardize
#' @param classifier The classifier used to generate the data: possible values are 'zooniverse', 'traptagger', 'digikam'
#' @param verbose Should the info output messages be displayed?
#' @param standard_colnames A dataframe with 2 columns (at least) named like the classifier
#' and 'new'.
#' The column named like the classifier contains column names 
#' that are expected in the initial file. These names will be matched in the column names of df using partial matching (case insensitive and
#' removing blanks).
#' The column 'new' contains the column names that are expected in the final file. 
#' Columns in the classifier column will be renamed following the name of the 
#' corresponding value in 'new'. If no old column corresponds to 'new' (indicated with a NA)
#' then the column will be created and filled with NAs.
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return A dataframe with 25 columns with the standardized names
#'
#' @noRd
standardize_columns <- function(df, 
                                classifier = c("zooniverse", "traptagger", "digikam"), 
                                standard_colnames,
                                verbose = FALSE,
                                logger = NA){
  
  classifier <- match.arg(classifier)
  
  check_standard_colnames(standard_colnames, classifier, logger = logger)
  
  # --- Prepare data
  if(classifier == "traptagger") {
    df_res <- prepare_traptagger(df, logger = logger)
  } else if (classifier == "digikam") {
    df_res <- prepare_digikam(df, logger = logger)
  } else if (classifier == "zooniverse") {
    df_res <- df
  }
  
  # --- Keep only columns expected with classifier 'classifier'
  cols <- standard_colnames[classifier][!is.na(standard_colnames[classifier])]
  
  df_res <- keep_only_expected_columns(df_res, expected = cols, 
                                       verbose = verbose, approx = TRUE,
                                       logger = logger)
  
  # --- Rename according to new standard
  df_res <- rename_standard(df_res, standard_colnames, classifier,
                            logger = logger)
  
  # --- Keep only standard columns
  std_cols <- standard_colnames$new[!is.na(standard_colnames$new)]
  
  df_res <- keep_only_expected_columns(df_res, expected = std_cols, 
                                       verbose = verbose, approx = FALSE,
                                       logger = logger)
  
  # --- Reorder
  df_res <- df_res %>% select(all_of(std_cols))
  
  return(df_res)
}

#' Rename columns to match standard names
#'
#' Renames the columns in a dataframe to match the standard names
#' given in `standard_df`.
#'
#' @param df The dataframe with the columns to rename
#' @param standard_df A dataframe with 2 columns (at least). The columns must be named like `classifier` 
#' (either `zooniverse`, `traptagger` or `digikam`) and `new`.
#' @param classifier (optional) the classifier used to create the dataframe `df`. 
#' Can be `zooniverse`, `traptagger` or `digikam`. If it is not given, will be guessed from `df` column names.
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return A dataframe for which the columns have been renamed/added.
#'
#' @details 
#' The column named like the classifier in `standard_df` are the column names 
#' that are expected in the initial file, which will be matched in the column names of `df` 
#' using partial matching (case insensitive and removing blanks).
#' Columns in the classifier column will be renamed as the 
#' corresponding value in `new`. If no pre-existing column corresponds to `new` (indicated with a `NA`)
#' then the column will be created and filled with `NA`.
#' 
#' @noRd
rename_standard <- function(df,
                            standard_df,
                            classifier = c("zooniverse", "traptagger", "digikam"),
                            logger = NA){
  
  if (missing(classifier)) {
    classifier <- guess_classifier(colnames(df),
                                   logger = logger)
  }
  
  classifier <- match.arg(classifier)
  
  check_standard_colnames(standard_df, classifier, logger = logger)
  
  # Rename the columns we want into 'classifier'
  newnames_df <- standard_df %>% rename("classifier" = all_of(classifier))
  
  # Get only the columns to rename
  newnames_df <- newnames_df %>% filter(!is.na(classifier)) %>%
    select(classifier, new)
  
  newnames <- as.character(newnames_df$classifier)
  names(newnames) <- newnames_df$new
  
  # Copy df
  df_res <- df
  colnames(df_res) <- names(newnames[match(newnames, colnames(df_res))])
  
  return(df_res)
}

## Fill info ----------------------------------------

#' Fill capture info
#' 
#' Fills different columns related to capture info.
#'
#' @param df a dataframe for which columns should be filled. 
#' @param classifier The classifier
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return A dataframe with values filled for locationID, cameraID,
#' roll and capture. Also fills season if Zooniverse.
#' 
#' @noRd
fill_capture_info <- function(df,
                              classifier = c("zooniverse", "traptagger", "digikam"),
                              logger = NA) {
  
  classifier <- match.arg(classifier)
  
  if (classifier == "zooniverse") {
    # Copy df
    res <- df
    
    # --- Location code
    # Set location to first set of majuscules letters in eventID
    locationID <- str_extract(res$eventID, "^[A-Z]+")
    
    unique_locationID <- unique(locationID)
    
    if(length(unique_locationID) != 1) {
      msg <- paste("Location extracted from eventID is not unique: ", 
                   paste(locationID, collapse = ", "))
      write_log_message(msg, logger = logger, level = "warn")
      warning(msg)
    }
    
    res$locationID <- locationID
    
    # --- Season
    season <- str_match(res$season, "^.*_S(.*)$")[,2] # Get second match
    res$season <- season
    
  } else if (classifier == "traptagger") {
    res <- fill_capture_info_traptagger(df, logger = logger)
  } else if (classifier == "digikam") {
    res <- fill_capture_info_digikam(df)
  }
  
  return(res)
}

#' Fill capture info for Digikam
#'
#' Fills some data on capture event for Digikam-format data.
#' Fills season, roll and capture (as character).
#' 
#' @param df The dataframe to fill. Must have a column 'locationID'
#'
#' @return The dataframe with completed values for columns 
#' 'season', 'roll' and 'captureID'. season' defaults to 1; 
#' 'roll' is extracted from 'file_path_1'; 
#' 'captureID' is the i-th picture for a given camera.
#' 
#'
#' @noRd
fill_capture_info_digikam <- function(df){
  
  # Copy df
  df_res <- df
  
  # --- Add capture number
  df_res <- df_res %>% arrange(cameraID, eventDate, eventTime)
  
  df_res <- df_res %>% 
    group_by(cameraID) %>%
    mutate(capture = order(eventDate, eventTime))
  
  df_res$capture <- as.character(df_res$capture)
  
  # --- Fill season
  df_res$season <- NA # Season is NA
  
  # --- Fill roll
  extracted_roll <- str_extract(df_res$filePath1, 
                                pattern = "Roll\\d+")
  extracted_roll <- str_extract(extracted_roll, 
                                pattern = "\\d+")
  
  df_res$roll <- extracted_roll
  
  # --- Ungroup and cast as df
  df_res <- df_res %>% ungroup() %>%
    as.data.frame()
  
  return(df_res)
}

#' Fill capture info TrapTagger
#'
#' Completes capture info for TrapTagger from the eventID.
#' Completes values for columns cameraID, roll, captureID, location and 
#' adds column season (NA). These columns are character.
#' 
#' @param df The dataframe to complete
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return The dataframe with values completed for the captureID
#' info columns. If no info, it remains NA.
#' 
#'
#' @noRd
fill_capture_info_traptagger <- function(df, logger = NA) {
  
  df_res <- df
  
  # --- Parse eventID
  capture_split <- strsplit(df_res$eventID, "#")
  
  nfields <- unique(sapply(capture_split, length))
  
  if(length(nfields) != 1) {
    msg <- "All eventID fields don't have the same length..."
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  } else if (nfields == 3) { # First format
    location_event <- sapply(capture_split, function(l) l[1])
    
    roll <- sapply(capture_split, function(l) l[2])
    
    # Format location_event to get location and event
    locationID <- gsub(location_event, pattern = "\\d*", 
                       replacement = "")
    capture <- gsub(location_event, pattern = "[a-zA-Z]*", 
                    replacement = "")
    
  } else if (nfields == 4) { # Second format
    
    # 2 formats possible (at least fount in TSW roll 5)
    # To discriminate, look at last part (most of the time
    # it is event but can be camera)
    test <- sapply(capture_split, function(l) l[4])
    test <- suppressWarnings(as.integer(test))
    
    # If any last part cannot be coerced to numeric, then use locationID#capture#roll#cameraID format
    if(any(is.na(test))) {
      locationID <- sapply(capture_split, function(l) l[1])
      capture <- sapply(capture_split, function(l) l[2])
      roll <- sapply(capture_split, function(l) l[3])
    } else { # use locationID#cameraID#roll#capture format
      locationID <- sapply(capture_split, function(l) l[1])
      roll <- sapply(capture_split, function(l) l[3])
      capture <- sapply(capture_split, function(l) l[4])
    }
    
  }
  
  unique_location <- unique(locationID)
  if(length(unique_location) != 1) {
    msg <- paste("Non-unique location name:",
                 paste(unique_location, collapse = ", "))
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
  }
  
  # --- Sanity check
  check_locationID <- get_NAs(transformed_data = suppressWarnings(as.integer(locationID)), 
                              origin_data = locationID, 
                              collapse = FALSE,
                              nmax = length(locationID)) # all should be NAs
  if(length(check_locationID) != length(locationID)) { # problem, is maybe a numeric
    head_locationID <- locationID[1:min(10, length(locationID))]
    head_locationID <- paste(head_locationID, collapse = ", ")
    msg <- paste("locationID might be incorrect: all values are corecible to integer.",
                 "Here are some data:",
                 head_locationID)
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  check_roll <- get_NAs(transformed_data = suppressWarnings(as.integer(roll)), 
                        origin_data = roll, 
                        collapse = FALSE,
                        nmax = length(roll))
  if(length(check_roll) == length(roll)) { # problem, is maybe a character
    head_roll <- roll[1:min(10, length(roll))]
    head_roll <- paste(head_roll, collapse = ", ")
    msg <- paste("roll might be incorrect: values not coercible to integer.",
                 "Here are some data:",
                 head_roll)
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  check_capture <- get_NAs(transformed_data = suppressWarnings(as.integer(capture)), 
                           origin_data = capture, 
                           collapse = FALSE,
                           nmax = length(capture))
  if(length(check_capture) == length(capture)) { # problem, is maybe a character
    head_capture <- capture[1:min(10, length(capture))]
    head_capture <- paste(head_capture, collapse = ", ")
    msg <- paste("roll might be incorrect: values not coercible to integer.",
                 "Here are some data:",
                 head_capture)
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  # --- Fill in info
  df_res$season <-  NA
  df_res$roll <- as.character(roll)
  df_res$capture <- as.character(capture)
  df_res$locationID <- as.character(locationID)
  
  return(df_res)
}

## Re-format columns --------------------------------

#' Standardize times
#' 
#' Standardizes a times vector to match the format "HH:MM:SS"
#'
#' @param times A times vector (character). Will work if the times are
#' already "HH:MM:SS" or "HH:MM:SS AM/PM"
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return The vector standardized in the format "HH:MM:SS"
#' 
#' @noRd
standardize_time <- function(times, logger = NA){
  
  # Copy
  tim <- times
  
  # Split
  timsplit <- str_split(tim, ":")
  
  # Check for AM/PM
  AMPM <- grep(tim, pattern = "AM|PM", ignore.case = TRUE)
  if(length(AMPM) != 0) {
    # Transform PM hours
    
    # Helper function
    convert_pm <- function(t) {
      PM <- grepl(t[3], pattern = "PM", ignore.case = TRUE)
      
      if(PM) {
        t[1] <- as.numeric(t[1])
        if(t[1] != 12) {
          t[1] <- as.character(as.numeric(t[1]) + 12)
        } else { # case 12 PM (midnight)
          t[1] <- as.character(as.numeric(t[1]) - 12)
        }
      }
      
      t[3] <- gsub(x = t[3],
                   pattern = " AM|PM", replacement = "",
                   ignore.case = TRUE)
      return(t)
    }
    timsplit <- lapply(timsplit, function(ti) convert_pm(ti))
  }
  
  # Count fields
  cnt <- sapply(timsplit, function(l) length(l))
  cnt <- unique(cnt)
  
  if(length(cnt) > 1){
    msg <- "Bad time formatting"
    write_log_message(msg, logger = logger, level = "error")
    stop(msg)
  }else{
    if(cnt == 2){
      timsplit <- lapply(timsplit, function(l) c(l, "00"))
    }
    
    test <- tryCatch(
      lapply(timsplit, as.numeric),
      warning = function(w){
        # Get NAs
        tim_numeric <- suppressWarnings(lapply(timsplit, as.numeric))  
        
        # TRUE if at least one NA
        tim_numeric_NAs <- sapply(tim_numeric, function(t) any(is.na(t)))
        tim_numeric_NAs[tim_numeric_NAs] <- NA # transform TRUE to NA
        
        NAs <- get_NAs(transformed_data = tim_numeric_NAs, 
                       origin_data = times)
        
        msg <- paste("Could not cast times", NAs, "to numeric.")
        write_log_message(msg, logger = logger, level = "error")
        stop(msg)
      }
    )
    
    # Helper function to add a zero to some elements in a list 'l'
    # for the indices 'index'
    # and for the i-th element of each list element
    add_zero <- function(l){
      for(i in 1:3){
        elem <- sapply(l, function(li) li[i])
        index <- which(nchar(elem) == 1)
        for(ind in index){
          l[[ind]][i] <- paste0("0", l[[ind]][i])
        }
      }
      return(l)
    }
    
    timsplit <- add_zero(timsplit)
    
    tim_final <- sapply(timsplit, paste, collapse =":")
    tim_final <- chron::times(tim_final)
    
    NAs <- get_NAs(transformed_data = tim_final,
                   origin_data = times)
    
    if(length(NAs) != 0) {
      msg <- paste("Times", NAs, "could not be transformed to times.")
      write_log_message(msg, logger = logger, level = "info")
      message(msg)
    }
    
  }
  return(tim_final)
}

#' Standardize dates
#'
#' Standardizes a vector of dates to the format "YYYY-MM-DD".
#' 
#' @param dates A dates vector (character). Accepts all formats coercible to date
#' with lubridate::as_date, else will look for "\%d-\%m-\%Y" or "\%m-\%d-\%Y".
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return A vector of dates in the format "YYYY-MM-DD".
#'
#' @noRd
standardize_date <- function(dates, logger = NA){
  
  # Copy
  dat <- dates
  
  # Look for '/' and replace with '-'
  dat <- gsub(dat, 
              pattern = "/", replacement = "-")
  
  # Try vanilla date conversion
  dat_test <- suppressWarnings( lubridate::as_date(dat) )
  
  if(all(is.na(dat_test))) { # Failed to parse
    # Try month/date first
    sup12 <- strsplit(dat, "-")
    sup12 <- sapply(sup12, function(l) as.numeric(l[1]) > 12) # Find first slot > 12
    
    if (length(sup12[sup12]) != 0) { # At least one leading digit > 12
      # Leading digit = month
      fmt <- "%d-%m-%Y"
    } else {
      # Leading digit = day
      fmt <- "%m-%d-%Y"
    }
    dat <- lubridate::as_date(dat, format = fmt)
  } else { # Parsing successful
    dat <- dat_test
  }
  
  NAs <- get_NAs(transformed_data = dat, origin_data = dates)
  if(length(NAs) != 0) {
    msg <- paste("Date format failed to parse for values:\n",
                 NAs, "...")
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  return(dat)
}

## Misc -----------------------------------------
#' Add columns that don't exist
#'
#' Helper function to add columns that are not in the original data (code found on https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist)
#' 
#' @param data the dataframe to add columns to
#' @param cname a character vector with names that should be added (these names can be present in the data already)
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return The dataframe with all original columns + those that are in 'cname' 
#'
#' @noRd
fncols <- function(data, cname, approx = FALSE, logger = NA) {
  res <- data
  
  # First check if the name we wanna add is "almost" in data
  if(approx) {
    for(cn in cname) {
      match <- grep(pattern = paste0("^", cn, "$"), 
                    x = names(data), ignore.case = TRUE)
      if(length(match) == 1) {
        if(names(data)[match] != cn){ # If match but different
          msg <- paste("Match found in column names: renaming column", 
                       names(data)[match],
                       "into", cn)
          write_log_message(msg, logger = logger, level = "info")
          message(msg)
          
          names(res)[match] <- cn
        }
      } else if (length(match) > 1) {
        msg <- paste0("Multiple matches for ", cn, " in column names: ", 
                     paste(names(data)[match], collapse = ", "),
                     ", not renaming.")
        write_log_message(msg, logger = logger, level = "info")
        message(msg)
      }
    }
  }
  
  add <- cname[!(cname %in% names(res))]
  
  if(length(add)!=0){
    res[add] <- NA
  } 
  return(res)
}

#' Keep only expected columns
#'
#' @param df The dataframe to filter
#' @param expected Character vector of expected column names.
#' @param verbose Should a message be displayed if some columns are deleted?
#' @param approx Should partial name matching be used to match expected columns?
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return The dataframe with all columns in expected, and no others.
#' If a column was not originally there, then it is filled with NAs.
#' 
#'
#' @noRd
keep_only_expected_columns <- function(df, expected, verbose, approx,
                                       logger = NA) {
  
  # --- Create columns that don't exist 
  df_res <- fncols(df, expected, approx = approx, logger = logger)
  
  # --- Discard all columns not in the standard (and reorder)
  discarded <- colnames(df_res)[!(colnames(df_res) %in% expected)]
  
  if(verbose) {
    if(length(discarded) != 0) {
      msg <- paste0("Column(s) ", paste(discarded, collapse = ", "),
                    " will be discarded.")
      write_log_message(msg, logger = logger, level = "info")
      message(msg)
    }
  }
  
  df_res <- df_res %>% select(all_of(expected)) # reorder in the same order as cols used before
  
  return(df_res)
}

#' Check standard column names
#'
#' Check that the standard column names match expectations, i.e.
#' is a dataframe with >= 2 columns named like the classifier object and 'new'.
#' 
#' @param standard_colnames The dataframe to check
#' @param classifier The classifier
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return Nothing or stops execution if standard_colnames is not in
#' compliance with expectations.
#' 
#'
#' @noRd
check_standard_colnames <- function(standard_colnames, 
                                    classifier = c("zooniverse", "traptagger", "digikam"),
                                    logger = NA) {
  
  classifier <- match.arg(classifier)
  
  if(!is.data.frame(standard_colnames)) {
    msg <- "standard_colnames must be a dataframe"
    write_log_message(msg, logger = logger, level = "error")
    stop(msg)
  } else if (ncol(standard_colnames) < 2) { # if it is a df but has less than 2 columns
    msg <- "standard_colnames must have 2 columns at least"
    write_log_message(msg, logger = logger, level = "error")
    stop(msg)
  } else { # a df with >= 2 columns
    cond <- all(c(classifier, 'new') %in% colnames(standard_colnames))
    if(!cond) {
      msg <- paste("2 columns in standard_colnames must be named",
                   paste(c(classifier, 'new'), sep = ", "))
      write_log_message(msg, logger = logger, level = "error")
      stop(msg)
    }
  }
}

#' Get NAs
#'
#' Get the original data that are NA in a new vector.
#' 
#' @param transformed_data The data that was transformed which we want to check for NAs
#' @param origin_data The original, untransformed data
#' @param nmax Maximum number of values to return. If zero, returns all values
#' @param collapse Whether to collapse the values into a character (collapses 
#' with ', ')
#'
#' @return Returns a vector of the original values that were transformed 
#' to NAs (if collapse is FALSE). If collapse is TRUE, returns a character
#' containing the original values that were transformed to NA.
#' If no NAs were found, returns a vector numeric(0)
#'
#' @noRd
get_NAs <- function(transformed_data, origin_data, 
                    nmax = 50, collapse = TRUE) {
  
  NAs <- which(is.na(transformed_data))
  NAs_orig <- origin_data[NAs]
  
  if(length(NAs_orig) != 0) { # NAs found
    if(nmax != 0) {
      NAs_orig <- head(NAs_orig, n = nmax)
    }
    
    if(collapse){
      res <- paste(NAs_orig, collapse = ", ")
    }else {
      res <- NAs_orig
    }
  } else {
    res <- NAs_orig
  }
  
  return(res)
}

#' Get locationID
#' 
#' Gets the locationID from a string of filename. The locationID is assumed to
#' be majuscule letters leading filename.
#' 
#' @param filename A string of filename. If it does not begin with majuscule letters, NA will be returned.
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`.
#' 
#' @return The location code (string) as the leading majuscule letters of filename.
#'
#' @note If the file name is `Marinmane NR _record_table_0min_deltaT_2021-06-10.csv`, will
#' return the location code `MAR`.
#' 
#'
#' @noRd
locationID_from_filename <- function(filename, logger = NA) {
  
  fname <- basename(filename)
  
  if(fname == "Marinmane NR _record_table_0min_deltaT_2021-06-10.csv") {
    # Manually set location
    locationID <- "MAR"
    msg <- "Manually setting location code for Marinmane."
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
  } else {
    locationID <- str_extract(fname, "^[A-Z]+")
  }
  return(locationID)
}

#' Check values
#'
#' @param vec A vector of values
#'
#' @return TRUE if any value is `NA`, `NULL`, `""` (empty chacacter),
#' `NaN`.
#' 
#'
#' @noRd
has_empty_values <- function(vec) {
  
  # NA
  if ( any(is.na(vec)) ) return(TRUE)
  
  # NULL
  if ( any(is.null(vec)) ) return(TRUE)
  
  # ""
  if ( any(as.character(vec) == "") ) return(TRUE)
  
  # NaN
  if ( any(is.nan(vec)) ) return(TRUE)
  
  return(FALSE)
}


#' Prepare df to print as message
#'
#' Prepares a dataframe to be written to the console with `message()`
#' (transforms the dataframe to a single string).
#' 
#' @param df The dataframe
#'
#' @return A string with tabulations and newlines to print 
#' a dataframe in a good form
#' 
#' @noRd
#'
#' @examples
#' df_for_message(head(traptagger, 3))
df_for_message <- function(df) {
  
  # Transform to matrix
  dfm <- as.matrix(df)
  
  # Add names
  dfm <- rbind(colnames(df), dfm)
  
  # Paste
  dfp <- apply(dfm, 1, paste, collapse = "\t")
  dfp <- paste(dfp, collapse = "\n")
  
  return(dfp)
}

