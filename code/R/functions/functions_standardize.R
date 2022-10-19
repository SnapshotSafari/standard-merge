library(here)
library(tidyverse)
library(lubridate)
library(stringr)
library(chron)

here::i_am("code/R/functions/functions_standardize.R") # Set working directory


#' Guess classifier
#' 
#' Guesses the classifier used to annotate the data based on the column names
#' given in colnames_df.
#'
#' @param colnames_df A character vector of column names.
#'
#' @return The classifier: either zooniverse, digikam or traptagger.
#' 
#' @export
#'
#' @examples
guess_classifier <- function(colnames_df) {
  
  if ("question__species" %in% colnames_df) {
    classifier <- "zooniverse"
  } else if ("HierarchicalSubject" %in% colnames_df) {
    classifier <- "digikam"
  } else if ("timestamp" %in% colnames_df) {
    classifier <- "traptagger"
  } else {
    stop("Could not set classifier from column names: column names do not contain 'question__species', 'HierarchicalSubject' or 'timestamp'\n")
  }
  return(classifier)
}
#' Prepare TrapTagger data
#'
#' Prepares data processed with TrapTagger to be standardized by 
#' creating columns date and time from timestamp and formatting
#' capture_label_xx into a unique column.
#' 
#' @param df The dataframe to standardize (must have columns starting with capture_label
#' and column timestamp)
#'
#' @return Returns the dataframe with 2 additional columns date and time, minus the timestamp column, and with merged capture_label
#' @export
#'
#' @examples
prepare_traptagger <- function(df){
  
  # --- Address multiple species
  # Transform "None" species to NA
  df_res <- df %>% mutate(
    across(.cols = starts_with("capture_label"),
           .fns = ~ na_if(.x, y = "None")))
  
  # Pivot table
  df_res <- df_res %>% pivot_longer(cols = starts_with("capture_label"), 
                                    names_to = "capture_nb",
                                    values_to = "capture_label",
                                    values_drop_na = TRUE) 
  
  # --- Split dates/times
  date_time <- strsplit(df_res$timestamp, " ")
  
  unique_length <- unique(sapply(date_time, length))
  
  if(unique_length != 2) {
    warning("Some dates/times may be ill-parsed")
  }
  
  df_res$date <- sapply(date_time, function(l) l[1])
  df_res$time <- sapply(date_time, function(l) l[2])
  
  # --- Delete intermediate columns
  df_res <- df_res %>% select(-c(timestamp, capture_nb))
  
  return(df_res)
}

#' Add columns that don't exist
#'
#' Helper function to add columns that are not in the original data (code found on https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist)
#' 
#' @param data the dataframe to add columns to
#' @param cname a character vector with names that should be added (these names can be present in the data already)
#'
#' @return The dataframe with all original columns + those that are in 'cname' 
#' @export
#'
#' @examples
fncols <- function(data, cname, approx = FALSE) {
  res <- data
  
  # First check if the name we wanna add is "almost" in data
  if(approx) {
    for(cn in cname) {
      match <- grep(pattern = paste0("^", cn, "$"), 
                    x = names(data), ignore.case = TRUE)
      if(length(match) == 1) {
        if(names(data)[match] != cn){ # If match but different
          message("Match found in column names: renaming column ", 
                  names(data)[match],
                  " into ", cn)
          names(res)[match] <- cn
        }
      } else if (length(match) > 1) {
        message("Multiple matches for ", cn, " in column names: ", 
                paste(names(data)[match], collapse = ", "),
                ", not renaming.")
      }
    }
  }
  
  add <- cname[!(cname %in% names(res))]
  
  if(length(add)!=0){
    res[add] <- NA
  } 
  return(res)
}

#' Recode a behavior column
#'
#' @param behavior a character vector of behaviors, expected to be separated by a '_&_'
#'
#' @return A dataframe with one column per behavior initially present, with
#' 1 if the behavior was present and 0 otherwise.
#' 
#' @export
#'
#' @examples
recode_behavior_digikam <- function(behaviors) {
  
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
      warning(msg)
    }
  } else {
    warning("All behaviors are NA.")
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

#' Prepare Digikam data
#'
#' Standardizes Digikam data to be ready to be standardized: removes 'X' index column,
#' (if it exists), merges 'Directory' and 'Filename' to create a path, 
#' splits 'metadata_Behaviour' column into the relevant behaviors and 
#' recodes 'metadata_young_present' into 0 and 1.
#' 
#' @param df Dataframe to standardize (must have columns 'metadata_Behaviour' and 'metadata_young_present').
#' 
#' @return Returns the dataframe without 'X' column, 'Directory' and 'Filename' merged
#' into 'file_path_1' column, 'metadata_Behaviour' splitted into up to 6 columns
#' 'Eating', 'Drinking', 'Resting', 'Standing', 'Moving', 'Interacting' (depending on the
#' behavoirs that were present in the data) and the 'metadata_young_present' column recoded
#' so that 'Yes'/'No' becomes 0/1.
#' @export
#'
#' @examples
prepare_digikam <- function(df){
  
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
    warning("Could not create column file_path_1: missing columns 'Directory' and 'FileName'")
  }
  
  # --- Format behaviors
  behaviors <- recode_behavior_digikam(df_res$metadata_Behaviour) 
  
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

#' Keep only expected columns
#'
#' @param df The dataframe to filter
#' @param expected Character vector of expected column names.
#' @param verbose Should a message be displayed if some columns are deleted?
#' @param approx Should partial name matching be used to match expected columns?
#'
#' @return The dataframe with all columns in expected, and no others.
#' If a column was not originally there, then it is filled with NAs.
#' @export
#'
#' @examples
keep_only_expected_columns <- function(df, expected, verbose, approx) {
  
  # --- Create columns that don't exist 
  df_res <- fncols(df, expected, approx = approx)
  
  # --- Discard all columns not in the standard (and reorder)
  discarded <- colnames(df_res)[!(colnames(df_res) %in% expected)]
  
  if(verbose) {
    if(length(discarded) != 0) {
      message(paste0("Column(s) ", paste(discarded, collapse = ", "),
                     " will be discarded."))
    }
  }
  
  df_res <- df_res %>% select(all_of(expected)) # reorder in the same order as cols used before
  
  return(df_res)
}

#' Check standard column names
#'
#' Check that the standard column names match expectations, i.e.
#' is a dataframe with >= 2 columns named like the classifier object and 'new'.

#' @param standard_colnames The dataframe to check
#' @param classifier The classifier.
#'
#' @return Nothing or stops execution if standard_colnames is not in
#' compliance with expectations.
#' 
#' @export
#'
#' @examples
check_standard_colnames <- function(standard_colnames, 
                                    classifier = c("zooniverse", "traptagger", "digikam")) {
  
  classifier <- match.arg(classifier)
  
  if(!is.data.frame(standard_colnames)) {
    stop("standard_colnames must be a dataframe")
  } else if (ncol(standard_colnames) < 2) { # if it is a df but has less than 2 columns
    stop("standard_colnames must have 2 columns at least")
  } else { # a df with >= 2 columns
    cond <- all(c(classifier, 'new') %in% colnames(standard_colnames))
    if(!cond) {
      stop(paste("2 columns in standard_colnames must be named",
                 paste(c(classifier, 'new'), sep = ", ")))
    }
  }
}

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
#'
#' @return A dataframe with 25 columns with the standardized names
#' @export
#'
#' @examples
standardize_columns <- function(df, 
                                classifier = c("zooniverse", "traptagger", "digikam"), 
                                standard_colnames,
                                verbose = FALSE){
  
  classifier <- match.arg(classifier)
  
  check_standard_colnames(standard_colnames, classifier)
  
  # --- Keep only columns expected with classifier 'classifier'
  cols <- standard_colnames[classifier][!is.na(standard_colnames[classifier])]
  
  df_res <- keep_only_expected_columns(df, expected = cols, 
                                       verbose = verbose, approx = TRUE)
  
  # --- Rename according to new standard
  df_res <- rename_standard(df_res, classifier, standard_colnames)
  
  # --- Keep only standard columns
  std_cols <- standard_colnames$new[!is.na(standard_colnames$new)]
  
  df_res <- keep_only_expected_columns(df_res, expected = std_cols, 
                                       verbose = verbose, approx = FALSE)

  # --- Reorder
  df_res <- df_res %>% select(all_of(std_cols))
  
  return(df_res)
}

#' Rename columns according to standard
#'
#' This function renames the existing columns in a dataframe to match the standard names.
#'
#' @param df The dataframe with the columns to rename
#' @param classifier The classifier used to create the dataframe df. Can be 'zooniverse', 'traptagger', 'digikam'.
#' @param standard_colnames A dataframe with 2 columns (at least) named like the classifier
#' and 'new'.
#' The column named like the classifier contains column names 
#' that are expected in the initial file. These names will be matched in the column names of df using partial matching (case insensitive and
#' removing blanks).
#' The column 'new' contains the column names that are expected in the final file. 
#' Columns in the classifier column will be renamed following the name of the 
#' corresponding value in 'new'. If no old column corresponds to 'new' (indicated with a NA)
#' then the column will be created and filled with NAs.
#' 
#' @return Returns a dataframe for which the columns have been renamed
#' @export
#'
#' @examples
rename_standard <- function(df,
                            classifier = c("zooniverse", "traptagger", "digikam"),
                            standard_colnames){
  
  classifier <- match.arg(classifier)
  
  check_standard_colnames(standard_colnames, classifier)
  
  # Rename the columns we want into 'classifier'
  newnames_df <- standard_colnames %>% rename("classifier" = all_of(classifier))
  
  # Get only the columns to rename
  newnames_df <- newnames_df %>% filter(!is.na(classifier)) %>%
    select(classifier, new)
  
  newnames <- as.character(newnames_df$classifier)
  names(newnames) <- newnames_df$new
  
  # Copy df
  df_res <- df
  colnames(df_res) <- names(newnames[match(newnames, names(df_res))])
  
  return(df_res)
}

standardize_time <- function(times){
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
    stop("Bad time formatting")
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
        stop(paste("Could not cast times", NAs, "to numeric."))
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
    tim_final <- times(tim_final)
    
    NAs <- get_NAs(transformed_data = tim_final,
                   origin_data = times)
    
    if(length(NAs) != 0) {
      message("Times ", NAs, " could not be transformed to times.")
    }
    
  }
  return(tim_final)
}

standardize_date <- function(dates){
  # Copy
  dat <- dates
  
  # Look for '/' and replace with '-'
  dat <- gsub(dat, 
                pattern = "/", replacement = "-")
  
  # Try vanilla date conversion
  dat_test <- suppressWarnings( as_date(dat) )
  
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
    dat <- as_date(dat, format = fmt)
  } else { # Parsing successful
    dat <- dat_test
  }


  NAs <- get_NAs(transformed_data = dat, origin_data = dates)
  if(length(NAs) != 0) {
    warning(paste("Date format failed to parse for values:\n",
                    NAs, "..."))
  }
  
  return(dat)
}

#' Get capture_id
#'
#' Get the capture_id from the capture info. All vectors must be the
#' same length or they will be recycled.
#' 
#' @param season character vector of season
#' @param cam_site character vector of cam_site
#' @param roll character vector of roll
#' @param capture character vector of capture
#'
#' @return A character vector of the same length of the inputs (or the
#' longest input) with fields formatted as 
#' season#cam_site#roll#event_no
#' 
#' @export
#'
#' @examples
get_capture_id <- function(season, cam_site, roll, capture) {
  
  capture_id <- paste(season,
                      cam_site,
                      roll, 
                      capture,
                      sep = "#")
  
  return(capture_id)
  
}
#' Fill capture info for Digikam
#'
#' Fills some data on capture event for Digikam-format data.
#' Fills capture_id, season, roll and capture (as character).
#' 
#' @param df The dataframe to fill. Must have a column 'location_code'
#'
#' @return The dataframe with completed values for columns 
#' 'season', 'roll' and 'capture'. season' defaults to 1; 
#' 'roll' is extracted from 'file_path_1'; 
#' 'capture' is the i-th picture for a given camera.
#' 
#' @export
#'
#' @examples
fill_capture_info_digikam <- function(df){
  
  # Copy df
  df_res <- df
  
  # --- Add capture number
  df_res <- df_res %>% 
    group_by(cam_site) %>%
    mutate(capture = order(date, time))
  
  df_res$capture <- as.character(df_res$capture)
  
  # --- Fill season
  df_res$season <- NA # Season is NA

  # --- Fill roll
  extracted_roll <- str_extract(df_res$file_path_1, 
                                pattern = "Roll\\d+")
  extracted_roll <- str_extract(extracted_roll, 
                                pattern = "\\d+")
  
  df_res$roll <- extracted_roll
  
  return(df_res)
}

#' Fill capture info TrapTagger
#'
#' Completes capture info for TrapTagger from the capture_id.
#' Completes values for columns cam_site, roll, capture, location and 
#' adds column season (NA). These columns are character.
#' 
#' @param df The dataframe to complete
#'
#' @return The dataframe with values completed for the capture
#' info columns. If no info, it remains NA.
#' 
#' @export
#'
#' @examples
fill_capture_info_traptagger <- function(df) {
  
  df_res <- df
  
  # --- Parse capture_id
  capture_split <- strsplit(df_res$capture_id, "#")
  
  nfields <- unique(sapply(capture_split, length))
  
  if(length(nfields) != 1) {
    warning("All capture_id fields don't have the same length...")
  } else if (nfields == 3) { # First format
    location_event <- sapply(capture_split, function(l) l[1])
    
    roll <- sapply(capture_split, function(l) l[2])
    cam_site <- sapply(capture_split, function(l) l[3])
    
    # Format location_event to get location and event
    location_code <- gsub(location_event, pattern = "\\d*", 
                          replacement = "")
    capture <- gsub(location_event, pattern = "[a-zA-Z]*", 
                    replacement = "")
    
  } else if (nfields == 4) { # Second format
    location_code <- sapply(capture_split, function(l) l[1])
    cam_site <- sapply(capture_split, function(l) l[2])
    roll <- sapply(capture_split, function(l) l[3])
    capture <- sapply(capture_split, function(l) l[4])
  }
  
  unique_location <- unique(location_code)
  if(length(unique_location) != 1) {
    message(paste("Non-unique location name:",
                  paste(unique_location, collapse = ", ")))
  }
  
  # --- Fill in info
  df_res$season <-  NA
  df_res$cam_site <- as.character(cam_site)
  df_res$roll <- as.character(roll)
  df_res$capture <- as.character(capture)
  df_res$location_code <- as.character(location_code)
  
  return(df_res)
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
#' @export
#'
#' @examples
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

#' Standardize species
#'
#' Eliminate species duplicate names (things like 'birdofprey' ans 'birdsofprey')
#' 
#' @param species vector of species names
#'
#' @return the vector of species names with names stabdardized
#' 
#' @export
#'
#' @examples
standardize_species <- function(species){
  
  res <- species
  
  # --- Convert to character (case factor)
  res <- as.character(res)
  
  # --- Only lowercase
  res <- tolower(res)
  
  # --- Remove spaces
  res <- gsub(pattern = "\\s+", 
              replacement = "", 
              res)
  
  # --- Remove underscore
  res <- gsub(pattern = "_+", 
              replacement = "", 
              res)
  
  # --- Standardize
  res[res == "birdsofprey"] <- "birdofprey"
  res[res == "duiker"] <- "duikercommon"
  res[res == "duikercommongrey"] <- "duikercommon"
  res[res == "harecape"] <- "hare"
  res[res == "wildcat"] <- "catafricanwild"
  res[res == "groundsquirrel"] <- "squirrelground"
  res[res == "mongoosewater"] <- "mongoosewatermarsh"
  res[res == "mongoosecapesmallgrey"] <- "mongoosesmallcapegrey"
  res[res == "mongooseegyptianlargegrey"] <- "mongooselargegrey"
  res[res == "mongoose(whitetailed)"] <- "mongoosewhitetailed"
  res[res == "springhare"] <- "harespring"
  res[res == "zorillastripedpolecat"] <- "polecatstriped"
  res[res == "rhino"] <- "rhinoceros"
  res[res == "zorillapolecatstriped"] <- "polecatstriped"
  res[res == "genetlargespotted"] <- "genetcapelargespotted"
  res[res == "genet(small)"] <- "genetcommonsmallspotted"
  res[res == "genetcape"] <- "genetcapelargespotted"
  res[res == "rhinocerossp"] <- "rhinoceros"
  res[res == "genetsmallspotted"] <- "genetcommonsmallspotted"
  res[res == "galagosouthernlesser"] <- "bushbaby"
  res[res == "aardvark"] <- "aardvarkantbear"
  res[res == "duiker"] <- "duikercommon"
  res[res == "grysbok"] <- "grysbokcape"
  res[res == "rodents"] <- "rodent"
  res[res == "rhebok(grey)"] <- "rhebokgrey"
  res[res == "unknown"] <- "unresolvable"
  res[res == "africanwildcat"] <- "catafricanwild"
  res[res == "catdomestic"] <- "domesticanimal"
  # res[res == "lionmale"] <- "lion"
  # res[res == "lionfemale"] <- "lion"
  # res[res == "lioncub"] <- "lion"
  # res[res == "lionfemaleorcub"] <- "lion"
  res[res == "NOT identifiable"] <- "unresolvable"
  res[res == "Unidentifiable"] <- "unresolvable"
  res[res == "unidentifiable"] <- "unresolvable"
  res[res == "UNIDENTIFIED"] <- "unresolvable"
  
  # ################### Added
  res[res == "africanwilddog"] <- "wilddog"
  res[res == "antelopeunknown"] <- "antelope"
  res[res == "bandedmongoose"] <- "mongoosebanded"
  res[res == "batearedfox"] <- "foxbateared"
  res[res == "blanks"] <- "blank"
  res[res == "blackbackedjackal"] <- "jackalblackbacked"
  res[res == "bustardludwig's"] <- "bustardludwigs"
  res[res == "blackwildebeest"] <- "wildebeestblack"
  res[res == "bluewildebeest"] <- "wildebeestblue"
  res[res == "brownhyena"] <- "hyenabrown"
  res[res == "capeporcupine"] <- "porcupinecape"
  res[res == "capemountainzebra"] <- "zebramountaincape"
  res[res == "commonduiker"] <- "duikercommon"
  res[res == "commonwarthog"] <- "warthog"
  res[res == "crestedguineafowl"] <- "guineafowlcrested" # ?
  res[res == "gemsbokoryx"] <- "gemsbok"
  res[res == "dwarfmongoose"] <- "mongoosedwarf"
  res[res == "feralcat"] <- "catferal"
  res[res == "fowlguinea"] <- "guineafowl" # ?
  res[res == "jackleblackbacked"] <- "jackalblackbacked"
  res[res == "helmetedguineafowl"] <- "guineafowlhelmeted"
  res[res == "koribustard"] <- "bustardkori"
  res[res == "chacmababoon"] <- "baboon"
  res[res == "noanimalpresent"] <- "blank"
  res[res == "people"] <- "human"
  res[res == "plainszebra"] <- "zebraplains"
  res[res == "rabbitsp"] <- "rabbit"
  res[res == "redhartebeest"] <- "hartebeestred"
  res[res == "reptiles|amphibians"] <- "reptilesamphibians"
  res[res == "roanantelope"] <- "roan"
  res[res == "sharpe'sgrysbok"] <- "grysboksharpes"
  res[res == "smallspottedgenet"] <- "genetcommonsmallspotted"
  res[res == "slendermongoose"] <- "mongooseslender"
  res[res == "spottedhyena"] <- "hyenaspotted"
  res[res == "vervetmonkey"] <- "monkeyvervet"
  res[res == "vegetation"] <- "blank" # ?
  res[res == "unidentified"] <- "unresolvable"
  res[res == "wildebeestsp"] <- "wildebeest"
  res[res == "whitetailedmongoose"] <- "mongoosewhitetailed"
  res[res == "zebrasp"] <- "zebra"
  
  # Not sure what to do with:
  # 0
  # 1
  # knockeddown
  # mongoosemellers (is it a species?)
  # rabbit|hare -> merge with rabbit (was rabbitsp before)?...
  # sundryother
  # vehicles/humans/livestock
  # guineafowlhelmeted = guineafowl?
  # guineafowlcrested = other guineafowls?
  # zebraplains = zebraburchells?
  # researchteam
  # reptile = reptilesambhibians?
  
  return(res)
}

