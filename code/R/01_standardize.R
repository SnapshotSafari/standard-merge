# Libraries etc -----------------------------------------------------------
library(here)
library(log4r)

here::i_am("code/R/01_standardize.R") # Set working directory

source(here("code/R/functions/functions_standardize.R"))
source(here("code/R/functions/functions_files_handling.R"))

# Parameters --------------------------------------------------------------
# Where is the general folder in which you want to read your data ?
IN_DATADIR <- "/home/lnicvert/Documents/PhD/Code/Snapshot_cleaning/data/1_raw_data/"
# This path will not be copied into the destination.

# Where do you want to copy your files?
OUT_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data/"

# File or folder you actually want to copy, within IN_DATADIR and to OUT_DATADIR.
# If this has a subfolder structure, it will be copied into OUT_DATADIR.
# Must be an absolute path.

# input <- IN_DATADIR # Here we standardize all files in IN_DATADIR
input <- file.path(IN_DATADIR, "KHO") # Here we only standardize files in "KHO"
input <- file.path(IN_DATADIR,
                   "APN/APN_S1_full_report_0-50__agreement_corrected_fin.csv") # Here we only standardize first season of APN

# Files or folders in IN_DATADIR that should be ignored.
to_ignore <- c("reports_FBIP_format_all_recs",
               "README_FILES_for_TEAMS.txt",
               "Special Projects",
               "DHP/DHP+OVE_same_file",
               "KGA/KGA-KHO_together")

logfolder <- here("code/log")

# Logfile -----------------------------------------------------------------
if(!dir.exists(logfolder)) {
  dir.create(logfolder)
}

logfile_name <- get_logfile_name(input, IN_DATADIR)

my_logfile <- file.path(logfolder, logfile_name)

file.create(my_logfile)

my_console_appender = console_appender(layout = default_log_layout())
my_file_appender = file_appender(my_logfile, append = TRUE, 
                                 layout = default_log_layout())

my_logger <- log4r::logger(threshold = "INFO", 
                           appenders = list(my_console_appender, 
                                            my_file_appender))

# Guess folder or file ----------------------------------------------------
# If the input has an extension in the end, it is a file
is_file <- grepl("\\..+$", input)

# Log parameters ----------------------------------------------------------
# Get relative path to input from IN_DATADIR
rel_input <- get_relative_path(input, IN_DATADIR) 

msg <- paste0("Parameters ==============================\n", 
              "Input ", ifelse(is_file, "file: ", "folder: "), rel_input, 
              "\nIgnored: ", paste(to_ignore, collapse = ", "),
              "\n")
log4r::info(my_logger, 
            msg)

# List files --------------------------------------------------------------
if(!is_file) { # If input is a folder
  # Set folder to input
  folder <- input 
  in_files_list <- list_files_in_folder(folder, 
                                        except = to_ignore)
  
  in_files_list <- sort(in_files_list)
} else { 
  # in_files_list is set to the file (without the path)
  in_files_list <- basename(input) 
  
  # Folder is the rest
  folder <- dirname(input)
}

msg <- paste0("Files ==============================\n", 
              length(in_files_list), " file(s) to standardize.\n")
log4r::info(my_logger, 
            msg)

# Read standard -----------------------------------------------------------
# Read the column names file
STANDARD <- read.csv(here("docs/standard.csv"),
                     na.strings = "")

# Loop files --------------------------------------------------------------
for(in_filename in in_files_list) {
  log4r::info(my_logger, 
              paste0("File ", in_filename, " ------------"))
  
  # Read input file ---------------------------------------------------------
  dat_in <- read_file(in_filename, 
                      base_folder = folder)
  
  log4r::info(my_logger,
              paste("Initial data:", ncol(dat_in), "columns."))
  
  # Guess classifier --------------------------------------------------------
  if ("classifier" %in% colnames(dat_in)) { # If the classifier is given in the data
    classifier <- unique(dat_in$classifier)
    if(length(classifier) != 1) {
      stop(paste("Column classifier returned non-unique value:",
           paste(classifier, sep = ", ")))
    }
  } else { # classifier unknown
    classifier <- NA
  }

  colnames_dat_in <- colnames(dat_in)
  
  if(is.na(classifier)) {
    classifier <- guess_classifier(colnames_dat_in)
  }
  
  log4r::info(my_logger,
              paste0("Classifier set to ", classifier))

  # Prepare data for standardization ----------------------------------------
  if(classifier == "traptagger") {
    dat_in <- prepare_traptagger(dat_in)
  } else if (classifier == "digikam") {
    dat_in <- prepare_digikam(dat_in)
  }
  
  # Standardize column names ------------------------------------------------
  out <- capture.output(
    std_dat <- standardize_columns(dat_in, 
                                   classifier = classifier,
                                   standard_colnames = STANDARD),
    type = "message")
  
  if(length(out) != 0){
    log4r::warn(my_logger, 
                paste("Message in standardize_columns:", out))
  }
  
  # Add classifier -------------------------------------------------------------
  std_dat$classifier <- classifier
  
  # Standardize date/times --------------------------------------------------
  std_dat <- std_dat %>% mutate(date = standardize_date(date))
  std_dat <- std_dat %>% mutate(time = standardize_time(time))
  
  # Order -------------------------------------------------------------------
  std_dat <- std_dat %>% arrange(cam_site, date, time)
  
  # Fill capture info -------------------------------------------------------
  
  # Depending on the classifier, different information must be filled
  if(classifier == "digikam") {
    # --- First add location field
    filename_contains_location <- basename(in_filename)
    
    # Case Marinmane
    if(filename_contains_location == "Marinmane NR _record_table_0min_deltaT_2021-06-10.csv") {
      # Manually set location
      log4r::info(my_logger,
                  "Manually setting location code for Marinmane.")
      location_code <- "MAR"
    }else { # Set location name as first set of majuscule letters of the file
      location_code <- str_extract(filename_contains_location, "^[A-Z]+")
    }
    std_dat$location_code <- location_code
    
    # --- Fill other info
    std_dat <- fill_capture_info_digikam(std_dat)
    
  } else if (classifier == "traptagger") {
    std_dat <- fill_capture_info_traptagger(std_dat)
    
  } else if (classifier == "zooniverse") { # Only add location_code and change season
    # Set location to first set of majuscules letters in capture_id
    location_code <- str_extract(std_dat$capture_id, "^[A-Z]+")
    
    unique_location_code <- unique(location_code)
    if(length(unique_location_code) != 1) {
      log4r::warn(my_logger,
                  paste("Location extracted from capture_id is not unique: ", 
                        paste(location_code, collapse = ", ")))
    }
    std_dat$location_code <- location_code
    
    # --- Season
    season <- str_match(std_dat$season, "^.*_S(.*)$")[,2] # Get second match
    
    std_dat$season <- season
  }
  
  # Modify cam_site ---------------------------------------------------------
  already_dash <- which(grepl(std_dat$cam_site, pattern = "_"))
  
  if (length(already_dash) != 0) {
    cam_prob <- unique(std_dat$cam_site[already_dash])
    
    msg <- paste0("Cameras ",
                  paste(cam_prob, collapse = ", "),
                  " already have a dash in it: adding the location before as RES_... might be a problem")
    
    log4r::warn(my_logger,
                msg)
  }
  std_dat$cam_site <- paste(std_dat$location_code, 
                            std_dat$cam_site,
                            sep = "_")

  # Add capture_id ----------------------------------------------------------
  capture_id <- get_capture_id(season = std_dat$season,
                               cam_site = std_dat$cam_site, 
                               roll = std_dat$roll, 
                               capture = std_dat$capture)
  
  if (classifier == "zooniverse") {
    expected_capture_id <- str_match(std_dat$capture_id, 
                                     "^.*_S(.*)$")[,2]
    if (!all(capture_id == expected_capture_id)) { # if there are differences
      log4r::warn(my_logger,
                  "Some capture_id in the initial data do not correspond to the collated columns")
    }
  }
  
  std_dat$capture_id <- capture_id


  # Standardize species names -----------------------------------------------
  std_dat <- std_dat %>%
    mutate(common_name = standardize_species(common_name))
  
  # Reorder -----------------------------------------------------------------
  # Reorder columns to be sure
  std_dat <- std_dat %>% select(all_of(STANDARD$new[!is.na(STANDARD$new)]))
  
  # Previsualise final data -------------------------------------------------
  log4r::info(my_logger,
              paste("Final data:", ncol(std_dat), " columns.",
                    "\nHere are some columns:"))
  (std_cat <- head(std_dat[, 1:8], n = 5))
  
  suppressWarnings({
    write.table(std_cat,
                file = my_logfile,
                quote = FALSE,
                sep = "\t",
                append = TRUE)
  })
  write("\n",
        file = my_logfile,
        append = TRUE)

  # Write data --------------------------------------------------------------
  full_file <- file.path(folder, in_filename)
  file_from_datadir <- get_relative_path(full_file, wd = IN_DATADIR)
  
  # Write data --------------------------------------------------------------
  
  write_standardized_file(std_dat, file_from_datadir, to = OUT_DATADIR)

}

log4r::info(my_logger, 
            "Code successfully executed yeah!")
