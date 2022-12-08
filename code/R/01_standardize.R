# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-10-20
#
# Script Description: Standardizes the data chosen to the standard format.

# Libraries etc -----------------------------------------------------------
library(here)
library(log4r)

here::i_am("code/R/01_standardize.R") # Set working directory
# here() starts at the project's root

source(here("code/R/functions/functions_standardize.R"))
source(here("code/R/functions/functions_files_handling.R"))

# Parameters --------------------------------------------------------------

# --- Path to the data
# Where is the general folder in which you want to read your data ?
# This path will not be copied into the destination.
IN_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data"

# --- Where wou want to copy data
# Where do you want to copy your files?
OUT_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data"
# OUT_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/test_new"

# --- Data to standardize
# File or folder you actually want to copy, within IN_DATADIR and to OUT_DATADIR.
# If this has a subfolder structure, it will be copied into OUT_DATADIR.

input <- IN_DATADIR # Here we standardize all files in IN_DATADIR
# input <- c(file.path(IN_DATADIR, "MAD/MAD_Roll15_Snapshot.csv"),
#            file.path(IN_DATADIR, "MAD/MAD_S1_full_report_0-50__agreement_corrected_fin.csv"),
#            file.path(IN_DATADIR, "roaming/TAN_sp_report_digikam_2021-02-24_fin.csv"))


# --- Any files/folders to ignore?
# Files or folders in IN_DATADIR that should be ignored.
# You can use "*" to match any character of any length (possibly empty)
to_ignore <- c("reports_FBIP_format_all_recs/*",
               "README_FILES_for_TEAMS.txt",
               # "Special Projects /*",
               "DHP/DHP+OVE_same_file/*",
               "KGA/KGA-KHO_together/*")

# --- Log
# Do you want to create a logfile in code/log (TRUE) or not (FALSE)?
create_log <- TRUE


# Read standard column names ----------------------------------------------
# Read the  file
STANDARD <- read.csv(here("docs/standard.csv"),
                     na.strings = "")


# Setup logfile -----------------------------------------------------------
logfolder <- here("code/log")

if(create_log) {
  logfile_name <- get_logfile_name() # Get automated name for the logger
  my_logfile <- file.path(logfolder, logfile_name) # Get logger path
  my_logger <- create_logger(my_logfile) # Create logger
} else {
  my_logger <- NA
}

# Log parameters ----------------------------------------------------------
msg <- paste0("Parameters ==============================\n", 
              "Input: ", paste(input, collapse = ", "), 
              "\nIgnored: ", paste(to_ignore, collapse = ", "),
              "\n")

write_log_message(msg, logger = my_logger)

# List files --------------------------------------------------------------
files_df <- get_files_and_folder(input, 
                                 except = to_ignore)

msg <- paste0("Files ==============================\n", 
              nrow(files_df), " file(s) to standardize.\n")

write_log_message(msg, logger = my_logger)


# Loop files --------------------------------------------------------------
for(i in 1:nrow(files_df)) {
  # Get file and folder ---------------------------------------------------------
  in_filename <- files_df$files[i]
  folder <- files_df$folders[i]
  
  msg <- paste0("File ", in_filename, " ------------")
  write_log_message(msg, logger = my_logger)
  
  # Read input file ---------------------------------------------------------
  dat_in <- read_file(in_filename, 
                      base_folder = folder)
  
  nrow_init <- nrow(dat_in)
  msg <- paste("Initial data:", 
               ncol(dat_in), "columns and",
               nrow_init, "rows.")
  write_log_message(msg, logger = my_logger)
  
  # Guess classifier --------------------------------------------------------
  colnames_dat_in <- colnames(dat_in)
  classifier <- guess_classifier(colnames_dat_in)

  msg <- paste0("Classifier set to ", classifier)
  write_log_message(msg, logger = my_logger)
  
  # Standardize column names ------------------------------------------------
  out <- capture.output(
    std_dat <- standardize_columns(dat_in, 
                                   classifier = classifier,
                                   standard_colnames = STANDARD),
    type = "message")
  
  if(length(out) != 0){
    msg <- paste("Message in standardize_columns:", out)
    write_log_message(msg, logger = my_logger)
  }
  
  # Standardize date/times --------------------------------------------------
  std_dat <- std_dat %>% mutate(eventDate = standardize_date(eventDate))
  std_dat <- std_dat %>% mutate(eventTime = standardize_time(eventTime))
  

  # Get location code (Digikam) ---------------------------------------------
  if(classifier == "digikam") { # Get locationID from filename
    # --- First add location field
    fname <- basename(in_filename)
    
    if(fname == "Marinmane NR _record_table_0min_deltaT_2021-06-10.csv") {
      # Manually set location
      locationID <- "MAR"
      
      msg <- "Manually setting location code for Marinmane."
      write_log_message(msg, logger = my_logger)
    } else {
      locationID <- str_extract(fname, "^[A-Z]+")
    }
    # Add location code
    std_dat$locationID <- locationID
  }
  
  # Fill capture info -------------------------------------------------------
  
  out2 <- capture.output(
    std_dat <- fill_capture_info(std_dat, classifier),
    type = "message")
  
  if(length(out2) != 0){
    msg <- paste("Message in fill_capture_info:", out2)
    write_log_message(msg, logger = my_logger)
  }
  
  # Add classifier -------------------------------------------------------------
  std_dat$classifier <- classifier
  
  # Standardize species names -----------------------------------------------
  # std_dat <- std_dat %>%
  #   mutate(snapshotName = standardize_species(snapshotName))
  
  # Reorder -----------------------------------------------------------------
  # Reorder columns (to be sure)
  to_reorder <- STANDARD$new[!is.na(STANDARD$new)]
  std_dat <- std_dat %>% select(all_of(to_reorder))
  

  # Remove capture column ---------------------------------------------------
  std_dat <- std_dat %>% select(-capture)

  # Reorder data
  std_dat <- std_dat %>% arrange(cameraID, eventDate, eventTime)
  
  # Previsualize final data -------------------------------------------------
  nrow_fin <- nrow(std_dat)
  msg <- paste("Final data:", ncol(std_dat), "columns,", 
               nrow_fin, "rows.",
               "\nHere are some columns:")
  
  if(nrow_init != nrow_fin) {
    msg <- paste("Different number of rows: initially", nrow_init, "rows,",
                 "final data has", nrow_fin, "rows.")
    write_log_message(msg, logger = my_logger,
                      level = "warning")
  }
  write_log_message(msg, logger = my_logger)
  (std_cat <- head(std_dat[, 1:8], 
                   n = min(nrow(std_dat), 5)))
  
  # Write head of result to logfile if there is a logfile
  if(create_log) {
    suppressWarnings({
      write.table(std_cat,
                  file = my_logfile,
                  quote = FALSE,
                  sep = "\t",
                  append = TRUE)
    })
  }
  
  # Write data --------------------------------------------------------------
  full_file <- file.path(folder, in_filename)
  file_from_datadir <- get_relative_path(full_file, wd = IN_DATADIR)
  
  filepath <- write_standardized_file(std_dat, 
                                      file_from_datadir, 
                                      to = OUT_DATADIR)
  
  filename <- basename(filepath)
  dir <- dirname(filepath)
  
  msg <- paste("File", filename, "successfully created into", dir)
  write_log_message(msg, logger = my_logger)
  
  if(create_log) {
    write("\n",
          file = my_logfile,
          append = TRUE)
  }
}

msg <- "Code successfully executed yeah!"
write_log_message(msg, logger = my_logger)
