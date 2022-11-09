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
IN_DATADIR <- "/home/lnicvert/Documents/PhD/Code/Snapshot_cleaning/data/1_raw_data/"

# --- Where wou want to copy data
# Where do you want to copy your files?
OUT_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data/"

# --- Data to standardize
# File or folder you actually want to copy, within IN_DATADIR and to OUT_DATADIR.
# If this has a subfolder structure, it will be copied into OUT_DATADIR.
input <- IN_DATADIR # Here we standardize all files in IN_DATADIR

# input <- file.path(IN_DATADIR, "roaming") # Here we only standardize files in "KHO"
# input <- file.path(IN_DATADIR,
#                    "APN/APN_S1_full_report_0-50__agreement_corrected_fin.csv") # Here we only standardize first season of APN

# --- Any files/folders to ignore?
# Files or folders in IN_DATADIR that should be ignored.
# All files/folders starting with these will be ignored.
to_ignore <- c("reports_FBIP_format_all_recs",
               "README_FILES_for_TEAMS.txt",
               "Special Projects",
               "DHP/DHP+OVE_same_file",
               "KGA/KGA-KHO_together")

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
# Get relative path to input from IN_DATADIR
rel_input <- get_relative_path(input, IN_DATADIR)

# Guess folder or file (if the input has an extension in the end, it is a file)
is_file <- grepl("\\..+$", input)

msg <- paste0("Parameters ==============================\n", 
              "Input ", ifelse(is_file, "file: ", "folder: "), rel_input, 
              "\nIgnored: ", paste(to_ignore, collapse = ", "),
              "\n")

write_log_message(msg, logger = my_logger)

# List files --------------------------------------------------------------
l <- get_files_and_folder(input, to_ignore)

folder <- l$folder
in_files_list <- l$files

msg <- paste0("Files ==============================\n", 
              length(in_files_list), " file(s) to standardize.\n")

write_log_message(msg, logger = my_logger)


# Loop files --------------------------------------------------------------
for(in_filename in in_files_list) {
  msg <- paste0("File ", in_filename, " ------------")
  write_log_message(msg, logger = my_logger)
  
  # Read input file ---------------------------------------------------------
  dat_in <- read_file(in_filename, 
                      base_folder = folder)
  
  msg <- paste("Initial data:", ncol(dat_in), "columns.")
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
  std_dat <- std_dat %>% mutate(date = standardize_date(date))
  std_dat <- std_dat %>% mutate(time = standardize_time(time))
  

  # Get location code (Digikam) ---------------------------------------------
  if(classifier == "digikam") { # Get location_code from filename
    # --- First add location field
    fname <- basename(in_filename)
    
    if(fname == "Marinmane NR _record_table_0min_deltaT_2021-06-10.csv") {
      # Manually set location
      location_code <- "MAR"
      
      msg <- "Manually setting location code for Marinmane."
      write_log_message(msg, logger = my_logger)
    } else {
      location_code <- str_extract(fname, "^[A-Z]+")
    }
    # Add location code
    std_dat$location_code <- location_code
  }
  
  # Fill capture info -------------------------------------------------------
  std_dat <- fill_capture_info(std_dat, classifier)
  
  # Add classifier -------------------------------------------------------------
  std_dat$classifier <- classifier
  
  # Standardize species names -----------------------------------------------
  std_dat <- std_dat %>%
    mutate(common_name = standardize_species(common_name))
  
  # Reorder -----------------------------------------------------------------
  # Reorder columns (to be sure)
  std_dat <- std_dat %>% select(all_of(STANDARD$new[!is.na(STANDARD$new)]))
  
  # Reorder data
  std_dat <- std_dat %>% arrange(cam_site, date, time)
  
  # Previsualize final data -------------------------------------------------
  msg <- paste("Final data:", ncol(std_dat), " columns.",
               "\nHere are some columns:")
  
  write_log_message(msg, logger = my_logger)
  (std_cat <- head(std_dat[, 1:8], n = 5))
  
  # Write head of result to logfile if there is a logfile
  if(create_log) {
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
  }
  
  # Write data --------------------------------------------------------------
  full_file <- file.path(folder, in_filename)
  file_from_datadir <- get_relative_path(full_file, wd = IN_DATADIR)
  
  write_standardized_file(std_dat, file_from_datadir, to = OUT_DATADIR)

}

msg <- "Code successfully executed yeah!"
write_log_message(msg, logger = my_logger)