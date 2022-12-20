# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-12
#
# Script Description: script to test standardizeSnapshot package standardizing.

# Libraries etc -----------------------------------------------------------
library(standardizeSnapshot)

library(here)
library(magrittr)
library(dplyr)
library(stringr)

# Parameters --------------------------------------------------------------

# --- Path to the data
# Where is the general folder in which you want to read your data ?
# This path will not be copied into the destination.
IN_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data"

# --- Where wou want to copy data
# Where do you want to copy your files?
# OUT_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data"
OUT_DATADIR_FILE <- "/home/lnicvert/Documents/PhD/Snapshot/data/test_file"
OUT_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/test_clean"

# --- Data to standardize
# File or folder you actually want to copy, within IN_DATADIR and to OUT_DATADIR.
# If this has a subfolder structure, it will be copied into OUT_DATADIR.

input <- IN_DATADIR # Here we standardize all files in IN_DATADIR
# input_file <- file.path(IN_DATADIR, "APN/APN_S1_full_report_0-50__agreement_corrected_fin.csv")
input <- c(file.path(IN_DATADIR, "APN/APN_S1_full_report_0-50__agreement_corrected_fin.csv"),
           file.path(IN_DATADIR, "ATH/ATH_Roll1_Snapshot.csv"),
           file.path(IN_DATADIR, "roaming/AUG_sp_report_digikam_2020-08-26_fin.csv"),
           file.path(IN_DATADIR, "KHO/KHO_S1_full_report_0-50__agreement_corrected_fin.csv"),
           file.path(IN_DATADIR, "DHP"))

# --- Any files/folders to ignore?
# Files or folders in IN_DATADIR that should be ignored.
# You can use "*" to match any character of any length (possibly empty)
to_ignore <- c("reports_FBIP_format_all_recs",
               "README_FILES_for_TEAMS.txt",
               # "Special Projects /*",
               "DHP/DHP+OVE_same_file",
               "KGA/KGA-KHO_together")

logfile <- here("scripts_tests/log/log_test.log")

# Read standard column names ----------------------------------------------
# Read the  file
data(standard)


# Logging -----------------------------------------------------------------
logger_file <- create_logger(logfile)

# Read files --------------------------------------------------------------
std_list <- read_snapshot_files(input = input,
                                except = to_ignore,
                                basepath = IN_DATADIR)
std_df <- read.csv(input_file)

# Standardize files -------------------------------------------------------
std_list <- standardize_snapshot_list(std_list, 
                                      standard)
std_df <- standardize_snapshot_df(std_df,
                                  standard)


# Print head --------------------------------------------------------------
lapply(std_list, function(df) head(df[,1:8]))
head(std_df[,1:8])

# Write files -------------------------------------------------------------
write_standardized_list(df_list = std_list,
                        to = OUT_DATADIR,
                        write = TRUE,
                        verbose = TRUE)

write_standardized_df(df = std_df,
                      write = TRUE,
                      to = OUT_DATADIR_FILE)

