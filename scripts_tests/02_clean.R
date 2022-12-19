# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-12
#
# Script Description: script to test standardizeSnapshot package cleaning function.

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
OUT_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/test_clean"

# --- Data to standardize
# File or folder you actually want to copy, within IN_DATADIR and to OUT_DATADIR.
# If this has a subfolder structure, it will be copied into OUT_DATADIR.

# input <- IN_DATADIR # Here we standardize all files in IN_DATADIR
input <- c(file.path(IN_DATADIR, "KHO"),
           file.path(IN_DATADIR, "KGA"),
           file.path(IN_DATADIR, "DHP"),
           file.path(IN_DATADIR, "OVE"),
           file.path(IN_DATADIR, "SAM"),
           file.path(IN_DATADIR, "TSW"),
           file.path(IN_DATADIR, "APN"))

# --- Any files/folders to ignore?
# Files or folders in IN_DATADIR that should be ignored.
# You can use "*" to match any character of any length (possibly empty)
to_ignore <- c("DHP/DHP+OVE_same_file/*",
               "KGA/KGA-KHO_together/*")

# Read standard column names ----------------------------------------------
# Read the  file
data(standard)

# Read files --------------------------------------------------------------
std_list <- read_snapshot_files(input = input,
                                except = to_ignore,
                                basepath = IN_DATADIR)

# Standardize files -------------------------------------------------------------

## KHO (TrapTagger) ----------
KHO_ttagger <- std_list$`KHO/KHO_Roll10_Snapshot.csv`
std_KHO_ttager <- standardize_snapshot_df(KHO_ttagger,
                                          standard)
unique(std_KHO_ttager$cameraID)
unique(std_KHO_ttager$locationID)

## KHO (Zooniverse) ----------
KHO_tzoo <- std_list$`KHO/KHO_S1_full_report_0-50__agreement_corrected_fin.csv`
std_KHO_zoo <- standardize_snapshot_df(KHO_tzoo,
                                       standard)
unique(std_KHO_zoo$cameraID)
unique(std_KHO_zoo$locationID)

## SAM ----------
SAM <- std_list$`SAM/SAM_Roll1_Snapshot.csv`
std_SAM <- standardize_snapshot_df(SAM,
                                   standard)
unique(std_SAM$cameraID)
unique(std_SAM$locationID)

## TSW (TrapTagger) ----------
TSW_ttagger <- std_list$`TSW/TSW_Roll5_Snapshot.csv`
std_TSW_ttagger <- standardize_snapshot_df(TSW_ttagger,
                                           standard)
unique(std_TSW_ttagger$cameraID)
unique(std_TSW_ttagger$locationID)

## TSW (Zooniverse) ----------
TSW_zoo <- std_list$`TSW/TSW_S1_full_report_0-50__agreement_corrected_fin.csv`
std_TSW_zoo <- standardize_snapshot_df(TSW_zoo,
                                       standard)
unique(std_TSW_zoo$cameraID)
unique(std_TSW_zoo$locationID)

## APN -----------
APN <- std_list$`APN/APN_S1_full_report_0-50__agreement_corrected_fin.csv`
std_APN <- standardize_snapshot_df(APN,
                                   standard)
unique(std_APN$cameraID)
unique(std_APN$locationID)

## KGA (Zooniverse) ----------
KGA_zoo <- std_list$`KGA/KGA_S1_full_report_0-50__agreement_corrected_fin.csv`
std_KGA_zoo <- standardize_snapshot_df(KGA_zoo,
                                       standard)
unique(std_KGA_zoo$cameraID)
unique(std_KGA_zoo$locationID)

## KGA (TrapTagger) ----------
KGA_ttagger <- std_list$`KGA/KGA_Roll6_Snapshot.csv`
std_KGA_ttagger <- standardize_snapshot_df(KGA_ttagger,
                                           standard)
unique(std_KGA_ttagger$cameraID)
unique(std_KGA_ttagger$locationID)

## DHP ---------
DHP_zoo <- std_list$`DHP/DHP_S1_full_report_0-50__agreement_corrected_fin.csv`
std_DHP_zoo <- standardize_snapshot_df(DHP_zoo,
                                       standard)
unique(std_DHP_zoo$cameraID)
unique(std_DHP_zoo$locationID)

## OVE (Zooniverse) ----------
OVE_zoo <- std_list$`OVE/OVE_S1_full_report_0-50__agreement_corrected_fin.csv`
std_OVE_zoo <- standardize_snapshot_df(OVE_zoo,
                                       standard)
unique(std_OVE_zoo$cameraID)
unique(std_OVE_zoo$locationID)

## OVE (TrapTagger) ----------
OVE_ttagger <- std_list$`OVE/OVE_Roll3_Snapshot.csv`
std_OVE_ttagger <- standardize_snapshot_df(OVE_ttagger,
                                           standard)
unique(std_OVE_ttagger$cameraID)
unique(std_OVE_ttagger$locationID)
