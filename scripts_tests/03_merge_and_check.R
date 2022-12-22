# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-22
#
# Script Description: script to merge all data

IN_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data"


# List files --------------------------------------------------------------
files <- list.files(IN_DATADIR, recursive = TRUE,
                    full.names = TRUE)

# Merge files -------------------------------------------------------------
df_all <- do.call('rbind', lapply(files, read.csv))


# Get species names -------------------------------------------------------
sort(unique(df_all$snapshotName))

# Remove 0?
