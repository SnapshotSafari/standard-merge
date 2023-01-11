# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-22
#
# Script Description: script to merge all data


# Libraries etc -----------------------------------------------------------
library(magrittr)
library(dplyr)


# Input folder ------------------------------------------------------------
IN_DATADIR <- "/home/lnicvert/Documents/PhD/Snapshot/data/02_standardized_data"


# List files --------------------------------------------------------------
files <- list.files(IN_DATADIR, recursive = TRUE,
                    full.names = TRUE)

# Merge files -------------------------------------------------------------
df_all <- do.call('rbind', lapply(files, read.csv))


# Get species names -------------------------------------------------------
sort(unique(df_all$snapshotName))

# Remove 0, blanks and the like
df_all_nozero <- df_all %>% 
  filter(!(snapshotName %in% c("0", "blank", "knockeddown")))

# Write final files --------------------------------------------------------
write.csv(df_all, 
          file.path(IN_DATADIR, "alldata.csv"), row.names = FALSE)

write.csv(df_all_nozero, 
          file.path(IN_DATADIR, "alldata_nozero.csv"), row.names = FALSE)
