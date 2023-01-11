# Library
library(here)

# Get standard
data(standard)

# Raw data folder
raw_data_folder <- "/home/lnicvert/Documents/PhD/Snapshot/data/01_raw_data"

# Load tests datasets
dat_zooniverse <- read.csv(file.path(raw_data_folder,
                                     "APN/APN_S1_full_report_0-50__agreement_corrected_fin.csv"))
dat_digikam <- read.csv(file.path(raw_data_folder,
                                  "roaming/MOK_record_table_0min_deltaT_2021-05-07.csv"))
dat_traptagger <- read.csv(file.path(raw_data_folder,
                                     "ATH/ATH_Roll1_Snapshot.csv"))

goal_names <- standard$new[!is.na(standard$new)]