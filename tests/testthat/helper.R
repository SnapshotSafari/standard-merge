# Library
library(here)

# Get standard
data(standard)

# Load tests datasets
dat_zooniverse <- read.csv("/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/APN/APN_S1_full_report_0-50__agreement_corrected_fin.csv")
dat_digikam <- read.csv("/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/roaming/MOK_record_table_0min_deltaT_2021-05-07.csv")
dat_traptagger <- read.csv("/home/lnicvert/Documents/PhD/Snapshot/data/1_raw_data/ATH/ATH_Roll1_Snapshot.csv")

goal_names <- standard$new[!is.na(standard$new)]