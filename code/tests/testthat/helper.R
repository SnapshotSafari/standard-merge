library(here)
here::i_am("code/tests/testthat/helper.R")

# Get standard
STANDARD <- read.csv(here("docs/standard.csv"),
                     na.strings = "")
goal_names <- STANDARD$new[!is.na(STANDARD$new)]

# Load tests datasets
dat_zooniverse <- read.csv(here("data/1_raw_data/APN/APN_S1_full_report_0-50__agreement_corrected_fin.csv"))
dat_digikam <- read.csv(here("data/1_raw_data/roaming/MOK_record_table_0min_deltaT_2021-05-07.csv"))
dat_traptagger <- read.csv(here("data/1_raw_data/ATH/ATH_Roll1_Snapshot_UPDATED.csv"))
