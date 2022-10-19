# Libraries etc -----------------------------------------------------------
library(here)
here::i_am("code/R/02_merge_files.R") # Set working directory

source(here("code/R/functions/functions_standardize.R"))
source(here("code/R/functions/functions_files_handling.R"))

# Parameters --------------------------------------------------------------
readfolder <- here("data/2_standardized_data")


# Get all files -----------------------------------------------------------
datnames <- list.files(readfolder, recursive = TRUE,
                       full.names = FALSE)
datnames <- sort(datnames)


# Read all files into a list ----------------------------------------------
datlist <- list()
for(i in 1:length(datnames)){
  # Get file name
  dn <- datnames[i]
  
  print("-------------------------------------------")
  print(paste("File:", dn))
  print("-------------------------------------------")
  
  # Read file
  d <- read.csv(file.path(readfolder, dn),
                stringsAsFactors = FALSE, 
                colClasses = rep("character", 27))
  
  datlist <- c(datlist, list(d))
  
  names(datlist)[i] <- gsub(pattern = "\\.csv$",
                            replacement = "",
                            basename(dn))
}

# Merge list --------------------------------------------------------------
alldata <- do.call(bind_rows, datlist)


# Check data --------------------------------------------------------------
alldata[is.na(alldata$capture_id), c("season", "cam_site", "classifier")]

alldata[is.na(alldata$date), c("season", "cam_site", "classifier")]
alldata[is.na(alldata$time), c("season", "cam_site", "classifier")]


sp <- alldata$common_name
(unique_sp <- sort(unique(sp)))

write.csv(alldata, here("data/2_standardized_data/alldata.csv"))


