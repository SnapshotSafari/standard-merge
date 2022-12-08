# Libraries etc -----------------------------------------------------------
library(here)
here::i_am("code/R/02_merge_files.R") # Set working directory

source(here("code/R/functions/functions_standardize.R"))
source(here("code/R/functions/functions_files_handling.R"))

# Parameters --------------------------------------------------------------
readfolder <- "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data/"


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
                colClasses = rep("character", 26))
  
  datlist <- c(datlist, list(d))
  
  names(datlist)[i] <- gsub(pattern = "\\.csv$",
                            replacement = "",
                            basename(dn))
}

# Merge list --------------------------------------------------------------
alldata <- do.call(bind_rows, datlist)
rm(datlist)

# Check data --------------------------------------------------------------
alldata[is.na(alldata$eventID), c("locationID", "cameraID", "classifier")]

alldata[is.na(alldata$eventDate), c("locationID", "cameraID", "classifier")]
alldata[is.na(alldata$eventTime), c("locationID", "cameraID", "classifier")]


sp <- alldata$snapshotName
(unique_sp <- sort(unique(sp)))

source(here("code/R/functions/functions_standardize.R"))
spp_clean <- standardize_species(sp)
(unique_sp_clean <- sort(unique(spp_clean)))

write.csv(alldata, file.path(readfolder, "alldata.csv"))


