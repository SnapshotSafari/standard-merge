# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2023-01-11
#
# Script Description: script to check and amend metadata content

# Libraries etc -----------------------------------------------------------
library(standardizeSnapshot)

library(here)
library(magrittr)
library(dplyr)
library(stringr)

# Plot map
library(sf)
library(ggplot2)
library(ggsn)
library(gridExtra)

# Read data ---------------------------------------------------------------
metadata <- read.csv("/home/lnicvert/Documents/PhD/Snapshot/metadata/02_raw_data_csv/+1_Metadata_all_fixed_as_snapshot.csv")
# alldata <- read.csv("/home/lnicvert/Documents/PhD/Snapshot/data/02_standardized_data/alldata.csv")


# Reduce data size --------------------------------------------------------
# alldata <- alldata %>%
#   select(locationID, cameraID, roll, season, classifier)
# alldata <- alldata %>% distinct() %>%
#   arrange(locationID, cameraID, roll)
# write.csv(alldata, here("scripts_tests/alldata_test.csv"), row.names = FALSE)

# Use the reduced size file
alldata <- read.csv(here("scripts_tests/alldata_test.csv"))

# Remove spaces ---------------------------------------------------------
metadata <- standardizeSnapshot:::clean_blanks(metadata)

# Amend cameras not in metadata -------------------------------------------
# (Useless if data was cleaned in a proper way)

# alldata <- alldata %>%
#   mutate(locationID = clean_locations(cameraID, locationID))
# 
# alldata <- alldata %>%
#   mutate(cameraID = clean_cameras(cameras = cameraID, 
#                                   locations = locationID,
#                                   classifiers = classifier))

# Amend metadata cameras and locations ------------------------------------
# Generic amendments
metadata <- metadata %>%
  mutate(locationID = clean_locations(Site.Original, Code.Loc.Snap),
         .after = Code.Loc.Snap)

metadata <- metadata %>%
  mutate(cameraID = clean_cameras(cameras = Site.Original, 
                                  locations = locationID),
         .after = Site.Original)

# Remove prefix temporarily
metadata <- metadata %>% 
  mutate(cameraID = get_camnames(cameraID, locationID))

# Change second OVE_F01 to F02
index <- which(metadata$cameraID == "F01" & metadata$locationID == "OVE")
metadata[max(index), "cameraID"] <- "F02"

# Correct KHO (replace G... -> E... and L.. -> M...)
metadata <- metadata %>% 
  mutate(cameraID = ifelse(locationID == "KHO", 
                           gsub("^G", "E", cameraID), cameraID)) %>%
  mutate(cameraID = ifelse(locationID == "KHO", 
                           gsub("^L", "M", cameraID), cameraID))

# Correct SAM (remplace B5_... -> B... and ... -> S...)
# I checked in the data, the SAM_B... sites correspond
# to camera names wih B5_ in the metadata (camera names without prefix are unique)
metadata <- metadata %>% 
  # Add S if there is no B5_
  mutate(cameraID = ifelse(locationID == "SAM" & !grepl("^B5_", cameraID), 
                           paste0("S", cameraID), cameraID)) %>%
  # Replace B5_ with B
  mutate(cameraID = ifelse(locationID == "SAM", 
                           gsub("^B5_", "B", cameraID), cameraID))
  

# Add prefix again
metadata <- metadata %>% 
  mutate(cameraID = add_location_prefix(cameraID, locationID))

# Ambiguous ID in metadata ------------------------------------------------
# Check if camera_ID could be ambiguous in metadata (2 cameras have the same name)
ambiguous <- length(sort(unique(metadata$cameraID))) != length(sort(metadata$cameraID))
ambiguous_cam <- metadata %>% select(cameraID) %>% 
  group_by(cameraID) %>% summarise(n = n()) %>% filter(n>1)

message(paste0("Camera ID is ", ifelse(ambiguous, "", "not "), "ambiguous."))

if(ambiguous){
  cam_ambiguous <- ambiguous_cam$cameraID
  message("camera ", cam_ambiguous, " is present twice.")
  caminfo <- metadata %>% filter(cameraID == cam_ambiguous) %>%
    select(cameraID, 
           locationID, Sector,
           Site.Original, Camera.Site.Concatenate,
           Long_X, Lat_Y, Setup.Date.Original)
  print(caminfo)
}



# Get list of cameras -----------------------------------------------------

## In the data -----
# Filter out data that is not in the metadata at all
# (we don't want to display those)
alldata_test <- alldata %>%
  filter(!(locationID %in% c("SP", "GON")))
# Cameras that are in data
cam_data <- sort(unique(alldata_test$cameraID))

## In the metadata -----
# Filter out metadata that is not in the data
metadata_test <- metadata %>%
  filter(locationID !="LAN")
# Cameras that are in metadata
cam_metadata <- sort(unique(metadata_test$cameraID))

# Get cameras that are not in the data (not a big deal) -------------------
not_in_data <- cam_metadata[!(cam_metadata %in% cam_data)]
message(paste(length(not_in_data), 
              "cameras are in the metadata but not in the data: ",
              paste(not_in_data, collapse = ", "))
)
# APN_BosP, APN_G2, APN_G3, APN_JKB1, APN_KAP1, APN_KAP10, APN_KAP15, 
#   APN_KAP16, APN_KAP18, APN_KAP3, APN_KAP8, APN_SV3?
# AUG_B02, AUG_B03, AUG_B04, AUG_B06, AUG_C01, AUG_C04, 
#   AUG_C05, AUG_C06, AUG_C07, AUG_C08, AUG_D04, AUG_D07, AUG_D08, 
#   AUG_D09, AUG_D12?
# BLO_C01, BLO_D07
# GAR cameras -> in the metadata they are GAR_xxxold and GAR_xxxnew
# KAR_A02b?
# MAR_A08, MAR_B07, MAR_B08, MAR_C07?
# SOM_A1, SOM_C2, SOM_C3, SOM_D3, SOM_D4, 
#   SOM_E1, SOM_E2, SOM_E3, SOM_E4, SOM_F1, SOM_F2?
# VEN_C03, VEN_D03?
# + all cameras from LAN

# Get cameras that are not in the metadata (important) --------------------
not_in_metadata <- cam_data[!(cam_data %in% cam_metadata)]
message(paste(length(not_in_metadata), 
              "cameras are in the data but not in the metadata: ",
              paste(not_in_metadata, collapse = ", "))
        )
# APN_Jack1, APN_JJP, APN_WS5, APN_WS6?
# GAR cameras -> in the metadata they are GAR_xxxold and GAR_xxxnew
# KAR_K06?
# KGA_C01, KGA_C02, KGA_C03, KGA_C04, KGA_C05, KGA_C06, 
#   KGA_C07, KGA_C08, KGA_C09, KGA_C10, KGA_D01, KGA_D02, 
#   KGA_D03, KGA_D04, KGA_D05, KGA_D06, KGA_D07, KGA_D08, KGA_D09, KGA_D10 ?
# KHO_EW1, KHO_EW2, KHO_EW3, KHO_EW4?
# MAD_A08, MAD_D02, MAD_E02, MAD_E03, MAD_E05, MAD_E07_2, MAD_F09, 
#   MAD_F10, MAD_G09, MAD_G10?
# MTZ_E08?
# Tswalu -> because of TSW_Roll6_Snapshot (missing sector prefix)
# + all cameras from SP (special projects) and GON

# Plot coordinates -------------------------------------------------------
metadata_plot_prep <- metadata %>% 
  select(Lat_Y, Long_X, 
         cameraID, Sector,
         locationID,
         Reserve.Location)

metadata_plot <- metadata_plot_prep %>% 
  filter(!(is.na(Lat_Y) | is.na(Long_X)))

message("There are ", nrow(metadata_plot_prep) - nrow(metadata_plot),
        " cameras with NA coordinates.")

metadata_plot <- st_as_sf(metadata_plot, 
                          coords = c("Long_X", "Lat_Y")) %>%
  st_set_crs("+proj=longlat +ellps=WGS84")

# Get unique locations
locations <- sort(unique(metadata_plot$locationID))

# Plot 
glist <- list()
for(loc in locations){
  # Select for one site
  subplot <- metadata_plot %>% filter(locationID == loc)
  reserve_name <- unique(subplot$Reserve.Location)
  
  # Get number of subreserves
  nsubreserves <- length(unique(subplot$Sector))
  
  subplot <- st_as_sf(subplot, 
                      coords = c("Long_X", "Lat_Y")) %>%
    st_set_crs("+proj=longlat +ellps=WGS84")
  
  if(nsubreserves == 1){ # no need for colours
    g <- ggplot(subplot) +
      geom_sf(size = 2)
  }else{
    g <- ggplot(subplot) +
      geom_sf(aes(col = Sector), size = 2) +
      theme(legend.title = element_blank(),
            legend.position = "bottom")
  }
  
  g <- g +
    ggsn::scalebar(data = subplot,
                   dist = 2,
                   dist_unit = "km", transform = TRUE,
                   model = "WGS84",
                   st.size = 3.5,
                   border.size = 0.2,
                   st.dist = .02) + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank()) +
    ggtitle(paste0(loc, " (", reserve_name, ")"))
  
  glist <- c(glist, list(g))
}

grid.arrange(grobs = glist, nrow = 4)


# Save metadata -----------------------------------------------------------
metadata_write <- metadata %>%
  select(-c("id", "Code.Loc.Snap", "Site.Original", "Camera.Site.Concatenate",
            "X", "X.1"))

write.csv(metadata_write,
          "/home/lnicvert/Documents/PhD/Snapshot/metadata/03_cleaned_metadata/metadata_snapshot.csv",
          row.names = FALSE)
