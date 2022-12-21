# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-09
#
# Script Description: functions to clean data from Snapshot files


# Cameras -----------------------------------------------------------------

#' Get camera names
#'
#' Subsets the locations IDs from the camera names vector.
#' 
#' @param cameras a character vector of camera names
#' @param locations a character vector of locations (must be the same length as `cameras`)
#' @param silence_warnings Should a warning be displayed when `locationID` is `NA`?
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#'
#' @return A vector of camera names without the location prefix (if it was present)
#' 
#' @export
#' 
#' @examples get_camnames(c("APN_A01", "MAD_B01"), c("APN", "MAD"))
get_camnames <- function(cameras, locations, 
                         silence_warnings = FALSE, logger = NA) {
  
  if ( any(is.na(locations))) {
    if (!silence_warnings) {
      msg <- "Some locations are NA, so camnames might be incorrect if they contain an underscore"
      write_log_message(msg, logger = logger, level = "warn")
      warning(msg)
    }
  }
  
  sapply(1:length(cameras), 
         function(i) {
           get_camname(cameras[i], locations[i])
         })
}

#' Clean locations and cameras
#'
#' Cleans locations and cameras for a dataframe
#' 
#' @param df a dataframe that must have columns `cameraID`, `locationID` and `classifier`.
#' @param camera should `cameraID` column be cleaned?
#' @param location should `locationID` column be cleaned?
#' @param silence_warnings Should a warning be displayed when `locationID` is `NA`?
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#'
#' @return The dataframe with cleaned columns `cameraID` and/or `locationID`
#' (depending whether `camera` and `location` are `TRUE`) and `eventID`
#' refactored to match new `cameraID`/`locationID`.
#' 
#' @details 
#' For column `locationID`:
#' 
#' + The location code `DHP` is replaced with `OVE` if the corresponding camera code starts with 'O'.
#' + The location code `KGA` is replaced with `KHO` if the corresponding camera code starts with 'KHO'.
#' 
#' For column `cameraID`:
#' 
#' + For TrapTagger data: will remove the leading location code part for all data 
#' (eg if `location` is `ATH`, will change `cameras` `ATH_A01` -> `A01`).
#' Also, if the location code is `KHO`, `SAM` or `TSW`: will remove the dash in 
#' the camera name (e.g `KHO_E_A01` -> `EA01`)
#' 
#' + For Zooniverse data: if the location code is `KHO`, will replace 
#' `KHOG` with `E` and  `KHOL` with `M` in `cameras`.
#' If the location code is `DHP`, will remove leading `D` in `cameraID`.
#' If the location code is `OVE`, will remove leading `O` in `cameraID`.
#' 
#' + For column `eventID`: the event ID formatted as season#cam_site#roll#event_no.
#' 
#' @noRd
clean_camera_location <- function(df, camera = TRUE, 
                                  location = TRUE,
                                  silence_warnings = FALSE,
                                  logger = NA) {
  
  # Initialize result
  clean_df <- df
  
  if (location) {
  clean_df <- clean_df %>%
    mutate(locationID = clean_locations(cameraID, 
                                        locationID,
                                        logger = logger))
  } 
  if (camera) {
    clean_df <- clean_df %>%
      mutate(cameraID = clean_cameras(cameraID, 
                                      locationID, 
                                      classifier,
                                      silence_warnings = silence_warnings,
                                      logger = logger))
  }
  
  clean_df <- clean_df %>%
    mutate(eventID = get_eventID(locationID, cameraID, roll, capture))
  
  return(clean_df)
}

# Species -----------------------------------------------------------------

#' Standardize species
#'
#' Eliminate species duplicate names (things like `birdofprey` and `birdsofprey`)
#' 
#' @param species vector of species names
#'
#' @return the vector of species names with standardized names, according to the
#' species names that have already been encountered in the past datasets.
#' 
#' @export
#' 
#' @examples
#' species <- c("zebraplains", "zebraburchells", "duiker", 
#'              "duikercommon", "aardvark", "lionfemale")
#' clean_species(species)
clean_species <- function(species){
  
  res <- species
  
  # --- Convert to character (case factor)
  res <- as.character(res)
  
  # --- Only lowercase
  res <- tolower(res)
  
  # --- Remove spaces
  res <- gsub(pattern = "\\s+", 
              replacement = "", 
              res)
  
  # --- Remove underscore
  res <- gsub(pattern = "_+", 
              replacement = "", 
              res)
  
  # --- Standardize
  ## Species
  res[res == "aardvark"] <- "aardvarkantbear"
  res[res == "africanwildcat"] <- "catafricanwild"
  res[res == "africanwilddog"] <- "wilddog"
  res[res == "antelopeunknown"] <- "antelope"
  res[res == "bandedmongoose"] <- "mongoosebanded"
  res[res == "batearedfox"] <- "foxbateared"
  res[res == "blackbackedjackal"] <- "jackalblackbacked"
  res[res == "bustardludwig's"] <- "bustardludwigs"
  res[res == "blackwildebeest"] <- "wildebeestblack"
  res[res == "bluewildebeest"] <- "wildebeestblue"
  res[res == "brownhyena"] <- "hyenabrown"
  res[res == "birdsofprey"] <- "birdofprey"
  res[res == "bird"] <- "birdother" # ?
  res[res == "capeporcupine"] <- "porcupinecape"
  res[res == "capemountainzebra"] <- "zebramountaincape"
  res[res == "catdomestic"] <- "domesticanimal"
  res[res == "chacmababoon"] <- "baboon"
  res[res == "commonduiker"] <- "duikercommon"
  res[res == "commonwarthog"] <- "warthog"
  res[res == "crestedguineafowl"] <- "guineafowlcrested" # ?
  res[res == "domesticcat"] <- "domesticanimal" # ?
  res[res == "domesticcow"] <- "domesticanimal" # ?
  res[res == "domesticdog"] <- "domesticanimal" # ?
  res[res == "domestichorse"] <- "domesticanimal" # ?
  res[res == "duiker"] <- "duikercommon"
  res[res == "duikercommongrey"] <- "duikercommon"
  res[res == "dwarfmongoose"] <- "mongoosedwarf"
  res[res == "feralcat"] <- "catferal" # ?
  res[res == "fowlguinea"] <- "guineafowl" # ?
  res[res == "gemsbokoryx"] <- "gemsbok"
  res[res == "genetlargespotted"] <- "genetcapelargespotted"
  res[res == "genet(small)"] <- "genetcommonsmallspotted"
  res[res == "genetcape"] <- "genetcapelargespotted"
  res[res == "genetsmallspotted"] <- "genetcommonsmallspotted"
  res[res == "galagosouthernlesser"] <- "bushbaby"
  res[res == "groundsquirrel"] <- "squirrelground"
  res[res == "grysbok"] <- "grysbokcape"
  res[res == "harecape"] <- "hare"
  res[res == "horse"] <- "domesticanimal" # ?
  res[res == "helmetedguineafowl"] <- "guineafowlhelmeted"
  res[res == "jackleblackbacked"] <- "jackalblackbacked"
  res[res == "mongoosewater"] <- "mongoosewatermarsh"
  res[res == "koribustard"] <- "bustardkori"
  res[res == "livestock"] <- "domesticanimal"
  res[res == "mongoosecapesmallgrey"] <- "mongoosesmallcapegrey"
  res[res == "mongooseegyptianlargegrey"] <- "mongooselargegrey"
  res[res == "mongoose(whitetailed)"] <- "mongoosewhitetailed"
  res[res == "pig"] <- "domesticanimal" # ?
  res[res == "poultry"] <- "domesticanimal" # ?
  res[res == "poulty"] <- "domesticanimal" # ?
  res[res == "plainszebra"] <- "zebraplains"
  res[res == "rabbitsp"] <- "rabbit"
  res[res == "redhartebeest"] <- "hartebeestred"
  res[res == "reptiles|amphibians"] <- "reptilesamphibians"
  res[res == "reptile"] <- "reptilesamphibians"
  res[res == "roanantelope"] <- "roan"
  res[res == "sharpe'sgrysbok"] <- "grysboksharpes"
  res[res == "smallspottedgenet"] <- "genetcommonsmallspotted"
  res[res == "slendermongoose"] <- "mongooseslender"
  res[res == "springhare"] <- "harespring"
  res[res == "spottedhyena"] <- "hyenaspotted"
  res[res == "wildcat"] <- "catafricanwild"
  res[res == "rhino"] <- "rhinoceros"
  res[res == "rhinocerossp"] <- "rhinoceros"
  res[res == "rhebok(grey)"] <- "rhebokgrey"
  res[res == "rodents"] <- "rodent"
  res[res == "vervetmonkey"] <- "monkeyvervet"
  res[res == "wildebeestsp"] <- "wildebeest"
  res[res == "whitetailedmongoose"] <- "mongoosewhitetailed"
  res[res == "zebrasp"] <- "zebra"
  res[res == "zebraplains"] <- "zebraburchells"
  res[res == "zorillapolecatstriped"] <- "polecatstriped"
  res[res == "zorillastripedpolecat"] <- "polecatstriped"
  
  ## People
  res[res == "humans"] <- "human" # ?
  res[res == "humancyclist"] <- "human" # ?
  res[res == "humanmotorcycle"] <- "human" # ?
  res[res == "people"] <- "human"
  res[res == "researchteam"] <- "human"
  res[res == "vehicle"] <- "human"
  
  ## Other
  res[res == "1"] <- "blank"
  res[res == "NOT identifiable"] <- "unresolvable"
  res[res == "Unidentifiable"] <- "unresolvable"
  res[res == "unidentifiable"] <- "unresolvable"
  res[res == "UNIDENTIFIED"] <- "unresolvable"
  res[res == "blanks"] <- "blank"
  res[res == "empty"] <- "blank"
  res[res == "noanimalpresent"] <- "blank"
  res[res == "none"] <- "blank"
  res[res == "vegetation"] <- "blank"
  res[res == "unidentified"] <- "unresolvable"
  res[res == "unknown"] <- "unresolvable"
  
  # Observations
  # 0 -> the image is duplicated somewhere in other rows and is not empty
  # sundryother -> something is in there
  # "knockeddown" ?
  
  # Questions
  # "porcupine" = "porcupinecape"?
  # "zebramountain" = "zebramountaincape"?
  
  return(res)
}


# Helpers -----------------------------------------------------------------

## Clean camera/location -------------------


#' Clean cameras
#' 
#' Standardize cameras names
#'
#' 
#' @param cameras Cameras vector
#' @param locations locations vector
#' @param classifiers Classifier vector
#' @param silence_warnings Should a warning be displayed when `locationID` is `NA`?
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return a character vector of locations with the same length as input.
#' 
#' @details 
#' For TrapTagger data: 
#' + will remove the leading location code part for all data (eg if `location` is `ATH`, will change `cameras` `ATH_A01` -> `A01`)
#' + if the location code is `KHO`: will remove the dash in the camera name (e.g `KHO_E_A01` -> `EA01`)
#' + if the location code is `SAM`: will remove the dash in the camera name (e.g `SAM_B_A01` -> `BA01`)
#' + if the location code is `SAM`: will remove the dash in the camera name (e.g `TSW_L_A01` -> `LA01`)
#' 
#' For Zooniverse data:
#' 
#' + if the location code is `KHO`, will replace `KHOG` with `E` and  `KHOL` with `M` in `cameras`.
#' + if the location code is `DHP`, will remove leading `D` in `cameras`.
#' + if the location code is `OVE`, will remove leading `O` in `cameras`.
#' 
#' The code `KGA` is replaced with `KHO` if the corresponding camera code starts with 'KHO'.
#'
#' @examples
#' cameras <- c("KHO_E_A01", "OCO2")
#' locations <- c("KHO", "OVE")
#' classifiers <- c("traptagger", "zooniverse")
#' clean_cameras(cameras, locations, classifiers)
#' 
#' @noRd
clean_cameras <- function(cameras, locations, 
                          classifiers,
                          silence_warnings = FALSE,
                          logger = NA) {
  
  camnames <- get_camnames(cameras,
                           locations,
                           silence_warnings = silence_warnings)
  
  cameras_final <- sapply(1:length(camnames), 
                          function(i) {
                            clean_camera(camnames[i], 
                                         location = locations[i],
                                         classifier = classifiers[i])
                          })
  
  return(cameras_final)
}

#' Clean location
#' 
#' Corrects potentially incorrect locations based on the content of cameras.
#' Indeed, for Zooniverse data, cameras in OVE have the locationID DHP and 
#' cameras in KHO have the locationID KGA.
#'
#' @param cameras characgter vector of cameras
#' @param locations character vector of locations (same length)
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return a character vector of locations with the same length as input.
#' 
#' @details
#' The code `DHP` is replaced with `OVE` if the corresponding camera code starts with 'O'.
#' The code `KGA` is replaced with `KHO` if the corresponding camera code starts with 'KHO'.
#' 
#' @examples
#' clean_locations(c("OA01", "KHOGA01"), c("DHP", "KGA"))
#' 
#' @noRd
clean_locations <- function(cameras, locations, logger = NA) {
  
  locations_final <- sapply(1:length(cameras), 
                            function(i) {
                              clean_location(cameras[i], locations[i])
                            })
  return(locations_final)
}

#' Clean one camera
#' 
#' Standardize a camera name
#'
#' @param classifier Classifier vector
#' @param camera A camera
#' @param location A location
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return a cleaned camera name
#' 
#' @details 
#' See clean_cameras
#' 
#' @examples
#' clean_camera("KHO_E_A01", "KHO", "traptagger")
#' 
#' @noRd
clean_camera <- function(camera, location, 
                         classifier = c("zooniverse", "traptagger", "digikam"),
                         logger = NA) {
  
  classifier <- match.arg(classifier)
  
  if(length(camera) != 1) {
    stop("Provide a camera vector of length 1")
  }
  if(length(location) != 1) {
    stop("Provide a location vector of length 1")
  }
  
  if (is.na(camera)) {
    # Don't do anything and return NA
    return(camera)
  }
  
  res <- camera
  
  if(classifier == "traptagger") {
    if (grepl(pattern = "^KHO$|^SAM$|^TSW$", location)) {
      # Subset the dash in camera name
      res <- gsub("_", "", res)
    }
  } else if (classifier == "zooniverse") {
    if (grepl(pattern = "^KHO$", location)) {
      res <-  gsub("^KHOG", "E", res)
      res <-  gsub("^KHOL", "M", res)
    } else if (grepl(pattern = "^DHP$", location)){
      res <- gsub("^^D", "", res)
    } else if (grepl(pattern = "^OVE$", location)){
      res <- gsub("^O", "", res)
    }
  }
  return(res)
}

#' Clean a unique location
#' 
#' Corrects potentially incorrect locations based on the content of cameras.
#' Indeed, for Zooniverse data, cameras in OVE have the locationID DHP and 
#' cameras in KHO have the locationID KGA.
#'
#' @param camera camera
#' @param location location
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return a character vector of locations with the same length as input.
#' 
#' @details
#' See clean_locations
#' 
#' @noRd
clean_location <- function(camera, location, logger = NA) {
  
  # Remove leading locationID (if it is present)
  camname <- get_camnames(camera, location, logger = logger)
  
  new_location <- location
  
  # In Zooniverse, some sites codes are merged whereas they are different in TrapTagger:
  # KHO is assimilated to KGA
  # OVE is assimilated to DHP
  if (!is.na(location)) {
    if(location == "KGA") {
      if (grepl(pattern = "^KHO", camname)) {
        new_location <- "KHO"
      }
    } else if (location == "DHP") {
      # If OVE pattern detected
      if (grepl(pattern = "^O", camname)) {
        new_location <- "OVE"
      }
    }
  }
  
  return(new_location)
}

## Adapt columns -------------------

#' Get camera ID
#' 
#' Returns the complete cameraID as locationID_camera.
#'
#' @param locationID the location code vector (usually 3 letters)
#' @param camera the "old" camera code vector, which is expected not to contain the locationID already.
#' In case it does, then it will not be added and give a message.
#' @param classifier The classifier used. If it is traptagger, will not display message
#' because it is expected that cameraID will already be locationID_camera.
#' It is optional (if not specified, it will display the message by default)
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return a vector of same lengths as locationID and camera with pasted
#' locationID_camera. If camera is already in format locationID_camera, 
#' then it does not changes and displays a message.
#' 
#' @noRd
get_cameraID <- function(locationID, camera, classifier, logger = NA) {
  
  if(missing(classifier)) {
    classifier <- "placeholder"
  }
  
  # Get unique locationID
  unique_locationID <- unique(locationID)
  
  # Check if camera is already in format locationID_camera.
  already_loc <- which(grepl(camera, 
                             pattern = paste0("^", paste0(unique_locationID, collapse = "|"), "_")))
  
  if (length(already_loc) != 0) { # if some cameras already begin with code_loc
    cam_prob <- unique(camera[already_loc])
    
    if(classifier != "traptagger") {
      msg <- paste0("Cameras ",
                    paste(cam_prob, collapse = ", "),
                    " already begin with code_loc: not adding the location.")
      write_log_message(msg, logger = logger, level = "info")
      message(msg)
    }
    
    cameraID <- camera
    cameraID[-already_loc] <- paste(locationID[-already_loc], 
                                    camera[-already_loc],
                                    sep = "_")
  } else {
    cameraID <- paste(locationID, 
                      camera,
                      sep = "_")
  }
  return(cameraID)
}

#' Get eventID
#'
#' Get the eventID from the capture info. All vectors must be the
#' same length or they will be recycled.
#' 
#' @param locationID character vector of location
#' @param cameraID character vector of cam_site
#' @param roll character vector of roll
#' @param capture character vector of capture
#'
#' @return A character vector of the same length of the inputs (or the
#' longest input) with fields formatted as 
#' season#cam_site#roll#event_no
#' 
#' @noRd
get_eventID <- function(locationID, cameraID, roll, capture) {
  
  eventID <- paste(locationID,
                   cameraID,
                   roll, 
                   capture,
                   sep = "#")
  
  return(eventID)
  
}

#' Get camera name
#'
#' Subsets the locations IDs from the camera names vector.
#' 
#' @param camera camera name (character)
#' @param location location (character)
#'
#' @return A camera name without the location prefix (if it was present)
#' 
#' 
#' @examples get_camname("APN_A01", "APN")
#' 
#' @noRd
get_camname <- function(camera, location) {
  
  if (is.na(camera)) {
    return(camera)
  }
  
  if (is.na(location)) {
    res <- gsub(pattern = paste0("^.*_"), 
                replacement = "", 
                camera)
  } else {
    res <- gsub(pattern = paste0("^", location, "_"), 
                replacement = "", 
                camera)
  }
  
  return(res)

}
