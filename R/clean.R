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

#' Add location prefix
#' 
#' Adds a prefix to a set of cameras, previously verifying that the prefix is not in the camera 
#' name.
#'
#' @param cameras a character vector of camera names
#' @param locations a character vector of locations (must be the same length as `cameras`)
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#'
#' @return A character vector of the same length as `cameras` where each value is prefixed
#' with the corresponding value of `locations` (in the same order). The function will not double
#' the prefix in case it was already present.
#' 
#' @export
#'
#' @examples
#' cameras <- c("A01", "B01", "KAR_A01")
#' locations <- c("KAR", "KAR", "KAR")
#' add_location_prefix(cameras, locations)
add_location_prefix <- function(cameras,
                                locations,
                                logger = NA) {
  
  cameras <- get_camnames(cameras,
                          locations,
                          silence_warnings = TRUE,
                          logger = logger)
  
  if ( any(is.na(locations))) {
    msg <- "Some locations are NA: camera codes have been modified accordingly to NA_cam."
    write_log_message(msg, logger = logger, level = "warn")
    warning(msg)
  }
  
  locations_cameras <- paste(locations, cameras, sep = "_")
  
  return(locations_cameras)
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
#' + For all cameras: will prefix the camera name with the location (e.g. `A01` -> `KHO_A01`)
#' 
#' + For TrapTagger data: if the location code is `KHO`, `SAM` or `TSW`: 
#' will remove the dash in the camera name (e.g. `KHO_E_A01` -> `KHO_EA01`)
#' 
#' + For Zooniverse data: if the location code is `KHO`, will replace 
#' `KHOG` with `E` and  `KHOL` with `M` in `cameras` (and add the location prefix).
#' If the location code is `DHP`, will remove leading `D` in `cameraID` (and add the location prefix).
#' If the location code is `OVE`, will remove leading `O` in `cameraID`(and add the location prefix).
#' 
#' + For column `eventID`: the event ID formatted as cameraID#roll#event_no.
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
    mutate(eventID = get_eventID(cameraID, roll, capture))
  
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
  # spider -> "insect"?
  
  return(res)
}


# Helpers -----------------------------------------------------------------

## Clean camera/location -------------------


#' Clean cameras
#' 
#' Standardize cameras names
#'
#' @param cameras Cameras vector
#' @param locations locations vector
#' @param classifiers Classifier vector. If the classifier is `NULL`, 
#' the function will apply all Zooniverse corrections.
#' @param silence_warnings Should a warning be displayed when `locationID` is `NA`?
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return a character vector of cameras with the same length as input.
#' 
#' @details 
#' For all data: will add the location prefix.
#' 
#' For TrapTagger data: 
#' + if the location code is `KHO`: will remove the dash in the camera name (e.g `KHO_E_A01` -> `KHO_EA01`)
#' + if the location code is `SAM`: will remove the dash in the camera name (e.g `SAM_B_A01` -> `SAM_BA01`)
#' + if the location code is `SAM`: will remove the dash in the camera name (e.g `TSW_L_A01` -> `TSW_LA01`)
#' + if the location code is `PLN`: will replace leading `PIL_` with `PLN` (e.g `PIL_A01` -> `PLN_A01`)
#' For Zooniverse data:
#' 
#' + if the location code is `KHO`, will replace `KHOG` with `E` and  `KHOL` with `M` in `cameras` (and add location prefix).
#' + if the location code is `DHP`, will remove leading `D` in `cameras`  (and add location prefix).
#' + if the location code is `OVE`, will remove leading `O` in `cameras`  (and add location prefix).
#' 
#' @export
#' 
#' @examples
#' cameras <- c("KHO_E_A01", "OCO2")
#' locations <- c("KHO", "OVE")
#' classifiers <- c("traptagger", "zooniverse")
#' clean_cameras(cameras, locations, classifiers)
clean_cameras <- function(cameras, locations, 
                          classifiers,
                          silence_warnings = FALSE,
                          logger = NA) {
  
  camnames <- get_camnames(cameras,
                           locations,
                           silence_warnings = silence_warnings)
  
  if(missing(classifiers)) {
    cameras_final <- sapply(1:length(camnames), 
                            function(i) {
                              clean_camera(camnames[i], 
                                           location = locations[i])
                            })
  } else {
    cameras_final <- sapply(1:length(camnames), 
                            function(i) {
                              clean_camera(camnames[i], 
                                           location = locations[i],
                                           classifier = classifiers[i])
                            })
  }
  
  # Add location prefix
  cameras_final <- add_location_prefix(cameras_final, locations, 
                                       logger = logger)
  
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
#' The code `PIL` is replaced with `PLN`
#' @export
#' 
#' @examples
#' clean_locations(c("OA01", "KHOGA01"), c("DHP", "KGA"))
clean_locations <- function(cameras, locations, logger = NA) {
  
  if("PIL" %in% locations){
    msg <- "Will change location PIL -> PLN"
    write_log_message(msg, logger = logger, level = "info")
    message(msg)
  }
  
  locations_final <- sapply(1:length(cameras), 
                            function(i) {
                              clean_location(cameras[i], locations[i])
                            })
  return(locations_final)
}

#' Clean one camera
#' 
#' Standardize a camera name. This function assumes that there is no location prefix.
#'
#' @param classifier Classifier vector. If the classifier is `NULL`, the function will apply all
#' Zooniverse corrections.
#' @param camera A camera
#' @param location A location
#' @param logger a `log4r` `logger` object if you want logging (can be created with `create_logger`), 
#' else `NA`. 
#' 
#' @return a cleaned camera name (without prefix)
#' 
#' @details 
#' See clean_cameras
#' 
#' @examples
#' clean_camera("KHO_E_A01", "KHO", "traptagger")
#' 
#' @noRd
clean_camera <- function(camera, location, 
                         classifier = c(NULL, "zooniverse", "traptagger", "digikam"),
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
    
    if (location == "PLN") {
      # Remove "PIL_" that was the old location_code
      res <- gsub("^PIL_", "", res)
    }
    
  } else if (classifier == "zooniverse" | is.null(classifier)) {
    if (grepl(pattern = "^KHO$", location)) {
      res <-  gsub("^KHOG", "E", res)
      res <-  gsub("^KHOL", "M", res)
    } else if (grepl(pattern = "^DHP$", location)){
      if(nchar(res) >= 4) { # This condition to avoid deleting D in eg "D04"
        res <- gsub("^D", "", res)
      }
    } else if (grepl(pattern = "^OVE$", location)){
      if(nchar(res) >= 4){ # This condition to avoid deleting D in eg "O04" (even if not sure it exists)
        res <- gsub("^O", "", res)
      }
    } else if (grepl(pattern = "^KGA$", location)) { 
      # remove KGA before camera name in KGA cameras in Zooniverse data
      res <- gsub("^KGA", "", res)
    }
  }
  
  if(!is.na(location)) { # For the following corrections we need non-NA location
    
    if(location == "APN"){
      # Correct majuscules/minuscules Umh
      if(grepl("^Umh", res)) {
        res <- gsub("^Umh", "UMH", res)
      }
      # Correct JJxb -> JJx
      if(grepl("^JJ(1|5|6|7|8)b$", res)) {
        res <- gsub("^(JJ(1|5|6|7|8))b$", "\\1", res)
      }
      
    }
    
    # Correct c04/05 in ATH
    if(location == "ATH" & grepl("^c(04|05)$", res)){
      res <- gsub("^c", "C", res)
    }
    
    # Correct ADWA
    if(location == "MAD" & grepl("^ADWA", res)){
      res <- gsub("^ADWA", "MADWA", res)
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
        if (nchar(camname >= 4)) {
          # This condition to avoid changing location 
          # to OVE if camname is eg "O04" (even if not sure it exists)
          new_location <- "OVE"
        }
      }
    } else if (location == "PIL") { # Replace PLN erroneous code_loc
      new_location <- "PLN"
    }
  }
  
  return(new_location)
}

## Adapt columns -------------------

#' Get eventID
#'
#' Get the eventID from the capture info. All vectors must be the
#' same length or they will be recycled.
#' 
#' @param cameraID character vector of cam_site
#' @param roll character vector of roll
#' @param capture character vector of capture
#'
#' @return A character vector of the same length of the inputs (or the
#' longest input) with fields formatted as 
#' cam_site#roll#event_no
#' 
#' @noRd
get_eventID <- function(cameraID, roll, capture) {
  
  eventID <- paste(cameraID,
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

## Misc -------------------
#' Clean blanks
#' 
#' Removes the blanks at the beginning or end of the contents of
#' the columns of a dataframe.
#'  
#' @param df The dataframe
#'
#' @return The dataframe with cleaned columns
#' 
#'
#' @noRd
clean_blanks <- function(df) {
  
  df_res <- lapply(df, 
                   function(i) {
                     gsub(pattern = "^\\s+|\\s+$", 
                          replacement = "", 
                          i)
                   }
  )
  df_res <- as.data.frame(df_res)
  return(df_res)
}
