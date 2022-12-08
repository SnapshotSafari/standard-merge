#' Standardize species
#'
#' Eliminate species duplicate names (things like 'birdofprey' ans 'birdsofprey')
#' 
#' @param species vector of species names
#'
#' @return the vector of species names with names stabdardized
#' 
#' @export
#'
#' @examples
standardize_species <- function(species){
  
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