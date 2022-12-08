library(data.table)

#' List all files in folder
#' 
#' This function lists all csv files in a base folder. It works recursively 
#' (lists all files in the folder and all files in subfolders of this folder)
#'
#' @param folder the folder to list files in
#' @param except Paths to ignore (optional): these are the relative
#' path from 'folder'. They are either full file names, or a path
#' with partial matching like "folder/*".
#'
#' @return A character vector with relative paths of all files in the folder 'folder',
#' except the files that are indicated by 'except'.
#' 
#' @export
#'
#' @examples
list_csv_in_folder <- function(folder, except){
  
  if(length(folder) > 1){
    stop(paste("Only give one folder in 'folder' please."))
  }
  if(!dir.exists(folder)){
    stop(paste("The folder", folder, "does not exist."))
  }
  
  # --- List files
  in_files_list <- list.files(folder, recursive = TRUE)
  
  # --- Exclude some files
  
  if(!missing(except)) {
    discarded <- c() # initialize vector of discarded files indices
    for(exc in except){
      # Modify exc 
      exc <- gsub(pattern = "*", replacement = ".*", 
                  x = exc,
                  fixed = TRUE) # Replace * (anything) by .* (compatible with R syntax)
      exc <- paste0("^", exc, "$") # match the exact expression from start to end
      
      # get all discarded files for element i
      discard_i <- grep(pattern = exc, x = in_files_list) 
      
      discarded <- c(discarded, discard_i) # add discarded files to total vector
    }
    
    # Message
    if(length(discarded) != 0){
      message("Files:\n", 
              paste(in_files_list[discarded], collapse = "\n"),
              "\nwill be ignored (they are in 'except').")
      in_files_list <- in_files_list[-discarded]
    }
  }
  
  # --- Get csv
  csv_files <- grep(pattern = "\\.csv$", in_files_list, value = TRUE)

  not_csv <- grep(pattern = "\\.csv$", in_files_list, value = TRUE,
                  invert = TRUE)
  
  # --- Keep only csv
  if(length(not_csv) != 0) {
    message(paste0("File(s) ", 
                   paste(not_csv, collapse = ", "),
                   " will be ignored (they are not csv)."))
  }
  
  return(csv_files)
}

#' Name logfile
#'
#' Create a name for the logfile from an input file/folder.
#' 
#'
#' @return A string with format "log__YYYY-MM-DD_HH:MM:SS.log" 
#' where YYYY-MM-DD_HH:MM:SS is the current date/time.
#' 
#' @export
#'
#' @examples
get_logfile_name <- function() {
  
  now <- Sys.time()
  now <- gsub(pattern = " ", 
              replacement = "_",
              now)
  
  logfile_name <- paste0(paste("log", now, sep = "__"), ".log")
  return(logfile_name)
}

#' Create logger
#'
#' Initializes a logger with a given logfile.
#' 
#' @param my_logfile path to the log file (character). 
#'
#' @return Returns a logger and creates a logfile at the given path. If the path given does not
#' exist, also creates this path.
#' 
#' @export
#'
#' @examples
create_logger <- function(my_logfile) {
  
  logfolder <- dirname(my_logfile)
  
  if(!dir.exists(logfolder)) {
    dir.create(logfolder)
  }
  
  file.create(my_logfile)
  
  my_console_appender = console_appender(layout = default_log_layout())
  my_file_appender = file_appender(my_logfile, append = TRUE, 
                                   layout = default_log_layout())
  
  my_logger <- log4r::logger(threshold = "INFO", 
                             appenders = list(my_console_appender, 
                                              my_file_appender))
  
  return(my_logger)
}

#' Write log message
#'
#' Writes a log message. If a logger is provided, writes to that logger; 
#' if it is NA, displays a message.
#' 
#' @param message The message to display/write to the logger
#' @param logger Logger to write to (log4r object of class "logger") (defaults to NA)
#' @param level Logging level: either 'info', 'warn', 'debug' or 'error'.
#'
#' @return Either a message or writes a log (with the logger parameters)
#' 
#' @export
#'
#' @examples
write_log_message <- function(message, logger = NA,
                              level = "info") {
  
  if(all(is.na(logger))) {
    message(msg)
  } else {
    if(level == "info") {
      log4r::info(logger, 
                  message)
    } else if (level == "warn") {
      log4r::warn(logger, 
                  message)
    } else if (level == "error") {
      log4r::error(logger, 
                   message)
    } else if (level == "debug") {
      log4r::debug(logger, 
                   message)
    }
  }
}

#' Get relative path
#'
#' Get the relative path to a file or folder.
#' 
#' @param f the full path to the folder/file.
#' @param wd a string describing the part of the path to delete.
#'
#' @return A string with the truncated path, without leading/terminal "/"
#' if f is a folder.
#' 
#' @export
#'
#' @examples
get_relative_path <- function(f, wd) {
  
  # Get relative path
  rel_path <- gsub(pattern = wd, 
                   replacement = "",
                   f)
  
  rel_path <- gsub(pattern = "^/|/$", 
                   replacement = "",
                   rel_path)
  
  return(rel_path)
}


#' Get files and folder
#' 
#' Get all files from the input character vector. Either the basename from input
#' if they are files, or all files into input recursively if input are folders.
#'
#' @param input a character vector of valid paths: can be files or folders
#' @param except files to ignore (optional)
#'
#' @return A dataframe with columns folder and files, where folder is the 
#' folder up to a given file and file is the file.
#' 
#' @export
#'
#' @examples
get_files_and_folder <- function(input, except) {
  
  # Initialize results
  folders <- c()
  files <- c()
  
  for(inp in input) { # Iterate through inputs
    # Guess if input is a file
    is_file <- grepl("\\..+$", inp)
    
    if(!is_file) { # If input is a folder
      # Set folder to input
      folder <- inp
      
      if(missing(except)) {
        in_files_list <- list_csv_in_folder(folder)
      } else {
        in_files_list <- list_csv_in_folder(folder, 
                                            except = except)
      }
      in_files_list <- sort(in_files_list)
    } else { 
      # in_files_list is set to the file (without the path)
      in_files_list <- basename(inp) 

      # Folder is the rest
      folder <- dirname(inp)
    }
    
    # Store all folders and files
    folders <- c(folders, rep(folder, length(in_files_list)))
    files <- c(files, in_files_list)
  }
  
  df <- data.frame(folders, files)
}

#' Read a file
#'
#' @param filename The name of the file which should exist in base_folder.
#' starting at the base of the R project.
#' @param base_folder The folder to look in
#' @param verbose logical. Display the file name?
#'
#' @return A dataframe with the contents of the csv file
#' @export
#'
#' @examples
read_file <- function(filename, base_folder, verbose = FALSE){
  
  # Set file name
  in_file <- file.path(base_folder, filename)
  
  # Check if file exists
  if (file.exists(in_file)) {
    if(verbose) {
      message("File set to ", in_file)
    }
  }else{
    stop(paste("File", in_file , "does not exist."))
  }
  
  # Issue, empty columns
  if(grepl(filename, pattern = "MAD_S2_full_report_0-50__agreement_corrected_fin.csv")){
    dat <- fread(in_file, select = c(1:26))
    dat <- as.data.frame(dat)
  }else{
    dat <- read.csv(in_file, sep = ",")
  }
  
  if(ncol(dat) == 1){
    message("Trying ';' as separator in the csv file")
    dat <- read.csv(in_file, sep = ";")
  }
  
  return(dat)
}


#' Get final filemane
#'
#' Return the filename from the file columns.
#' 
#' @param df The dataframe to be copied. Must have columns locationID, season, roll.
#'
#' @return The filename for this file in the format locationID_Sseason_Rroll.csv
#' It there are several locationID, seasons or rolls, they are separated by a dash
#' in the filename: locationID1-locationID2...
#' 
#' @export
#'
#' @examples
get_final_filename <- function(df) {
  
  reserve <- unique(df$locationID)
  reserve <- paste(reserve, collapse = "-")
  
  season <- unique(df$season)
  season <- paste(season, collapse = "-")
  
  roll <- unique(df$roll)
  roll <- paste(roll, collapse = "-")
  
  filename <- paste0(reserve, "_S", season, "_R", roll, ".csv")
  return(filename)
}

#' Write the standardized file
#'
#' @param df The standardized file
#' @param in_filename The path to the original file. If it has subfolders,
#' the subfolder structure is copied in to.
#' @param to The target folder to copy data in. It must exist.
#'
#' @return Writes the file to the folder to/xxx where xxx is
#' the subdirectory in which the original file was in.
#' Also returns the path to the file.
#' 
#' @export
#'
#' @examples
write_standardized_file <- function(df, in_filename, to) {
  
  # Check if results folder exists
  if(!dir.exists(to)){
    message(paste("Creating folder", to))
    dir.create(to, recursive = TRUE)
    # stop(paste("Folder ", to, " does not exist."))
  }
  
  # Get subdir name
  subdir <- dirname(in_filename)
  
  if (subdir != ".") { # There is a subdirectory structure
    subdir_target <- file.path(to, subdir)
    if(!dir.exists(subdir_target)){
      dir.create(subdir_target, recursive = TRUE)
    }
  }
  
  # Get filename
  filename <- get_final_filename(df)
  
  # Get full filepath
  filepath <- file.path(to, subdir, filename)
  
  write.csv(df, filepath, 
            row.names = FALSE)
  
  return(filepath)
}
