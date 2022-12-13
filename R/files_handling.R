# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-09
#
# Script Description: functions to help read and write files to standardize Snapshot data.

# Logger ------------------------------------------------------------------

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
create_logger <- function(my_logfile) {
  
  logfolder <- dirname(my_logfile)
  
  if(!dir.exists(logfolder)) {
    dir.create(logfolder)
  }
  
  file.create(my_logfile)
  
  my_console_appender = log4r::console_appender(layout = log4r::default_log_layout())
  my_file_appender = log4r::file_appender(my_logfile, append = TRUE, 
                                          layout = log4r::default_log_layout())
  
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


# Read Snapshot files -----------------------------------------------------


#' Read Snapshot files
#' 
#' Reads files from a vector of folders (and optionnally ignores some file/folders)
#' into a list of dataframes.
#'
#' @param input a character vector of valid paths: can be files or folders, or a mix of both
#' @param except files to ignore (optional)
#' @param basepath the part of the path that should be ignored when copying final
#' files (i.e. absolute path inside one's comupter that should not be copied in final file.)
#'
#' @return A named list of dataframe. Each element of the list is a dataframe containing
#' the contents of a file read from the files list given in input.
#' The names of the list are the file names from the root of input:
#' If the input is a file, it is a filename. 
#' If input is a folder, it is the relative path 
#' from input to the file inside input.
#' 
#' @export
#'
read_snapshot_files <- function(input, except,
                                basepath) {
  
  # Get the files names
  if(missing(except)) {
    files_df <- get_csv_files_and_folders(input = input,
                                          basepath = basepath)
  } else {
    files_df <- get_csv_files_and_folders(input = input, 
                                          except = except,
                                          basepath = basepath)
  }
  
  # Initialize result
  df_list <- vector(mode = 'list', 
                    length = nrow(files_df))
  
  # Iterate through file names
  for(i in 1:nrow(files_df)) {
    # Get file and folder
    in_filename <- files_df$files[i]
    folder <- files_df$folders[i]
    
    message(paste("Reading file", in_filename, "---"))
    
    # Read file
    dat_in <- read_snapshot_file(in_filename, 
                                 base_folder = folder)
    df_list[[i]] <- dat_in
  }
  
  # Name list
  unique_files <- unique(files_df$files)
  
  if(length(unique_files) == length(files_df$files)) {
    names(df_list) <- unique_files
  } else {
    message("Could not name list because there were non-unique file names.")
  }
  
  return(df_list)
}

# Write Snapshot files ----------------------------------------------------

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
write_standardized_file <- function(df, in_filename, to) {
  
  # Check if results folder exists
  if(!dir.exists(to)){
    message(paste("Creating folder", to))
    dir.create(to, recursive = TRUE)
    # stop(paste("Folder ", to, " does not exist."))
  }
  
  # Get subdir name
  if(missing(in_filename)) {
    subdir <- "."
  } else {
    subdir <- dirname(in_filename)
  }
 
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


# Helpers -----------------------------------------------------------------

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
#'
#' @noRd
list_csv_in_folder <- function(folder, except){
  
  if(length(folder) > 1){
    stop(paste("Only give one folder in 'folder' please."))
  }
  if(!dir.exists(folder)){
    stop(paste("The folder", folder, "does not exist."))
  }
  
  # --- Clean folder
  folder_clean <- gsub(pattern = "/$", 
                       replacement = "",
                       folder)
  
  # --- List files
  in_files_list <- list.files(folder_clean, 
                              recursive = TRUE, 
                              full.names = TRUE)
  
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
#'
#' @noRd
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
#' Get all csv files from the input character vector. 
#' If 'input' is a folder, will list all files within input; if 'input'
#' is a file, will only list this file.
#' 
#' @inheritParams read_snapshot_files
#' 
#' @return A dataframe with columns folder and files, where folder is the 
#' folder up to a given file and file is the file.
#' 
#' @noRd
get_csv_files_and_folders <- function(input, 
                                      except,
                                      basepath) {
  # Initialize results
  folders <- c()
  files <- c()
  
  for(inp in input) { # Iterate through inputs
    # Guess if input is a file
    is_file <- grepl("\\..+$", inp)
    
    if(!is_file) { # If input is a folder
      # Set folder to input
      folder_to_read <- inp
      
      if(missing(except)) {
        in_files_list <- list_csv_in_folder(folder_to_read)
      } else {
        in_files_list <- list_csv_in_folder(folder_to_read, 
                                            except = except)
      }
      in_files_list <- sort(in_files_list)
    } else {
      # in_files_list is set to the file (without the path)
      in_files_list <- inp
    }
    
    # Get interesting file path and absoulute path
    in_files_list <- get_relative_path(in_files_list,
                                       wd = basepath)
    folder <- basepath
    
    # Store all folders and files
    folders <- c(folders, 
                 rep(folder, length(in_files_list)))
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
#'
#' @noRd
read_snapshot_file <- function(filename, base_folder, verbose = FALSE){
  
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
    dat <- data.table::fread(in_file, select = c(1:26))
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