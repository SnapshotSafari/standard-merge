library(data.table)

#' List all files in folder
#' 
#' This function lists all files il a base folder. It works recursively (lists
#' all files in the folder and all files in subfolders of this folder)
#'
#' @param folder the folder to list files in
#' @param except Folders or files to ignore (optional): these are the relative
#' path from 'folder'. If an element in 'except' is a folder, then all files in 
#' this folder are ignored.
#'
#' @return A character vector with relative paths of all files in the folder 'folder',
#' except the files that are indicated by 'except'.
#' 
#' @export
#'
#' @examples
list_files_in_folder <- function(folder, except){
  
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
    for(exc in except){
      # Modify exc to match only the leading characters
      exc <- paste0("^", exc, ".*$")
      discard_i <- grep(pattern = exc, x = in_files_list)
      
      if(length(discard_i) != 0){
        message("Files:\n", paste(in_files_list[discard_i], collapse = "\n"),
                "\nwill be ignored.")
        in_files_list <- in_files_list[-discard_i]
      }
    }
  }
  
  # --- Look for xls/xlsx files that are copies of csv
  split_extension_list <- str_split(in_files_list, "\\.")
  
  excel_files <- sapply(split_extension_list, 
                        function(l) grepl(l[2], pattern = "xls"))
  excel_files <- split_extension_list[excel_files]
  
  if(length(excel_files) != 0) {
    message("Excel file(s) detected: ", paste(excel_files, collapse = ", "))
    
    # Look for doublets
    for (ex in excel_files) {
      filename_i <- ex[1]
      
      csvname <- paste0(filename_i, ".csv")
      
      if(csvname %in% in_files_list) {
        ex_total <- paste(ex, collapse = ".")
        message("File ", ex_total, "\nalready present in csv as ",
                csvname, "\nDiscarding Excel file.")
        in_files_list <- in_files_list[-which(in_files_list == ex_total)]
      }
    }
  }
  
  return(in_files_list)
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
#'
#' @return Either a message or writes a log (with the logger parameters)
#' 
#' @export
#'
#' @examples
write_log_message <- function(message, logger = NA) {
  
  if(all(is.na(logger))) {
    message(msg)
  } else {
    log4r::info(logger, 
                message)
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
#' Get all files inside the input folder (if it is a folder)
#' else if it is a file, folder will just be its basename.
#'
#' @param input input (can be a file or a folder, but must be a valid path)
#' @param to_ignore files to ignore (optional)
#'
#' @return
#' @export
#'
#' @examples
get_files_and_folder <- function(input, to_ignore) {
  
  # Guess if input is a file
  is_file <- grepl("\\..+$", input)
  
  if(!is_file) { # If input is a folder
    # Set folder to input
    folder <- input 
    
    if(missing(to_ignore)) {
      in_files_list <- list_files_in_folder(folder)
    } else {
      in_files_list <- list_files_in_folder(folder, 
                                            except = to_ignore)
    }
    
    
    in_files_list <- sort(in_files_list)
  } else { 
    # in_files_list is set to the file (without the path)
    in_files_list <- basename(input) 
    
    # Folder is the rest
    folder <- dirname(input)
  }
  
  return(list(folder = folder, 
              files = in_files_list))
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

#' Write the standardized file
#'
#' @param df The standardized file
#' @param in_filename The path to the original file. If it has subfolders,
#' the subfolder structure is copied in to.
#' @param to The target folder to copy data in. It must exist.
#'
#' @return Writes the file to the folder to/xxx where xxx is
#' the subdirectory in which the original file was in.
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
  
  # Prepare file name
  reserve <- str_extract(basename(in_filename), "^[A-Z]+")
  
  season <- std_dat$season
  season <- unique(season)
  season <- paste(season, collapse = "-")
  
  roll <- unique(df$roll)
  roll <- paste(roll, collapse = "-")
  
  filename <- paste0(reserve, "_S", season, "_R", roll, ".csv")
  
  write.csv(df, file.path(to, subdir, filename), 
            row.names = FALSE)
}
