library(data.table)

#' List all files in folder
#' 
#' This function lists all files il a base folder. It works recursively (lists
#' all files in the folder and all files in subfolders of this folder)
#'
#' @param folder the folder to list files in
#' @param except Folders or files to ignore: these are the relative
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
#' @param input The input file/folder that will appear in the logfile
#' name.
#'
#' @return A string with the path to the file/folder ("/" and "." replaced by "-") 
#' and the current date/time, separated with "__". Has the extension .log
#' 
#' @export
#'
#' @examples
get_logfile_name <- function(input, wd) {
  
  now <- Sys.time()
  now <- gsub(pattern = " ", 
              replacement = "_",
              now)
  
  rel_input <- get_relative_path(input, wd)
  
  rel_input <- gsub(pattern = "/", 
                    replacement = "-",
                    rel_input)
  rel_input <- gsub(pattern = "\\.", 
                    replacement = "-",
                    rel_input)
  
  logfile_name <- paste0(paste(rel_input, now, sep = "__"), ".log")
  return(logfile_name)
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
    stop(paste("Folder ", to, " does not exist."))
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
