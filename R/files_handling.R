# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-09
#
# Script Description: functions to help read and write files to standardize Snapshot data.

# Read Snapshot files -----------------------------------------------------

#' Get csv files and folder
#' 
#' Get the names of all csv files from the input character vector. 
#' 
#' 
#' @inheritParams read_snapshot_files
#' 
#' @return A dataframe with columns `folders` and `files`, where `folders` are the 
#' paths up to a given file (corresponding to the relative path to the file from `basepath`), 
#' and `files` are the files paths from `folders`.
#' 
#' @details 
#' `input` can be a vector of files and/or folders.
#' If an element of `input` is a folder, the function will list
#' all files within `input`; if the element is a file, 
#' the function will only list this file.
#'
#' @export
#' 
#' @examples 
#' \dontrun{
#' get_csv_files_and_folders(input = c("path/to/datafolder/KGA",
#'                                     "path/to/datafolder/ATH_Roll1_Snapshot.csv"), 
#'                                     except = "KGA/KGA-KHO_together", 
#'                                     basepath = "path/to/datafolder")
#' }
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
                                            except = except,
                                            basepath = basepath)
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
  return(df)
}

#' Read Snapshot files
#' 
#' Reads files from a vector of files and/or folders
#' into a list of dataframes.
#'
#' @param input a character vector of valid paths: can be files and/or folders
#' @param except files to ignore (optional): all paths for (a part of) which match 
#' the exact expression in `except` will be ignored.
#' @param basepath the part of the path that should be ignored when copying final
#' files (i.e. absolute path inside one's comupter that should not be copied in final file.)
#'
#' @return A named list of dataframes. Each element of the list is a dataframe with
#' the contents of a file read from the files list given in input.
#' The names of the list are the file names from the root of input:
#' If the element from `input` is a file, it is a filename. 
#' If the element from `input` is a folder, it is the relative path 
#' from `basepath` to the file.
#' 
#' @details 
#' The files are assumed to be comma or semicolon-separated CSV.
#' If the filename is `MAD_S2_full_report_0-50__agreement_corrected_fin.csv`,
#' the function only reads the first 26 columns as this file has empty columns.
#' 
#' @export
#' @examples 
#' \dontrun{
#' read_snapshot_files("path/to/datafolder/DHP", 
#'                     basepath = "path/to/datafolder", 
#'                     except = "DHP/DHP+OVE_same_file/*")
#' }
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
#' @param df The dataframe to be copied. Must have columns `locationID`, `season`, `roll`.
#'
#' @return The filename for this file in the format `locationID_Sseason_Rroll.csv`
#' It there are several values in `locationID`, `season` or `roll`, they are separated by a dash
#' in the filename: `locationID1-locationID2`...
#' 
#' @export
#' 
#' @examples
#' zooniverse_std <- standardize_snapshot_df(df = zooniverse, standard = standard)
#' get_final_filename(zooniverse_std)
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
#' Writes a file to a given location. If `to` does not exist, it is 
#' created, and if `filename` is not provided, a default standardized name is chosen.
#' 
#' @param df The standardized file
#' @param filename The name to give to the file.
#' @param to The target folder to copy data in. If it does not exist, will be created.
#' @param write if TRUE (default), will write the `df` in the 
#' `to` folder, and `df` will be named `filename`.
#' @param return_path Should the path be returned?
#' @param verbose Should messages be displayed when creating a folder/file?
#'
#' @return Writes the file to the folder `to/filename`.
#' Also returns the path `to/filename` if `return_path == TRUE`.
#' 
#' @export
#'
#' @examples
#' std_dat <- standardize_snapshot_df(zooniverse, standard)
#' 
#' # Don't write data
#' write_standardized_df(std_dat, 
#'                       to = "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data", 
#'                       write = FALSE)
#' # Don't write data and use custom name
#' write_standardized_df(std_dat, 
#'                       to = "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data",
#'                       filename = "myname.csv", 
#'                       write = FALSE)
#' # Write file to temporary location
#' write_standardized_df(std_dat, 
#'                       to = tempdir())
write_standardized_df <- function(df, to,
                                  filename,
                                  write = TRUE,
                                  return_path = ifelse(write, FALSE, TRUE),
                                  verbose = TRUE) {
  
  # Check if results folder exists
  if(!dir.exists(to) & write){
    if (verbose) {
      message(paste("Creating folder", to))
    }
    dir.create(to, recursive = TRUE)
  }
  
  # Get default filename if not provided
  if(missing(filename)) {
    filename <- get_final_filename(df)
  }
  
  # Check filename (no /)
  if(grepl(pattern = "/", filename, fixed = TRUE)) {
    stop(paste("Filename", filename, "contains a slash: please provide a valid filename."))
  }
  
  # Get full filepath
  to_clean <- gsub(pattern = "/$", 
                   replacement = "",
                   to)
  filepath <- file.path(to_clean, filename)
  
  if(write) {
    if(verbose) {
      message(paste("Writing file", filepath))
    }
    write.csv(df, filepath, 
              row.names = FALSE)
  }
  if(return_path) {
    return(filepath)
  }
}

#' Write the standardized files
#'
#' Writes a list of files to a given location. If `to` does not exist, it is 
#' created, and if `filename` is not provided, a default standardized name is chosen.
#' 
#' @param df_list The standardized files list to write. If the list is named,
#' the subdirectory structure in the names will be used to replicate the subdirectiry structure
#' in the destination.
#' 
#' @param filenames (Optional) vector of customized file names.
#' @param to Destination folder to write to.
#' @param write Should the result be written or only the path returned?
#' @param return_path Should the path be returned?
#' @param verbose Should messages be displayed when creating a folder/file?
#'
#' @return Writes the files `to/filename1`, `to/filename2`...
#' Also returns the paths `to/filename1`, `to/filename2`... if `return_path == TRUE`.
#' 
#' @export
#'
#' @examples
#' # Example with a subdirectory structure (inferred from the filenames)
#' df_list <- list(zooniverse, digikam, traptagger)
#' names(df_list) <- c("APN/APN.csv", "MOK/MOK.csv", "ATH/ATH.csv")
#' std_list <- standardize_snapshot_list(df_list, standard)
#' 
#' # Don't write data
#' write_standardized_list(std_list, 
#'                         to = "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data", 
#'                         write = FALSE)
#' # Don't write data and use custom name
#' write_standardized_list(std_list, 
#'                         to = "/home/lnicvert/Documents/PhD/Snapshot/data/2_standardized_data",
#'                         filenames = c("myname1.csv", "myname2.csv", "myname3.csv"),
#'                         write = FALSE)
#' # Write files to temporary location
#' write_standardized_list(std_list, to = tempdir())
#'                         
#' # Without a subdirectory structure (and without list names)
#' df_list <- list(zooniverse, digikam, traptagger)
#' names(df_list)[2] <- "MOK"
#' std_list <- standardize_snapshot_list(df_list, standard)
#' 
#' # Write files to temporary location
#' write_standardized_list(std_list, to = tempdir())
write_standardized_list <- function(df_list, 
                                    filenames, to,
                                    write = TRUE,
                                    return_path = ifelse(write, FALSE, TRUE),
                                    verbose = TRUE) {
  
  # --- Check arguments
  if(!inherits(df_list, "list")) {
    stop(paste("df_list must be a list, you provided an object of class", 
               paste(class(df_list), collapse = ", ")))
  }
  
  if(!missing(filenames)) {
    if(length(filenames) != length(df_list)) {
      stop("If filenames are provided, they should be the same length as df_list.")
    }
  }
  
  if(length(to) != 1) {
    stop("Please provide a unique folder in to.")
  }
  
  path_list <- c()
  for(i in 1:length(df_list)) {
    
    df_i <-  df_list[[i]]
    
    # --- Check object
    if(!inherits(df_i, "data.frame")) {
      stop(paste0(deparse(quote(df_list)),"[[", i, "]] ", 
                  "must be a dataframe."))
    }
    
    # --- Final filename
    if(!missing(filenames)) {
      final_name <- filenames[i]
    } else {
      final_name <- get_final_filename(df_i)
    }
    
    # --- Message
    if(!is.null(names(df_list)[i]) & !is.na(names(df_list)[i])) {
      initial_name <- names(df_list)[i]
      subdir <- dirname(initial_name)
    } else {
      initial_name <- i
      subdir <- "."
    }
    
    if (subdir != ".") { # There is a subdirectory structure
      subdir_target <- file.path(to, subdir)
      if(!dir.exists(subdir_target) & write){
        if (verbose) {
          message(paste("Creating folder", subdir_target))
        }
        dir.create(subdir_target, recursive = TRUE)
      }
      final_to <- file.path(to, subdir)
      final_to <- gsub(pattern = "/$", 
                       replacement = "",
                       final_to)
    } else {
      final_to <- to
    }
    
    if(verbose & write) {
      if(subdir != ".") {
        final_name_message <- file.path(subdir, final_name)
      } else {
        final_name_message <- final_name
      }
      message(paste("Writing file", initial_name, "->", final_name_message, "---"))
    }
    path_i <- write_standardized_df(df_i, 
                                    filename = final_name, 
                                    to = final_to,
                                    write = write,
                                    verbose = FALSE,
                                    return_path = TRUE) 
    path_list <- c(path_list, path_i)
  }
  
  if(return_path) {
    return(path_list)
  }
  
}


# Helpers -----------------------------------------------------------------

#' List all files in folder
#' 
#' This function lists all csv files in a base folder. It works recursively 
#' (lists all files in the folder and all files in subfolders of this folder)
#'
#' @param folder the folder to list files in
#' @param except files to ignore (optional): all paths for (a part of) which match 
#' the exact expression in `except` will be ignored.
#' @param basepath (optional) the part of the path that should not be displayed in
#' warning message for ignored files.
#'
#' @return A character vector with relative paths of all files in the folder 'folder',
#' except the files that are indicated by 'except'.
#' 
#'
#' @noRd
list_csv_in_folder <- function(folder, except, basepath){
  
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
      # get all discarded files for element i
      discard_i <- grep(pattern = exc,
                        x = in_files_list,
                        fixed = TRUE)
      
      discarded <- c(discarded, discard_i) # add discarded files to total vector
    }
    
    # Message
    if(length(discarded) != 0){
      if (!missing(basepath)) {
        discarded_files <- get_relative_path(in_files_list[discarded], basepath)
      } else {
        discarded_files <- in_files_list[discarded]
      }
      message("Files:\n", 
              paste(discarded_files, collapse = "\n"),
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