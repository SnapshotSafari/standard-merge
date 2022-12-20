# Header #############################################################
# 
# Author: Lisa Nicvert
# Email:  lisa.nicvert@univ-lyon1.fr
# 
# Date: 2022-12-20
#
# Script Description: functions for logging


#' Create logger
#'
#' Initializes a logger with a given logfile.
#' 
#' @param my_logfile path to the log file (character). 
#' @param threshold logging levels to write to the logger (see `loglevel` documentation in `log4r`).
#' Defalts to `INFO`.
#'
#' @return Returns a logger and creates a logfile at the given path. If the path given does not
#' exist, also creates this path.
#' 
#' @export
#' 
#' @examples 
#' logger <- create_logger(tempfile())
create_logger <- function(my_logfile, threshold = c("INFO", "DEBUG", "WARNING", "ERROR", "FATAL")) {
  
  threshold <- match.arg(threshold)
  
  logfolder <- dirname(my_logfile)
  
  if(!dir.exists(logfolder)) {
    dir.create(logfolder)
  }
  
  file.create(my_logfile)
  
  my_console_appender = log4r::console_appender(layout = log4r::default_log_layout())
  my_file_appender = log4r::file_appender(my_logfile, append = TRUE, 
                                          layout = log4r::default_log_layout())
  
  my_logger <- log4r::logger(threshold = threshold, 
                             appenders = list(my_console_appender, 
                                              my_file_appender))
  
  
  write_log_message(paste("Create logger", my_logfile), 
                    level = "info", logger = my_logger)
  
  return(my_logger)
}

#' Write log message
#'
#' Writes a log message. If a logger is provided, writes to that logger; 
#' if it is NA, displays a message.
#' 
#' @param message The message to display/write to the logger
#' @param logger Logger to write to (`log4r` object of class `logger`) (defaults to `NA`)
#' @param level Logging level: either `info`, `warn`, `debug` or `error`.
#'
#' @return Will either write the message specified in `message` to the logger or 
#' returns a print, message, warning or stop depending on `level` (see details).
#' 
#' @details
#' + if `logger` is not `NA`, writes a message to the logger file with the specified level.
#' + if `logger` is `NA`, the output depends on `level`. If `level` is `info`
#' returns a `message()`, if it is `warn` returns a `warning()`,
#' if `error` returns an `error()` and if `debug` returns a `print`.
#' 
#' @export
#' 
#' @examples
#' # With no logger
#' write_log_message("Test", level = "debug")
#' write_log_message("Test", level = "info")
#' \dontrun{
#' write_log_message("Test", level = "warn") # Returns a warning
#' write_log_message("Test", level = "error") # Returns an error
#' }
#' # With a logger
#' logger <- create_logger(tempfile()) # Create a logger
#' write_log_message("Test", level = "debug", logger = logger)
#' write_log_message("Test", level = "info", logger = logger)
#' \dontrun{
#' write_log_message("Test", level = "warn", 
#'                   logger = logger) # Writes the log mesage and returns a warning
#' write_log_message("Test", level = "error", 
#'                   logger = logger) # Writes the log message and returns an error
#' }
write_log_message <- function(message, logger = NA,
                              level = "info") {
  
  if(level == "info") {
    if(!all(is.na(logger))) {
      log4r::info(logger, 
                  message)
    } else {
      message(message)
    }
  } else if (level == "warn") {
    if(!all(is.na(logger))) {
      log4r::warn(logger, 
                  message)
      warning(message)
    } else {
      warning(message)
    }
  } else if (level == "error") {
    if(!all(is.na(logger))) {
      log4r::error(logger, 
                   message)
      stop(message)
    } else {
      stop(message)
    }
  } else if (level == "debug") {
    if(!all(is.na(logger))) {
      log4r::debug(logger, 
                   message)
    } else {
      print(message)
    }
  }
}


