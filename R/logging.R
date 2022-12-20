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
#' @param console Write log messages to console?
#'
#' @return Returns a logger and creates a logfile at the given path. If the path given does not
#' exist, also creates this path.
#' 
#' @export
#' 
#' @examples 
#' logger <- create_logger(tempfile())
create_logger <- function(my_logfile, 
                          threshold = c("INFO", "DEBUG", "WARNING", "ERROR", "FATAL"),
                          console = FALSE) {
  
  threshold <- match.arg(threshold)
  
  logfolder <- dirname(my_logfile)
  
  if(!dir.exists(logfolder)) {
    dir.create(logfolder)
  }
  
  file.create(my_logfile)
  
  my_file_appender = log4r::file_appender(my_logfile, append = TRUE, 
                                          layout = log4r::default_log_layout())
  
  if (console) {
    my_console_appender = log4r::console_appender(layout = log4r::default_log_layout())
    my_logger <- log4r::logger(threshold = threshold, 
                               appenders = list(my_console_appender, 
                                                my_file_appender))
  } else {
    my_logger <- log4r::logger(threshold = threshold, 
                               appenders = my_file_appender)
  }
  
  msg <- paste("Create logger", my_logfile)
  write_log_message(msg, 
                    level = "info", logger = my_logger)
  message(msg)
  
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
#' do nothing if logger is `NA`.
#' 
#' @details
#' + if `logger` is not `NA`, writes a message to the logger file with the specified level.
#' + else, doesn't do anything.
#' 
#' @export
#' 
#' @examples
#' logger <- create_logger(tempfile()) # Create a logger
#' write_log_message("Test", level = "debug", logger = logger)
#' write_log_message("Test", level = "info", logger = logger)
#' write_log_message("Test", level = "warn", logger = logger) 
#' write_log_message("Test", level = "error", logger = logger) 
write_log_message <- function(message, logger = NA,
                              level = c("info", "warn", "error", "debug")) {
  
  level <- match.arg(level)
  
  if(!all(is.na(logger))) {
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


