#' @title Logger Class in Singleton Pattern
#' @description 
#' A logger class in Singleton pattern with various logging levels

#' @examples 
#' # Create a logger instance
#' my_logger <- Logger$new(log_file = "logfile.txt")

#' # Create a logger instance without specifying a log file
#' my_logger <- Logger$new()

#' # Set the logging level to "debug"
#' my_logger$set_logging_level("debug")

#' # Log messages with debug level
#' my_logger$debug("This is a debug message.")

#' # Call the function with logging
#' some_function(my_logger)
 
#' # Get and print the log entries
#' log_entries <- my_logger$get_log_entries()
#' print("Log Entries:")
#' print(log_entries)

#' @export
##########################################################################

# Define the extended Logger class with log levels
Logger.Singleton <- R6::R6Class("Logger.Singleton", inherit = R6P::Singleton, public = list(
  #' @param logging_level logging level
  #' @param log_file file to store logging messages
  initialize = function(logging_level = NULL, log_file = NULL) {
    # Call the initialize method of the base class
    super$initialize()
    
    # Additional initialization for logging
    private$log_file <- log_file
    private$log_entries <- list()
    if (is.null(logging_level) || logging_level == "") {
      private$logging_level <- "info" # Default logging level
    } else {
      private$logging_level <- logging_level
    }
  },
  
  #' @param level logging level
  set_logging_level = function(level) {
    private$logging_level <- tolower(level)
  },
  
  #' @param file logging file
  set_log_file = function(file) {
    private$log_file <- file
  },
  
  #' @param level logging level
  #' @param message logging message
  log_message = function(level, message) {
    print(private$logging_level)
    print(match(private$logging_level, c("debug", "info", "warn", "error")))
    print(tolower(level))
    # Check if the message level is equal to or above the logging level
    if (match(tolower(level), c("debug", "info", "warn", "error")) >=
        match(private$logging_level, c("debug", "info", "warn", "error"))) {
      timestamp <- Sys.time()
      log_entry <- paste(timestamp, " [", level, "]: ", message)
      private$log_entries <- c(private$log_entries, list(log_entry))
      cat(log_entry, "\n")

      # If a log file is specified, write to it
      if (!is.null(private$log_file)) {
        cat(log_entry, "\n", file = private$log_file, append=TRUE)
      }
    }
  },
  
  #' @param message logging message
  debug = function(message) {
    self$log_message("DEBUG", message)
  },
  
  #' @param message logging message
  info = function(message) {
    self$log_message("INFO", message)
  },
  
  #' @param message logging message
  warn = function(message) {
    self$log_message("WARN", message)
  },
  
  #' @param message logging message
  error = function(message) {
    self$log_message("ERROR", message)
  },
  
  get_log_entries = function() {
    return(private$log_entries)
  },
  
  get_logging_level = function() {
    return(private$logging_level)
  }, 
  
  get_log_file = function() {
    return(private$log_file)
  }
), private = list(
  log_file = NULL,
  log_entries = NULL,
  logging_level = NULL
))
