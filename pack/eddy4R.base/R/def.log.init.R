##############################################################################################
#' @title Initialize logging for NEON IS Data Processing

#' @author 
#' Cove Sturtevant \email{csturtevant@battelleecology.org}

#' @description 
#' Definition function. Initialize logging for NEON IS Data Processing. Currently uses the lgr 
#' package and implements hierarchical logging with options for json formatting and writing to file.
#' Note that all parameters default to NULL, in which case an attempt will be made to retrieve the 
#' system environmental variables specified.  

#' @param Name String (optional). Name of logger. Defaults to root if environment variable 
#' LOG_NAME is not found.
#' @param To String (optional). Location to log to. Options are 'stdout' (console) or 'file'. 
#' Defaults to stdout if environment variable LOG_TO is not found.
#' @param NameFile String (optional). Name of file, including relative or absolute path, to log to. 
#' Ingored if To='stdout'. Defaults to ./log.txt if environment variable LOG_FILENAME is not found.
#' @param Fmt String (optional). Logging format. Options are 'plain' (human readable text)
#' or 'json'. Defaults to plain if environment variable LOG_FORMAT is not found.
#' @param Lvl String (optional). The minimum logging level that triggers log output. Options are, in 
#' order of increasing severity, 'debug','info','warn','error','fatal'. 
#' Defaults to info if environment variable LOG_LEVEL is not found.

#' @return A logger object, configured according to inputs. Logging can now be conducted in the main 
#' code, using the functions in the lgr package. See example.

#' @references 
#' License: (example) GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples 
#' lg <- def.log.init(To='stdout',Fmt='plain','Lvl'='info')
#' lg$warn('This is a warning')
#' lg$debug('This debug message will not print because the minimum logging level is info')

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2019-05-09)
#     original creation
#   Cove Sturtevant (2019-06-11)
#     use list input for logger configuration 
##############################################################################################
def.log.init <- function(Name=NULL,
                         To=NULL,
                         NameFile=NULL,
                         Fmt=NULL,
                         Lvl=NULL
){
  
  # Load the logging library
  library(lgr)
  
  # Attempt to retrieve environment variables to set configu parameters
  if(base::is.null(Name)){
    Name <- base::Sys.getenv('LOG_NAME')
    if(Name == ""){
      Name <- NULL
    }
  }

  if(base::is.null(To)){
    To <- base::Sys.getenv('LOG_TO')
    if(To == ""){
      To <- c('stdout','file')[1]
    }
  }
  To <- base::tolower(To)
  
  if(base::is.null(NameFile)){
    NameFile <- base::Sys.getenv('LOG_FILENAME')
    if(NameFile == ""){
      NameFile <- 'log.txt'
    }
  }

  if(base::is.null(Fmt)){
    Fmt <- base::Sys.getenv('LOG_FORMAT')
    if(Fmt == ""){
      Fmt <- c('plain','json')[1]
    }
  }
  Fmt <- base::tolower(Fmt)
  
  if(base::is.null(Lvl)){
    Lvl <- base::Sys.getenv('LOG_LEVEL')
    if(Lvl == ""){
      Lvl <- c('debug','info','warn','error','fatal')[2]
    }
  }
  Lvl <- base::tolower(Lvl)
  
  
  
  
  # Set the layout
  lgFmt  <- switch(Fmt,
                   json=lgr::LayoutJson$new(),
                   plain=lgr::LayoutFormat$new()
  )
  
  # Set the appender
  lgTo <- switch(To,
                 stdout=lgr::AppenderConsole$new(
                   layout=lgFmt
                 ),# write to console (stdout) in Fmt format
                 file=lgr::AppenderFile$new(
                   file=NameFile,
                   layout=lgFmt
                 ) # write to json file in Fmt
                 )
    
  # Initialize the logger
  lg <- lgr::get_logger(Name)
  lg$config(base::list(
    threshold=Lvl,
    appenders=lgTo
  ))
  
  lg$set_propagate(FALSE) # Remove inherited appenders (so we don't double log)
  
  return(lg)
}
