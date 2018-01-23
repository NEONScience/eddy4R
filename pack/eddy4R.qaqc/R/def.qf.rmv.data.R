##############################################################################################
#' @title Definition function: to remove high frequency data points that have failed quality flags

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function  to remove high frequency data points that have failed quality flags from a data.frame
#' @param dfData Input data.frame for data to be removed from based on quality flags
#' @param dfQf Input data.frame of quality flags (must be of class integer to be included in the processing)
#' @param Sens Character string indicating the sensor the high frequency data come from to check for sensor specific flags
#' @param Vrbs Optional. A logical {FALSE/TRUE} value indicating whether to:\cr
#' \code{Vrbs = FALSE}: (Default) cleaned data set, list of variables assessed, list of quality flags for each variable assessed, and the total number of bad data per variable, or \cr
#' \code{Vrbs = TRUE}: cleaned data set, list of variables assessed, list of quality flags for each variable assessed, the number of each quality flag tripped for each variable and the total number of bad data per variable
#' 
#' @return A list (\code{rpt}) containing a dataframe (\code{rpt$dfData}) of the data with bad data replaced by NaN's, a vector of data variables to assess (\code{rpt$listVar}),  a list containing vectors of flag names used for each variable (\code{rpt$listQf}),  a list containing vectors of the number of quality flags set high for each variable (\code{rpt$fracQfBad}), a list containing vectors of flagged data points for each variable (\code{rpt$posBad}), a list containing total number of flagged data points for each variable (\code{rpt$numBadSum}).

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000807) \cr

#' @keywords NEON, qfqm, quality

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-10-27)
#     original creation
#   Dave Durden (2017-12-12)
#     bug fixes when only one qf is used to determine qf analysis
#   Dave Durden (2017-01-15)
#     bug fixes when only one qf is set high in first element of list
##############################################################################################


def.qf.rmv.data <- function(
  dfData,
  dfQf,
  Sens = NULL,
  Vrbs = FALSE,
  TypeData = c("integer", "real")[1]){
  
  #Create a list to hold all the output
  rpt <- list()
  
  #Can convert to normal data.frame to prevent impact ff objects
  #rpt$dfData<- as.data.frame(dfData)
  #dfQf2 <- as.data.frame(dfQf)
  
  #Package data for reporting
  rpt$dfData <- dfData
  
  #List of variables to check for flags to remove bad data
  rpt$listVar <- base::names(dfData[!base::names(dfData) %in% c("time", "idx")])
  
  #If a sensor (Sens) is included, check for sensor specific flags to perform filtering of data    
  if(!base::is.null(Sens) && base::length(Sens) == 1){ 
    qfSens <- base::paste0("qf",base::toupper(substr(Sens, 1, 1)),base::substr(Sens,2,base::nchar(Sens))) # Sensor flags to look for i.e. qfIrga
    qfSens <- base::ifelse(test = qfSens == "qfSoniAmrs", yes = "qfAmrs", qfSens) #IF Sensor is the soniAmrs, change the qualifier to qfAmrs (remove soni)
  } else{
    qfSens <- NULL  
  }
  
  # Grab only qf that are integers 
  if(TypeData == "integer"){
  if(ff::is.ffdf(dfQf)){
    qfName <-  base::names(dfQf[ff::vmode(dfQf) == "integer"]) #method for ffdf objects
  }else{
    qfName <-  base::names(base::Filter(base::is.integer, dfQf))  #method for normal data.frame objects
  }
  }else{
    qfName <-  base::names(dfQf)
  }
  
  # A list of all the flags to be included in the data removal  
  rpt$listQf  <- base::lapply(rpt$listVar, function(x){ 
    tmp <- base::subset(x = names(dfQf), subset = base::grepl(pattern = base::paste(x,qfSens, sep ="|"),x = qfName, ignore.case = TRUE))
    #qfCal flags from consideration
    tmp[base::grep(pattern = "qfCal", x = tmp, invert = TRUE), drop = FALSE]
  })
  
  #Add list names
  base::names(rpt$listQf) <- rpt$listVar
  
  #Initialize list to prevent simplifying
  rpt$posBad <- list()
  
  # Replace the flagged data with NaN, and calculate the total number of bad data points
  base::lapply(rpt$listVar, function(x){
    if(base::length(rpt$listQf[[x]]) > 0 ) {
    
    #Subset the set of flags to be used
    tmp <- dfQf[,base::grep(pattern = base::paste(x,qfSens, sep ="|"), x = qfName, ignore.case = TRUE, value = TRUE), drop = FALSE]
    #Remove qfCal flags
    tmp <- tmp[,base::grep(pattern = "qfCal", x = names(tmp), invert = TRUE), drop = FALSE]
    #Calculate the number of times each quality flag was set high (qf..= 1)
    if(Vrbs == TRUE) {if(ncol(tmp) > 1){rpt$fracQfBad[[x]] <<- base::apply(X = tmp, MARGIN = 2, FUN = function(x) base::length(which(x == 1))/base::length(x))}
      else{rpt$fracQfBad[[x]] <<- base::sum(tmp)/base::nrow(tmp)
      names(rpt$fracQfBad[[x]]) <<- names(tmp)}}
    #Record the row positions for each variable where at least 1 flag is raised
    rpt$posBad[[x]] <<-  base::which(base::rowSums(tmp == 1) > 0)
    #Remove the data for each variable according to the position vector identified
    rpt$dfData[[x]][rpt$posBad[[x]]] <<- NaN 
    #Calculate the total number of bad data to be removed for each variable
    rpt$numBadSum[[x]] <<- base::length(rpt$posBad[[x]])
    } else {
      # Report NA for output if no quality flags applied to a data stream
      rpt$posBad[[x]] <<- NA
      rpt$numBadSum[[x]] <<- NA
      rpt$fracQfBad[[x]] <<- NA
    }
  })
  

  #Return output list
  return(rpt)
  
}