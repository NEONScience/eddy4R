##############################################################################################
#' @title Definition function: to remove high frequency data points that have failed quality flags

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function  to remove high frequency data points that have failed quality flags from a data.frame
#' @param dfData Input data.frame for data to be removed from based on quality flags
#' @param dfQf Input data.frame of quality flags
#' Switch for quality flag determination for the LI7200, diag01 provides ones for passing quality by default the equals "lico". The "qfqm" method changes the ones to zeros to match the NEON QFQM logic.
#' @param Sens Character string indicating the sensor the high frequency data come from to check for sensor specific flags
#' 
#' @return A list (\code{rpt}) containing a dataframe (\code{rpt$dfData}) of the data with bad data replaced by NaN's, a vector of data variables to assess (\code{rpt$listVar}),  a list containing vectors of flag names used for each variable (\code{rpt$listQf}), a list containing vectors of flagged data points for each variable (\code{rpt$posBad}), a list containing total number of flagged data points for each variable (\code{rpt$numBad}).

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
##############################################################################################


def.qf.rmv.data <- function(
  dfData,
  dfQf,
  Sens = NULL){
  
  #Create a list to hold all the output
  rpt <- list()
  
  #Can convert to normal data.frame to prevent impact ff objects
  #rpt$dfData<- as.data.frame(dfData)
  #dfQf2 <- as.data.frame(dfQf)
  
  #Package data for reporting
  rpt$dfData <- dfData
  
  #List of variables to check for flags to remove bad data
  rpt$listVar <- names(dfData[!names(dfData) %in% c("time", "idx")])
  
  #If a sensor (Sens) is included, check for sensor specific flags to perform filtering of data    
  if(!is.null(Sens) && length(Sens) == 1){ 
    qfSens <- paste0("qf",toupper(substr(Sens, 1, 1)),substr(Sens,2,nchar(Sens))) # Sensor flags to look for i.e. qfIrga
    qfSens <- ifelse(test = qfSens == "qfSoniAmrs", yes = "qfAmrs", qfSens) #IF Sensor is the soniAmrs, change the qualifier to qfAmrs (remove soni)
  } else{
    qfSens <- NULL  
  }
  
  # Grab only qf that are integers  
  if(is.ffdf(dfQf)){
    qfName <-  names(dfQf[vmode(dfQf) == "integer"]) #method for ffdf objects
  }else{
    qfName <-  names(Filter(is.integer, dfQf))  #method for normal data.frame objects
  }
  
  # A list of all the flags to be included in the data removal  
  rpt$listQf  <- lapply(rpt$listVar, function(x){ subset(x = names(dfQf), subset = grepl(pattern = paste(x,qfSens, sep ="|"),x = qfName, ignore.case = TRUE))
  })
  
  #Add list names
  names(rpt$listQf) <- rpt$listVar
  
  # Replace the flagged data with NaN, and calculate the total number of bad data points
   lapply(rpt$listVar, function(x){
    if(length(rpt$listQf[[x]]) > 0 ) {
    
    #Subset the set of flags to be used
    tmp <- dfQf[,grep(pattern = paste(x,qfSens, sep ="|"), x = qfName, ignore.case = TRUE, value = TRUE)]
    #Calculate the number of times each quality flag was set high (qf..= 1)
    rpt$numQf[[x]] <<- apply(X = tmp, MARGIN = 2, FUN = function(x) length(which(x == 1)))
    #Record the row positions for each variable where at least 1 flag is raised
    rpt$posBad[[x]] <<-  which(rowSums(tmp == 1) > 0)
    #Remove the data for each variable according to the position vector identified
    rpt$dfData[[x]][rpt$posBad[[x]]] <<- NaN 
    #Calculate the total number of bad data to be removed for each variable
    rpt$numBad[[x]] <<- length(rpt$posBad[[x]])
    } else {
      # F
      rpt$posBad[[x]] <<- NA
      rpt$numBad[[x]] <<- NA
      rpt$numQf[[x]] <<- NA
    }
  })
  

  #Return output list
  return(rpt)
  
}