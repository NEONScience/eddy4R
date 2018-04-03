##############################################################################################
#' @title Defination Function to combine two daily files into one file and replacing NA values with previous value. 

#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description  Defination Function to two combine daily files into one file, replacing NA with previous value, and divide file back into daily.\cr
#' Specific for pump and soleniod valves.

#' @param dataProcDate A dataframe containing data and time in the processing date. [User-defined]
#' @param dataBgnDate A dataframe containing data and time in the day before processing date. [User-defined]
#' @param Date Character: Processing date e.g. "20170521". [-]
#' @param numCol Numeric: Define which column number in \code{dataProcDate} and \code{dataBgnDate} are being process. [-]

#' @return A dataframe including the data and time in the processing date which NA values have been replaced by previous value. [User-defined]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords eddy-covariance, NEON, NA

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2017-10-26)
#     original creation
#   Natchaya P-Durden (2018-03-28)
#     update license
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################
def.repl.na <- function(
  dataProcDate,
  dataBgnDate,
  Date,
  numCol
){
#merge daily files into one file
mergFile <- rbind(dataBgnDate, dataProcDate) 
#replace NA with previous value
mergFile[,numCol] <- zoo::na.locf(mergFile[,numCol], na.rm = FALSE)
#mergFile$timeNew <- as.POSIXct(strptime(mergFile$time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
 
#add hyphen between date
Date <- gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",Date)

#devide mergeFile into daily file
dateBgn <- strptime(paste0(Date, " ", "00:00:00", sep=""),format=("%Y-%m-%d %H:%M:%OS"), tz="UTC")
dateEnd <- strptime(paste0(Date, " ", "23:59:59.595", sep=""),format=("%Y-%m-%d %H:%M:%OS"), tz="UTC")

subFile <- subset(mergFile, mergFile$time >= dateBgn & mergFile$time <= dateEnd)
#subFile <- subFile[, !(names(subFile) %in% c("timeNew"))]

#report output
rpt <- subFile

return(rpt)

}#end of function
