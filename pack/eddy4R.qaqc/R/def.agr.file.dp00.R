##############################################################################################
#' @title Combine individual L0 data streams from file

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description 
#' Definition function. Combine (and regularize) L0 data streams downloaded in individual files from the L0 sandbox tool. 

#' @param \code{dirFile} Optional. A character string indicating the directory in which the files to combine are stored. Default is the current working directory.
#' @param \code{nameFile} Required. A character vector of file names holding the L0 data streams to combine. Must be in .csv format. Within these files, the first column must containing the measurement time, the second column must contain one L0 data stream
#' @param \code{nameVar} Required. A character vector of the same length as nameFile with the variable names of the L0 data streams
#' @param \code{unitVar} Required. A character vector of the same length as nameFile with the variable units
#' @param \code{Freq} Required. A numeric value indicating the expected frequency [Hz] of L0 data within the files in nameFile
#' @param \code{FmtTime} Optional. An character format string to interpret the time values in the first column of each file in nameFile. Default is \%d-\%b-\%Y \%I.\%M.\%OS \%p
#' @param \code{Tz} Optional. A character string specifying the time zone in with the time values in the first column of each file in nameFile are represented. Default is GMT

#' @return A list of: \cr
#' \code{time} a POSIXlt vector of regularized times corresponding to each row in data. Limits are the min and max of times found within nameFile \cr
#' \code{data} a named data frame containing the regularized time series of L0 variables found within nameFile

#' @references Currently none

#' @keywords NEON, L0

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-11-03)
#     original creation
##############################################################################################

def.agr.file.dp00 <- function (
  dirFile = '.', # Directory where input files can be found
  nameFile,
  nameVar,
  unitVar,
  Freq,
  FmtTime = "%d-%b-%Y %I.%M.%OS %p",
  Tz = "GMT"
) {
  

timeRglr <- list() # Initialize
dataRglr <- list() # initialize
for(idxFile in 1:base::length(nameFile)){
  # Read the file
  data <- read.csv(paste0(dirFile,'/',nameFile[idxFile])) 
  
  # Pull out and convert date strings 
  timeInp <- strptime(data[[1]],format=FmtTime,tz=Tz)

  # Do check to see if times were interpreted correctly 
  if (sum(is.na(timeInp)) == length(timeInp)) {
    stop("Check format string for interpreting timestamp. All time are listed as NA.")
  }
  
  # Pull the L0 data stream
  data <- data[[2]]
  
  # Regularize time series. 
  data <- eddy4R.base::def.rglr(timeMeas=timeInp,dataMeas=as.data.frame(data),unitMeas=unitVar[idxFile],FreqRglr=Freq,MethRglr="cybiDflt")
  timeRglr[[idxFile]] <- data$timeRglr 
  dataRglr[[idxFile]] <- data$dataRglr[[1]]
  
  # Grab min and max times over all files
  if(idxFile == 1) {
    timeMin <- base::min(timeRglr[[idxFile]])
    timeMax <- base::max(timeRglr[[idxFile]])
  } else {
    if (base::min(timeRglr[[idxFile]]) < timeMin) {
      timeMin <- base::min(timeRglr[[idxFile]]) # Update min
    }
    if (base::max(timeRglr[[idxFile]]) > timeMax) {
      timeMax <- base::max(timeRglr[[idxFile]]) # update Max
    }
  }
  
}


# Now that we have all the data and it's regularized, make one data frame with aligned times 
time <- base::seq.POSIXt(from=timeMin,to=timeMax,by=1/Freq)
data <- base::matrix(data=NA,nrow=base::length(time),ncol=base::length(nameFile))
for(idxFile in 1:base::length(nameFile)){
  # Find start and end 
  posBgn <- base::which(time == timeRglr[[idxFile]][1])
  posEnd <- base::which(time == timeRglr[[idxFile]][base::length(timeRglr[[idxFile]])])
  
  # Put the data in the correct place
  data[posBgn:posEnd,idxFile] <- dataRglr[[idxFile]]
  
}
data <- as.data.frame(data) # Make a data frame. 
names(data) <- nameVar # Assign variable names
data <- eddy4R.base::def.unit.conv(data = data,unitFrom = unitVar,unitTo=unitVar) # Assign units

# Return
return(list(time=time,data=data))

}
