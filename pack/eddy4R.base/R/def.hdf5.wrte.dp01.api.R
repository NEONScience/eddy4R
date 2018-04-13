##############################################################################################
#' @title Definition function: Gather/Write reingested NEON Level 1 data, qfqm, and uncertainty to output HDF5

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. To write NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points), quality flags and quality metrics, and uncertainty values gathered from via the API to an output HDF5 file. 

#' @param date Character: The date for the data to be gathered.
#' @param FileOut Character: The file name for the output HDF5 file
#' @param SiteLoca Character: Site location.
#' @param DpName Character: The data product name for the data to be gathered.
#' @param LvlTowr Character: The tower level that the sensor data is being collected in NEON data product convention (HOR_VER).
#' @param TimeAgr Integer: The time aggregation index in minutes (i.e. 30)

#' @return An updated dp0p HDF5 file with dp01 data, qfqm, and uncertainty written
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords output, HDF5

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2018-02-24)
#     original creation;
#   David Durden (2018-04-02)
#     fix for missing timeEnd;
#   David Durden (2018-04-12)
#     add failsafe for missing data in API;
#     #TODO: 
#     - Create check if group level exists and create if not
#     - Add other dp01's and remove hardcoded units
##############################################################################################


def.hdf5.wrte.dp01.api <- function(
  date,
  FileOut,
  SiteLoca,
  DpName,
  LvlTowr,
  TimeAgr
){

##############################################################################

#Needed library
library(rhdf5)

############################################################################
#Convert date to lubridate object
date <- lubridate::as_datetime(date)

timeBgn <- date - lubridate::seconds(1)

timeEnd <- date + lubridate::days(1) 


#List of DP numbers by eddy4R DP names
listDpNum <- c("tempAirLvl" = "DP1.00002.001", "tempAirTop" = "DP1.00003.001")
#Determine DP number
DpNum <- listDpNum[DpName]

if(substr(DpName, 1, 4) == "temp"){TblName <- substr(DpName, 1, 4)}

# Grab 30 minute data to be written
data <- try(expr = Noble::pull.date(site = SiteLoca, dpID = DpNum, bgn.date = timeBgn, end.date = timeEnd, package = "expanded", time.agr = TimeAgr), silent = TRUE) #Currently requires to subtract 1 minute otherwise (1 index will be cut from the beginning)

#Failsafe test if API pull produced an error
if(class(data) == "try-error"){
  #Initialize lists
  rpt <- list(data = list(), qfqm = list(), ucrt = list())
  
  #get tower top level
  LvlTop <- strsplit(LvlTowr,"")
  LvlTop <- base::as.numeric(LvlTop[[1]][6])
  
  #get the sequence from top to first level
  LvlMeas<- base::seq(from = LvlTop, to = 1, by = -1)
  LvlMeas <- paste0("000_0",LvlMeas,"0",sep="")
  
  #Grabbing the measurement levels based on the sensor assembly
  if(DpName == "tempAirTop"){LvlMeas <- LvlTowr}
  if(DpName == "tempAirLvl"){LvlMeas <- LvlMeas[!LvlMeas == LvlTowr]}
  
  #Create the timeBgn vector for aggregation period specified (1, 30 minutes)
  timeBgnOut <- seq(from = lubridate::ymd_hms(timeBgn) + lubridate::seconds(1), to = base::as.POSIXlt(timeEnd) - lubridate::minutes(TimeAgr), by = paste(TimeAgr, "mins", sep = " "))
  
  #Create the timeEnd vector for aggregation period specified (1, 30 minutes)
  timeEndOut <- seq(from = lubridate::ymd_hms(timeBgn) + lubridate::minutes(TimeAgr)+ lubridate::seconds(1), to = base::as.POSIXlt(timeEnd), by = paste(TimeAgr, "mins", sep = " "))
  
  #Creating a vector of NaN's to fill data.frames
  dataNa <- rep(x = NaN, length = length(timeBgnOut))
  
  #Create the output dataframe for data values
  dataOut <- data.frame("timeBgn" = timeBgnOut, "timeEnd" = timeEndOut, "max" = dataNa, "mean" = dataNa, "min" = dataNa, "numSamp" = dataNa, "vari"= dataNa) 
  #Create the output dataframe for qfqm values
  qfqmOut <- data.frame("timeBgn" = timeBgnOut, "timeEnd" = timeEndOut, "qmAlph" = rep(x = 0.0, length = length(timeBgnOut)), "qmBeta" = rep(x = 1.0, length = length(timeBgnOut)), "qfFinl" = rep(x = 1L, length = length(timeBgnOut)), "qfSci" = rep(x = 0L, length = length(timeBgnOut))) 
  
  #Create the output dataframe for ucrt values 
  ucrtOut <- data.frame("timeBgn" = timeBgnOut, "timeEnd" = timeEndOut, "ucrtCal95" = dataNa, "se" = dataNa) 
  
  #Create list structure for the return output (type>>HOR_VER>>output_dataframes)
  lapply(LvlMeas, function(x) {
    rpt$data[[x]] <<- dataOut
    rpt$qfqm[[x]] <<- qfqmOut
    rpt$ucrt[[x]] <<- ucrtOut
    }) #End of lapply around measurement levels

#Return the filled list of data.frames
return(rpt)  
} #End of failsafe if statement 

#Convert times to POSIXct
data$startDateTime <- as.POSIXct(data$startDateTime)
data$endDateTime <- as.POSIXct(data$endDateTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")


#Set of times where endDateTime is NA
setNa <- which(is.na(data$endDateTime))

#Set the endDateTime values based off of startDateTime and TimeAgr (aggregation period)
data$endDateTime[setNa] <- data$startDateTime[setNa] + lubridate::minutes(TimeAgr)

#replace NA with NaN
data[is.na(data)] <- NaN


###############################################################################


###############################################################################
#Preparing dp01 outputs through formatting for output
###############################################################################
#Initiate output list
rpt <- list()

nameVar <- list()
#Grab the names of variables for ucrt
nameVar$Ucrt <- grep(pattern = "stdermean|expUncert", x = names(data), ignore.case = TRUE, value =  TRUE)
#Grab the names of variables for qfqm
nameVar$Qfqm <- grep(pattern = "alphaqm|betaqm|finalqf", x = names(data), ignore.case = TRUE, value =  TRUE)
#Grab the names of variables for data
nameVar$Data <- grep(pattern = "mean|variance|minimum|maximum|numpts", x = names(data), ignore.case = TRUE, value =  TRUE)
#Grab the names of variables for time
nameVar$Time <- grep(pattern = "time", x = names(data), ignore.case = TRUE, value =  TRUE)
#Exclude stdermean from data
nameVar$Data <- nameVar$Data[!nameVar$Data %in% nameVar$Ucrt]

#Align eddy4R names with DP names, i.e. dp name mapping
nameVar$DataOut <- sort(unique(sub("[.](.*)", "", nameVar$Data)))
names(nameVar$DataOut) <- c("max", "mean", "min", "numSamp", "vari") 
nameVar$QfqmOut <- sort(unique(sub("[.](.*)", "", nameVar$Qfqm)))
names(nameVar$QfqmOut) <- c("qmAlph", "qmBeta", "qfFinl", "qfSci") 
nameVar$UcrtOut <- sort(unique(sub("[.](.*)", "", nameVar$Ucrt)))
names(nameVar$UcrtOut) <- c("ucrtCal95", "se") 
nameVar$TimeOut <- sort(nameVar$Time)
names(nameVar$TimeOut) <- c("timeEnd", "timeBgn") 


#Grabbing the tower measurement levels for a given dp01 product
###############################################################################
#get tower top level
LvlTop <- strsplit(LvlTowr,"")
LvlTop <- base::as.numeric(LvlTop[[1]][6])

#get the sequence from top to first level
LvlMeas<- base::seq(from = LvlTop, to = 1, by = -1)
LvlMeas <- paste0("000.0",LvlMeas,"0",sep="")

#Determine the output levels
LvlMeasOut <- grep(pattern = paste(unique(sub(".*\\.", "",nameVar$Data)), collapse = "|"), x = LvlMeas, value = TRUE )

#Name for HDF5 output
names(LvlMeasOut) <- gsub(pattern = "\\.", replacement = "_", LvlMeasOut)

#####################################################################################

#Sort output data and apply eddy4R naming conventions
rpt$data  <- lapply(LvlMeasOut, function(x){
  #Grab just the columns to be output  
  tmp <- data[,grep(pattern = paste(nameVar$DataOut, collapse = "|"), x = names(data))]
    #Sort the output columns to grab the HOR_VER level as separate lists of dataframes
    tmp <- tmp[,grep(pattern = x, x = names(tmp))]
    #Order the column names
    tmp <- tmp[order(names(tmp))]
    #Change the output column names to eddy4R terms
    colnames(tmp) <- names(nameVar$DataOut)
    # Adding time to output
    tmp<- data.frame("timeBgn" = strftime(as.character(data$startDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(data$endDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), tmp, stringsAsFactors = FALSE)
    #Adding unit attributes and naming them
    attributes(tmp)$unit <- c("NA","NA","C","C","C","NA","C2")
    names(attributes(tmp)$unit) <- names(tmp)
    #Return output
    return(tmp)
    })

rpt$qfqm <- lapply(LvlMeasOut, function(x){
  #Grab just the columns to be output  
  tmp <- data[,grep(pattern = paste(nameVar$QfqmOut, collapse = "|"), x = names(data))]
  #Sort the output columns to grab the HOR_VER level as separate lists of dataframes
  tmp <- tmp[,grep(pattern = x, x = names(tmp))]
  #Order the column names
  tmp <- tmp[order(names(tmp))]
  #Change the output column names to eddy4R terms
  colnames(tmp) <- names(nameVar$QfqmOut)
  # Adding time to output
  tmp<- data.frame("timeBgn" = strftime(as.character(data$startDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(data$endDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), tmp, stringsAsFactors = FALSE)
  #Adding unit attributes and naming them
  attributes(tmp)$unit <- base::rep_len(x = "NA", length.out = ncol(tmp))
  names(attributes(tmp)$unit) <- names(tmp)
  #Return output
  return(tmp)
})

#Convert all NaNs in the qfSci to 0
lapply(names(rpt$qfqm), function(x){
  rpt$qfqm[[x]][is.nan(rpt$qfqm[[x]]$qfSci),"qfSci"] <<- 0L
})

rpt$ucrt <- lapply(LvlMeasOut, function(x){
  #Grab just the columns to be output  
  tmp <- data[,grep(pattern = paste(nameVar$UcrtOut, collapse = "|"), x = names(data))]
  #Sort the output columns to grab the HOR_VER level as separate lists of dataframes
  tmp <- tmp[,grep(pattern = x, x = names(tmp))]
  #Order the column names
  tmp <- tmp[order(names(tmp))]
  #Change the output column names to eddy4R terms
  colnames(tmp) <- names(nameVar$UcrtOut)
  # Adding time to output
  tmp<- data.frame("timeBgn" = strftime(as.character(data$startDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(data$endDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), tmp, stringsAsFactors = FALSE)
  #Adding unit attributes and naming them
  attributes(tmp)$unit <- c("NA","NA","C","C")
  names(attributes(tmp)$unit) <- names(tmp)
  
  #Return output
  return(tmp)
})


#############################################################################
#Writing output to existing dp0p HDF5 file
#############################################################################
#Create the file, create a class
idFile <- rhdf5::H5Fopen(FileOut)

#Create a group level for site
idSite <- rhdf5::H5Gopen(idFile, SiteLoca) 

#Open dp01 level
idDp01 <- rhdf5::H5Gopen(idSite,"dp01")

#Open type levels
idDataLvlDp01 <- rhdf5::H5Gopen(idDp01,"data")
idQfqmLvlDp01 <- rhdf5::H5Gopen(idDp01,"qfqm")
idUcrtLvlDp01 <- rhdf5::H5Gopen(idDp01,"ucrt")



#Create group structures for the added dp01 variable
idDataDp01 <- rhdf5::H5Gopen(idDataLvlDp01, DpName)
idQfqmDp01 <-  rhdf5::H5Gopen(idQfqmLvlDp01, DpName)
idUcrtDp01 <-  rhdf5::H5Gopen(idUcrtLvlDp01, DpName)

#Loop around the measurement levels
for(idx in names(LvlMeasOut)){
#idx <- names(LvlMeasOut[2])
#write attribute to the data table level for each measurement level
idLvlMeasData <- rhdf5::H5Oopen(idDataDp01,paste0(names(LvlMeasOut[idx]), "_",base::formatC(TimeAgr, width=2, flag="0"),"m"))
#write attribute to the data table level for each measurement level
idLvlMeasQfqm <- rhdf5::H5Oopen(idQfqmDp01,paste0(names(LvlMeasOut[idx]), "_",base::formatC(TimeAgr, width=2, flag="0"),"m"))
#write attribute to the data table level for each measurement level
idLvlMeasUcrt <- rhdf5::H5Oopen(idUcrtDp01,paste0(names(LvlMeasOut[idx]), "_",base::formatC(TimeAgr, width=2, flag="0"),"m"))

#Write output data
rhdf5::h5writeDataset.data.frame(obj = rpt$data[[names(LvlMeasOut[idx])]], h5loc = idLvlMeasData, name = TblName, DataFrameAsCompound = TRUE)
# Writing attributes to the data
if(!is.null(attributes(rpt$data[[names(LvlMeasOut[idx])]])$unit) == TRUE){ 
  dgid <- rhdf5::H5Dopen(idLvlMeasData, TblName)
  rhdf5::h5writeAttribute(attributes(rpt$data[[names(LvlMeasOut[idx])]])$unit, h5obj = dgid, name = "unit")
}

#Write output data
rhdf5::h5writeDataset.data.frame(obj = rpt$qfqm[[names(LvlMeasOut[idx])]], h5loc = idLvlMeasQfqm, name = TblName, DataFrameAsCompound = TRUE)
  
# Writing attributes to the qfqm
if(!is.null(attributes(rpt$qfqm[[names(LvlMeasOut[idx])]])$unit) == TRUE){ 
  dgid <- rhdf5::H5Dopen(idLvlMeasQfqm, TblName)
  rhdf5::h5writeAttribute(attributes(rpt$qfqm[[names(LvlMeasOut[idx])]])$unit, h5obj = dgid, name = "unit")
}
  
  #Write output data
  rhdf5::h5writeDataset.data.frame(obj = rpt$ucrt[[names(LvlMeasOut[idx])]], h5loc = idLvlMeasUcrt, name = TblName, DataFrameAsCompound = TRUE)
    
  # Writing attributes to the data
  if(!is.null(attributes(rpt$ucrt[[names(LvlMeasOut[idx])]])$unit) == TRUE){ 
    dgid <- rhdf5::H5Dopen(idLvlMeasUcrt, TblName)
    rhdf5::h5writeAttribute(attributes(rpt$ucrt[[names(LvlMeasOut[idx])]])$unit, h5obj = dgid, name = "unit")
  }


} #End of for loop around measurement levels

#Close all the HDF5 connections
H5close()

return(rpt)

} #End of dp01 ingest function
