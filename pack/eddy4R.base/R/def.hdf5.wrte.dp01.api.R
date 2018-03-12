##############################################################################################
#' @title Definition function: Write NEON Level 1 data, qfqm, and uncertainty to output HDF5

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. To write NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points), quality flags and quality metrics, and uncertainty values gathered from via the API to an output HDF5 file. 

#' @param date Character: The date for the data to be gathered.
#' @param FileOut Character: The file name for the output HDF5 file
#' @param SiteLoca Character: Site location.
#' @param DpNum Character: The date for the data to be gathered.
#' @param DpName Character: The date for the data to be gathered.
#' @param TimeAgr Integer: The time aggregation index in minutes (1,30)

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
#     original creation
##############################################################################################

# date <- "20170925"
# 
# SiteLoca <- "CPER"
# 
# DpName <- "tempAirLvl" #"DP1.00002.001" #SAAT
# #DpName <- "tempAirTop" "DP1.00003.001" #TRAAT
# FileOut <- "/home/ddurden/eddy/data/dev_tests/dp01/ECSE_dp0p_CPER_2017-09-25.h5"
# 
# LevlTowr <- "000_040"  
# 
# TimeAgr <- 1

def.hdf5.wrte.dp01 <- function(
  
)

##############################################################################


library(rhdf5)

############################################################################
#Convert date to lubridate object
date <- lubridate::as_datetime(date)

timeBgn <- date - lubridate::minutes(1)

timeEnd <- date + lubridate::days(1)


#List of DP numbers by eddy4R DP names
listDpNum <- c("tempAirLvl" = "DP1.00002.001", "tempAirTop" = "DP1.00003.001")
#Determine DP number
DpNum <- listDpNum[DpName]

if(substr(DpName, 1, 4) == "temp"){TblName <- substr(DpName, 1, 4)}

# Grab 30 minute data to be written
data <- Noble::pull.date(site = SiteLoca, dpID = DpNum, bgn.date = timeBgn, end.date = timeEnd, package = "expanded", time.agr = TimeAgr) #Currently requires to subtract 1 minute otherwise (1 index will be cut from the beginning)


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
LevlTop <- strsplit(LevlTowr,"")
LevlTop <- base::as.numeric(LevlTop[[1]][6])

#get the sequence from top to first level
LevlMeas<- base::seq(from = LevlTop, to = 1, by = -1)
LevlMeas <- paste0("000.0",LevlMeas,"0",sep="")

#Determine the output levels
LevlMeasOut <- grep(pattern = paste(unique(sub(".*\\.", "",nameVar$Data)), collapse = "|"), x = LevlMeas, value = TRUE )

#Name for HDF5 output
names(LevlMeasOut) <- gsub(pattern = "\\.", replacement = "_", LevlMeasOut)

#####################################################################################

#Sort output data and apply eddy4R naming conventions
rpt$data  <- lapply(LevlMeasOut, function(x){
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
    attributes(tmp)$unit <- c("NA","NA","C","C","C","C","C2")
    names(attributes(tmp)$unit) <- names(tmp)
    #Return output
    return(tmp)
    })

rpt$qfqm <- lapply(LevlMeasOut, function(x){
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

rpt$ucrt <- lapply(LevlMeasOut, function(x){
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
for(idx in names(LevlMeasOut)){
#idx <- names(LevlMeasOut[2])
#write attribute to the data table level for each measurement level
idLevlMeasData <- rhdf5::H5Oopen(idDataDp01,paste0(names(LevlMeasOut[idx]), "_",base::formatC(TimeAgr, width=2, flag="0"),"m"))
#write attribute to the data table level for each measurement level
idLevlMeasQfqm <- rhdf5::H5Oopen(idQfqmDp01,paste0(names(LevlMeasOut[idx]), "_",base::formatC(TimeAgr, width=2, flag="0"),"m"))
#write attribute to the data table level for each measurement level
idLevlMeasUcrt <- rhdf5::H5Oopen(idUcrtDp01,paste0(names(LevlMeasOut[idx]), "_",base::formatC(TimeAgr, width=2, flag="0"),"m"))

#Write output data
rhdf5::h5writeDataset.data.frame(obj = rpt$data[[names(LevlMeasOut[idx])]], h5loc = idLevlMeasData, name = TblName, DataFrameAsCompound = TRUE)
# Writing attributes to the data
if(!is.null(attributes(rpt$data[[names(LevlMeasOut[idx])]])$unit) == TRUE){ 
  dgid <- rhdf5::H5Dopen(idLevlMeasData, TblName)
  rhdf5::h5writeAttribute(attributes(rpt$data[[names(LevlMeasOut[idx])]])$unit, h5obj = dgid, name = "unit")
}

#Write output data
rhdf5::h5writeDataset.data.frame(obj = rpt$qfqm[[names(LevlMeasOut[idx])]], h5loc = idLevlMeasQfqm, name = TblName, DataFrameAsCompound = TRUE)
  
# Writing attributes to the qfqm
if(!is.null(attributes(rpt$qfqm[[names(LevlMeasOut[idx])]])$unit) == TRUE){ 
  dgid <- rhdf5::H5Dopen(idLevlMeasQfqm, TblName)
  rhdf5::h5writeAttribute(attributes(rpt$qfqm[[names(LevlMeasOut[idx])]])$unit, h5obj = dgid, name = "unit")
}
  
  #Write output data
  rhdf5::h5writeDataset.data.frame(obj = rpt$ucrt[[names(LevlMeasOut[idx])]], h5loc = idLevlMeasUcrt, name = TblName, DataFrameAsCompound = TRUE)
    
  # Writing attributes to the data
  if(!is.null(attributes(rpt$ucrt[[names(LevlMeasOut[idx])]])$unit) == TRUE){ 
    dgid <- rhdf5::H5Dopen(idLevlMeasUcrt, TblName)
    rhdf5::h5writeAttribute(attributes(rpt$ucrt[[names(LevlMeasOut[idx])]])$unit, h5obj = dgid, name = "unit")
  }


} #End of for loop around measurement levels

#Close all the HDF5 connections
H5close()
