##############################################################################################
#' @title Definition function: Write NEON Level 1 data, qfqm, and uncertainty to output HDF5

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. To write NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points), quality flags and quality metrics, and uncertainty values gathered from via the API to an output HDF5 file. 

#' @param date Character: The date for the data to be gathered.
#' @param FileOut Character: The file name for the output HDF5 file
#' @param SiteLoca Character: Site location.
#' @param dpNum Character: The date for the data to be gathered.
#' @param dpName Character: The date for the data to be gathered.

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

date <- "20170925"

SiteLoca <- "CPER"

DpName <- "tempAirLvl" #"DP1.00002.001" #SAAT
#DpName <- "tempAirTop" "DP1.00003.001" #TRAAT
FileOut <- 

LevlTowr <- "000_040"  
##############################################################################



##TO DO Add functionality for DpName
############################################################################
#Grab all NEON data products  
#prod <- nneo::nneo_products()

#Extract just air temperature products
#tempVar <- prod[grep(pattern = "air temperature",x = prod$productName),]
############################################################################
#Convert date to lubridate object
date <- lubridate::as_datetime(date)

timeBgn <- date - lubridate::minutes(1)

timeEnd <- date + lubridate::days(1)

timeAgr <- 30

#List of DP numbers by eddy4R DP names
listDpNum <- c("tempAirLvl" = "DP1.00002.001", "tempAirTop" = "DP1.00003.001")
#Determine DP number
DpNum <- listDpNum[DpName]

if(substr(DpName, 1, 4) == "temp"){TblName <- substr(DpName, 1, 4)}

# Grab 30 minute data to be written
data <- Noble::pull.date(site = SiteLoca, dpID = DpNum, bgn.date = timeBgn, end.date = timeEnd, package = "expanded", time.agr = timeAgr) #Currently requires to subtract 1 minute otherwise (1 index will be cut from the beginning)


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
idFile <- H5Fopen(FileOut)

#Create a group level for site
idSite <- H5Gopen(idFile, SiteLoca) 

#Open dp01 level
idDp01 <- H5Gopen(idSite,"dp01")

#Open type levels
idDataLvlDp01 <- H5Gopen(idDp01,"data")
idQfqmLvlDp01 <- H5Gopen(idDp01,"qfqm")
idUcrtLvlDp01 <-H5Gopen(idDp01,"ucrt")



#Create group structures for the added dp01 variable
idDataDp01 <- H5Gcreate(idDataLvlDp01, DpName))
idQfqmDp01 <-  H5Gcreate(idQfqmLvlDp01, DpName))
idUcrtDp01 <-  H5Gcreate(idUcrtLvlDp01, DpName))

for(idx in seq(LevlMeasOut)){
  print(idx)

#write attribute to the data table level for each measurement level
idLevlMeasData <- H5Oopen(,paste0(names(LevlMeasOut), "_",timeAgr,"m"))

}

H5close()
