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
#' @param Tokn Optional. Character string. An API token allowing additional permissions/access.

#' @return An updated dp0p HDF5 file with dp01 data, qfqm, and uncertainty written

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords output, HDF5

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   David Durden (2018-02-24)
#     original creation
#   Natchaya P-Durden (2018-03-28)
#     removed comment lines from the header
#   Natchaya P-Durden (2018-03-30)
#     applied term name convention; replace Levl by Lvl
#   David Durden (2018-04-02)
#     fix for missing timeEnd;
#   David Durden (2018-04-12)
#     add failsafe for missing data in API;
#     #TODO:
#     - Create check if group level exists and create if not
#     - Add other dp01's and remove hardcoded units
#   Natchaya P-Durden (2019-06-10)
#     adding additional data products
#   Natchaya P-Durden (2019-09-12)
#     get information of existing dp01 hor and ver from dp0p hdf5 file
#     convert qmBeta and qmAlph to fraction
#   David Durden(2020-07-02)
#     updating function to check physical locations of reingest sensors exist before pulling from API
#   Chris Florian (2022-05-09)
#     updating to use SOM API function due to Noble package bug that replicated data from the highest horver in a missing horver
 
##############################################################################################

def.hdf5.wrte.dp01.api <- function(
  date,
  FileOut,
  SiteLoca,
  DpName,
  LvlTowr,
  TimeAgr,
  Tokn = NULL
){

##############################################################################

#Needed library
library(rhdf5)
library(neonUtilities)
#TODO:
#Create check if group level exists and create if not
#Add other dp01's and remove hardcoded units
############################################################################
#Convert date to lubridate object
date <- lubridate::as_datetime(date)
#get only year and month
yearMnth <- as.character.Date(date, format = "%Y-%m")

timeBgn <- date 

timeEnd <- date + lubridate::days(1)

#assign data unit attributes
outAttr <- base::list()
outAttr$data$tempAirLvl <- c("NA","NA","C","C","C","NA","C2")
outAttr$data$tempAirTop <- outAttr$data$tempAirLvl
outAttr$data$fluxHeatSoil <- c("NA","NA","W m-2","W m-2","W m-2","NA","W2 m-4")
outAttr$data$radiNet <- c("NA","NA","W m-2","W m-2","W m-2","NA","W2 m-4")
outAttr$data$tempSoil <- outAttr$data$tempAirLvl
outAttr$data$h2oSoilVol <- c("NA","NA","cm3 cm-3","cm3 cm-3","cm3 cm-3","NA","cm6 cm-6")
outAttr$data$ionSoilVol <- c("NA","NA","-","-","-","NA","-")
outAttr$data$presBaro <- c("NA","NA","kPa","kPa","kPa","NA","kPa2")
outAttr$data$presCor <- c("NA", "NA", "kPa")
#assign uncertainty unit attributes
outAttr$ucrt$tempAirLvl <- c("NA","NA","C","C")
outAttr$ucrt$tempAirTop <- outAttr$ucrt$tempAirLvl
outAttr$ucrt$fluxHeatSoil <- c("NA","NA","W m-2","W m-2")
outAttr$ucrt$radiNet <- c("NA","NA","W m-2","W m-2")
outAttr$ucrt$tempSoil <- outAttr$ucrt$tempAirLvl
outAttr$ucrt$h2oSoilVol <- c("NA","NA","cm3 cm-3","cm3 cm-3")
outAttr$ucrt$ionSoilVol <- c("NA","NA","-","-")
outAttr$ucrt$presBaro <- c("NA","NA","kPa","kPa")
outAttr$ucrt$presCor <- c("NA", "NA", "kPa")

#List of DP numbers by eddy4R DP names
listDpNum <- c("tempAirLvl" = "DP1.00002.001", "tempAirTop" = "DP1.00003.001", "fluxHeatSoil" = "DP1.00040.001",
               "radiNet" = "DP1.00023.001", "tempSoil" = "DP1.00041.001", "h2oSoilVol" = "DP1.00094.001",
               "presBaro" = "DP1.00004.001")
#Determine DP number
DpNum <- listDpNum[DpName]

#assign table name for each DP
if(substr(DpName, 1, 4) == "temp"){TblName <- substr(DpName, 1, 4)}
if(DpName == "fluxHeatSoil") TblName <- "fluxHeatSoil"
if(DpName == "radiNet") TblName <- c("radiLwIn", "radiSwIn", "radiLwOut", "radiSwOut")
if(DpName == "h2oSoilVol") TblName <- c("ionSoilVol", "h2oSoilVol")
if(DpName == "presBaro") TblName <- c("presCor", "presAtm")

#Grab 30 minute data to be written
msg <- paste0("downloading ", TimeAgr, " min data from the portal")
tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
#old Noble data pull
#data <- try(expr = Noble::pull.date(site = SiteLoca, dpID = DpNum, bgn.date = timeBgn, end.date = timeEnd, package = "expanded", time.agr = TimeAgr), silent = TRUE) #Currently requires to subtract 1 minute otherwise (1 index will be cut from the beginning)

data <- list()

for(idxLvl in LvlTowr[[DpName]]){
  #idxLvl <- LvlTowr[[DpName]][2]
  #extract hor and ver for each data product as a character string of three values e.g. "000" and "010"
  locHor <- substr(idxLvl, start = 1, stop = 3) 
  locVer <- substr(idxLvl, start = 5, stop = 7) 
  #pad TimeAgr with zero/s to fit the expected three digit format for wndwAgr 
  if(nchar(TimeAgr) == 1){
    wndwAgr <- paste0("00", TimeAgr)
  } else if(nchar(TimeAgr) == 2){
    wndwAgr <- paste0("0", TimeAgr)
  } 
  
  data[[idxLvl]] <- try(expr = som::def.neon.api.get.data(site = SiteLoca, idDpMain = DpNum, locHor = locHor, locVer = locVer, wndwAgr = wndwAgr, year = lubridate::year(timeBgn), mnth = lubridate::month(timeBgn), Pack = "expanded", Tokn = Tokn), silent = TRUE) #need to generalize TimeAgr to wndwAgr, Time Agr is 1,30 and wndwAgr needs "001" and "030"
  if(class(data[[idxLvl]]) == "try-error") rlog$info(paste0("Error in API call for ",DpName," at ",idxLvl,": ",data[[idxLvl]]))
}

#compile noble-like data format if not all the datasets have a try-error class
if(!all(sapply(data, class) == "try-error")){
  
  #Data for filling if necessary
  whrData <- which(lapply(data,class) == "data.frame")[1]
  #get timestamps
  timeBgnOut <- data[[whrData]][which(grepl(x = names(data[[whrData]]), pattern = "startdatetime", ignore.case = T))]
  timeEndOut <- data[[whrData]][which(grepl(x = names(data[[whrData]]), pattern = "enddatetime", ignore.case = T))]
  colNamesOut <- colnames(data[[whrData]])
  #create dataframe of NaNs to fill with
  dataFill <- data.frame(matrix(NaN, nrow=nrow(timeBgnOut), ncol=length(colnames(data[[whrData]])[which(!grepl(x = names(data[[whrData]]), pattern = "time", ignore.case = T))])))
  
  #fill in the try error datasets and paste horver to names
  for(idxLvl in 1:length(data)){
    if(class(data[[idxLvl]]) == "try-error"){
      
      #create placeholder data.frame
      data[[idxLvl]] <- data.frame(timeBgnOut, timeEndOut, dataFill)
      #apply names 
      colnames(data[[idxLvl]]) <- colNamesOut
     
      #fill QFQM
      data[[idxLvl]]$alphaQM <- 0.0
      data[[idxLvl]]$betaQM <- 100.0
      data[[idxLvl]]$finalQF <- 1L
      data[[idxLvl]]$finalQFSciRvw <- 0L
    }
    #idxLvl <- 1
    LocMeas <- gsub("\\_", ".", names(data[idxLvl]))
    #append horver to names of each ML's dataset
    colnames(data[[idxLvl]])[which(!grepl(x = names(data[[idxLvl]]), pattern = "time", ignore.case = T))] <- 
      paste0(colnames(data[[idxLvl]][which(!grepl(x = names(data[[idxLvl]]), pattern = "time", ignore.case = T))]), ".", LocMeas)
  }
  
  #remove data fill 
  rm(dataFill)
  
  #join list of data into wide format
  
  data <- data.frame(base::Reduce(function(x, y) merge(x, y, all=TRUE), data))
  
  #restrict API data to the processing date, TODO: is there any way to grab data from the API for just one day?
  
  data <- data[grep(pattern = date, x = data$startDateTime),]
  
}




#Failsafe test if API pull produced an error
if(all(sapply(data, class) == "try-error")){
  #Initialize lists
  rpt <- list(data = list(), qfqm = list(), ucrt = list())
  #get sensor HOR and VER
  LocMeas <- gsub("\\_", ".", LvlTowr[[DpName]])
  LvlMeas <- LvlTowr[[DpName]]

  #Determine the output levels
  LvlMeasOut <- LocMeas
  #Name for HDF5 output
  names(LvlMeasOut) <- LvlMeas

  #Create the timeBgn vector for aggregation period specified (1, 30 minutes)
  timeBgnOut <- seq(from = timeBgn, to = base::as.POSIXlt(timeEnd) - lubridate::minutes(TimeAgr), by = paste(TimeAgr, "mins", sep = " "))

  #Create the timeEnd vector for aggregation period specified (1, 30 minutes)
  timeEndOut <- seq(from = timeBgn + lubridate::minutes(TimeAgr), to = base::as.POSIXlt(timeEnd), by = paste(TimeAgr, "mins", sep = " "))

  #Creating a vector of NaN's to fill data.frames
  dataNa <- rep(x = NaN, length = length(timeBgnOut))

  #Create the output dataframe for data values
  dataOut <- data.frame("timeBgn" = strftime(as.character(timeBgnOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(timeEndOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "max" = dataNa, "mean" = dataNa, "min" = dataNa, "numSamp" = dataNa, "vari"= dataNa, stringsAsFactors = FALSE)

  #Adding unit attributes and naming them
  attributes(dataOut)$unit <- outAttr$data[[DpName]]
  names(attributes(dataOut)$unit) <- names(dataOut)

  #Create the output dataframe for qfqm values
  qfqmOut <- data.frame("timeBgn" = strftime(as.character(timeBgnOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(timeEndOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "qmAlph" = rep(x = 0.0, length = length(timeBgnOut)), "qmBeta" = rep(x = 1.0, length = length(timeBgnOut)), "qfFinl" = rep(x = 1L, length = length(timeBgnOut)), "qfSci" = rep(x = 0L, length = length(timeBgnOut)), stringsAsFactors = FALSE)

  #Setting attributes
  attributes(qfqmOut)$unit <- base::rep_len(x = "NA", length.out = ncol(qfqmOut))
  names(attributes(qfqmOut)$unit) <- names(qfqmOut)

  #Create the output dataframe for ucrt values
  ucrtOut <- data.frame("timeBgn" = strftime(as.character(timeBgnOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(timeEndOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "ucrtCal95" = dataNa, "se" = dataNa, stringsAsFactors = FALSE)

  #Adding unit attributes and naming them
  attributes(ucrtOut)$unit <- outAttr$ucrt[[DpName]]
  names(attributes(ucrtOut)$unit) <- names(ucrtOut)

  #Create list structure for the return output (type>>HOR_VER>>output_dataframes)
  lapply(LvlMeas, function(x) {
    lapply (TblName, function(y) {
      if (y %in% "presCor"){
        #Create the output dataframe for data values
        dataOut <- data.frame("timeBgn" = strftime(as.character(timeBgnOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(timeEndOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "mean" = dataNa, stringsAsFactors = FALSE)
        #Create the output dataframe for qfqm values
        qfqmOut <- data.frame("timeBgn" = strftime(as.character(timeBgnOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(timeEndOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "qfDew" = rep(x = 1L, length = length(timeBgnOut)), "qfFinl" = rep(x = 1L, length = length(timeBgnOut)), "qfSci" = rep(x = 0L, length = length(timeBgnOut)), "qfTemp" = rep(x = 1L, length = length(timeBgnOut)), stringsAsFactors = FALSE)
        #Create the output dataframe for ucrt values
        ucrtOut <- data.frame("timeBgn" = strftime(as.character(timeBgnOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(timeEndOut), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "ucrtCal95" = dataNa, stringsAsFactors = FALSE)
      }
      rpt$data[[x]][[y]] <<- dataOut
      rpt$qfqm[[x]][[y]] <<- qfqmOut
      rpt$ucrt[[x]][[y]] <<- ucrtOut
      #replace unit for ionSoilVol and presCor
      if (y %in% c("ionSoilVol", "presCor")){
        attributes(rpt$data[[x]][[y]])$unit <<- outAttr$data[[y]]
        attributes(rpt$qfqm[[x]][[y]])$unit <<- base::rep_len(x = "NA", length.out = ncol(rpt$qfqm[[x]][[y]]))
        attributes(rpt$ucrt[[x]][[y]])$unit <<- outAttr$ucrt[[y]]
        names(attributes(rpt$data[[x]][[y]])$unit) <<- names(rpt$data[[x]][[y]])
        names(attributes(rpt$qfqm[[x]][[y]])$unit) <<- names(rpt$qfqm[[x]][[y]])
        names(attributes(rpt$ucrt[[x]][[y]])$unit) <<- names(rpt$ucrt[[x]][[y]])
        }
      })}) #End of lapply around measurement levels

} else {

#Convert times to POSIXct
data$startDateTime <- as.POSIXct(data$startDateTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
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
tmp <- list()

nameVar <- list()
#Grab the names of variables for ucrt
nameVar$Ucrt <- grep(pattern = "stdermean|expUncert", x = names(data), ignore.case = TRUE, value =  TRUE)
#Grab the names of variables for qfqm
if (DpName %in% "presBaro"){
  nameVar$Qfqm <- grep(pattern = "alphaqm|betaqm|qf", x = names(data), ignore.case = TRUE, value =  TRUE)
  #Grab the names of variables for data
  nameVar$Data <- grep(pattern = "mean|variance|minimum|maximum|numpts|corPres", x = names(data), ignore.case = TRUE, value =  TRUE)
  #Exclude corPres qfqm and ucrt from data
  nameVar$Data <- nameVar$Data[!nameVar$Data %in% nameVar$Ucrt]
  nameVar$Data <- nameVar$Data[!nameVar$Data %in% nameVar$Qfqm]
} else{
  nameVar$Qfqm <- grep(pattern = "alphaqm|betaqm|finalqf", x = names(data), ignore.case = TRUE, value =  TRUE)
  #Grab the names of variables for data
  nameVar$Data <- grep(pattern = "mean|variance|minimum|maximum|numpts", x = names(data), ignore.case = TRUE, value =  TRUE)
  #Exclude stdermean from data
  nameVar$Data <- nameVar$Data[!nameVar$Data %in% nameVar$Ucrt]
}

#Grab the names of variables for time
nameVar$Time <- grep(pattern = "time", x = names(data), ignore.case = TRUE, value =  TRUE)

#Align eddy4R names with DP names, i.e. dp name mapping
nameVar$DataOut <- sort(unique(sub("[.](.*)", "", nameVar$Data)))
nameVar$QfqmOut <- sort(unique(sub("[.](.*)", "", nameVar$Qfqm)))
nameVar$UcrtOut <- sort(unique(sub("[.](.*)", "", nameVar$Ucrt)))

if (DpName %in% "presBaro"){
  names(nameVar$DataOut) <- c("mean", "max", "mean", "min", "numSamp", "vari")
  names(nameVar$QfqmOut) <- c("qfDew", "qfFinl", "qfSci", "qfTemp", "qmAlph", "qmBeta", "qfFinl", "qfSci")
  names(nameVar$UcrtOut) <- c("ucrtCal95", "ucrtCal95", "se")
} else{
  names(nameVar$DataOut) <- rep(c("max", "mean", "min", "numSamp", "vari"), length(nameVar$DataOut)/5)
  names(nameVar$QfqmOut) <- rep(c("qmAlph", "qmBeta", "qfFinl", "qfSci"), length(nameVar$QfqmOut)/4)
  names(nameVar$UcrtOut) <- rep(c("ucrtCal95", "se"), length(nameVar$UcrtOut)/2)
}

nameVar$TimeOut <- sort(nameVar$Time)
names(nameVar$TimeOut) <- c("timeEnd", "timeBgn")


#Grabbing the tower measurement levels for a given dp01 product
###############################################################################
#get vertical and horizontal measurement location
#get sensor HOR and VER
LocMeas <- gsub("\\_", ".", LvlTowr[[DpName]])
LvlMeas <- LvlTowr[[DpName]]

#Determine the output levels
LvlMeasOut <- LocMeas
#Name for HDF5 output
names(LvlMeasOut) <- LvlMeas

#Check measurement levels returned from API
LvlExis <- unique(unlist(regmatches(names(data),gregexpr(pattern = "[0-9][0-9][0-9].[)0-9][0-9][0-9]", names(data)))))

#Check against the measurement location
LvlMeasOut <- LvlMeasOut[LvlMeasOut %in% LvlExis]

#####################################################################################

#Sort output data and apply eddy4R naming conventions
tmp$data  <- lapply(LvlMeasOut, function(x){
  #print(x)
  #Grab just the columns to be output
  tmp <- data[,grep(pattern = paste(nameVar$DataOut, collapse = "|"), x = names(data))]
  if(DpName %in% "presBaro"){
  tmp <- tmp[,-grep(pattern = "QF|Uncert", x = colnames(tmp))]}
    #Sort the output columns to grab the HOR_VER level as separate lists of dataframes
    tmp <- tmp[,grep(pattern = x, x = names(tmp))]
    #Order the column names
    tmp <- tmp[order(names(tmp))]
    #Change the output column names to eddy4R terms
    colnames(tmp) <- names(nameVar$DataOut)
    #Adding time to output
    #tmp<- data.frame("timeBgn" = strftime(as.character(data$startDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(data$endDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), tmp, stringsAsFactors = FALSE)
    #Adding unit attributes and naming them
    #attributes(tmp)$unit <- outAttr$data[[DpName]]
    #names(attributes(tmp)$unit) <- names(tmp)
    #Return output
    return(tmp)
    })

#extract subdata products
for (idxLvl in names(tmp$data)){
  for (idxSupDp in 1:length(TblName)){
    #determine begin and end columns
    if (DpName %in% "presBaro"){
      if (idxSupDp == 1){
        bgn <- idxSupDp
        end <- idxSupDp
      } else {
        bgn <- idxSupDp
        end <- length(outAttr$data[[DpName]])-1
      }
    } else {
      bgn <- (idxSupDp*(length(outAttr$data[[DpName]])-2)) - (((length(outAttr$data[[DpName]])-2))-1)
      end <- idxSupDp*(length(outAttr$data[[DpName]])-2)
    }
    rpt$data[[idxLvl]][[TblName[idxSupDp]]] <- data.frame("timeBgn" = strftime(as.character(data$startDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(data$endDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), tmp$data[[idxLvl]][,bgn:end], stringsAsFactors = FALSE)
    #re column name for presCor
    if (TblName[idxSupDp] %in% "presCor") { names(rpt$data[[idxLvl]][[TblName[idxSupDp]]]) <- c("timeBgn", "timeEnd", "mean")}
    #Adding unit attributes and naming them
    if(TblName[idxSupDp] %in% c("ionSoilVol", "presCor")) {
      attributes(rpt$data[[idxLvl]][[TblName[idxSupDp]]])$unit <- outAttr$data[[TblName[idxSupDp]]]
    } else {
      attributes(rpt$data[[idxLvl]][[TblName[idxSupDp]]])$unit <- outAttr$data[[DpName]]
      }
    names(attributes(rpt$data[[idxLvl]][[TblName[idxSupDp]]])$unit) <- names(rpt$data[[idxLvl]][[TblName[idxSupDp]]])
  }
}

tmp$qfqm <- lapply(LvlMeasOut, function(x){
  #Grab just the columns to be output
  tmp <- data[,grep(pattern = paste(nameVar$QfqmOut, collapse = "|"), x = names(data))]
  #Sort the output columns to grab the HOR_VER level as separate lists of dataframes
  tmp <- tmp[,grep(pattern = x, x = names(tmp))]
  #Order the column names
  tmp <- tmp[order(names(tmp))]
  #Change the output column names to eddy4R terms
  colnames(tmp) <- names(nameVar$QfqmOut)
  # # Adding time to output
  # tmp<- data.frame("timeBgn" = strftime(as.character(data$startDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(data$endDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), tmp, stringsAsFactors = FALSE)
  # #Adding unit attributes and naming them
  # attributes(tmp)$unit <- base::rep_len(x = "NA", length.out = ncol(tmp))
  # names(attributes(tmp)$unit) <- names(tmp)
  #Return output
  return(tmp)
})

#extract subdata products
for (idxLvl in names(tmp$qfqm)){
  for (idxSupDp in 1:length(TblName)){
    #determine begin and end columns
    bgn <- (idxSupDp*4 - 3)
    end <- idxSupDp*4
    rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]] <- data.frame("timeBgn" = strftime(as.character(data$startDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(data$endDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), tmp$qfqm[[idxLvl]][,bgn:end], stringsAsFactors = FALSE)
    #Adding unit attributes and naming them
    attributes(rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]])$unit <- base::rep_len(x = "NA", length.out = ncol(rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]))
    names(attributes(rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]])$unit) <- names(rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]])
    #Convert all NaNs in the qfSci to 0
    rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]][is.nan(rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]$qfSci),"qfSci"] <- 0L
    if (!(TblName[idxSupDp] %in% "presCor")){
    #Convert all NaNs in the qmAlph and qmBeta to 100
    rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]][is.nan(rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]$qmAlph),"qmAlph"] <- 0L
    rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]][is.nan(rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]$qmBeta),"qmBeta"] <- 100L
    #Convert unit of qmAlph and qmBeta to fraction
    rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]$qmAlph <- (rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]$qmAlph)/100
    rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]$qmBeta <- (rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]$qmBeta)/100
    } else {
      rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]][is.nan(rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]$qfDew),"qfDew"] <- 1L
      rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]][is.nan(rpt$qfqm[[idxLvl]][[TblName[idxSupDp]]]$qfTemp),"qfTemp"] <- 1L
    }
  }
}
# #Convert all NaNs in the qfSci to 0
# lapply(names(rpt$qfqm), function(x){
#   rpt$qfqm[[x]][is.nan(rpt$qfqm[[x]]$qfSci),"qfSci"] <<- 0L
# })

tmp$ucrt <- lapply(LvlMeasOut, function(x){
  #Grab just the columns to be output
  tmp <- data[,grep(pattern = paste(nameVar$UcrtOut, collapse = "|"), x = names(data))]
  #Sort the output columns to grab the HOR_VER level as separate lists of dataframes
  tmp <- tmp[,grep(pattern = x, x = names(tmp))]
  #Order the column names
  tmp <- tmp[order(names(tmp))]
  #Change the output column names to eddy4R terms
  colnames(tmp) <- names(nameVar$UcrtOut)
  # Adding time to output
  #tmp<- data.frame("timeBgn" = strftime(as.character(data$startDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(data$endDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), tmp, stringsAsFactors = FALSE)
  #Adding unit attributes and naming them
  #attributes(tmp)$unit <- outAttr$ucrt[[DpName]]
  #names(attributes(tmp)$unit) <- names(tmp)

  #Return output
  return(tmp)
})
#extract subdata products
for (idxLvl in names(tmp$ucrt)){
  for (idxSupDp in 1:length(TblName)){
    #determine begin and end columns
    if (DpName %in% "presBaro"){
      if (idxSupDp == 1){
        bgn <- idxSupDp
        end <- idxSupDp
      } else {
        bgn <- idxSupDp
        end <- length(outAttr$ucrt[[DpName]])-1
      }
    } else {
      bgn <- (idxSupDp*(length(outAttr$ucrt[[DpName]])-2)) - (((length(outAttr$ucrt[[DpName]])-2))-1)
      end <- idxSupDp*(length(outAttr$ucrt[[DpName]])-2)
    }
    rpt$ucrt[[idxLvl]][[TblName[idxSupDp]]] <- data.frame("timeBgn" = strftime(as.character(data$startDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), "timeEnd" = strftime(as.character(data$endDateTime), format= "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), tmp$ucrt[[idxLvl]][,bgn:end], stringsAsFactors = FALSE)
    #re column name for presCor
    if (TblName[idxSupDp] %in% "presCor") { names(rpt$ucrt[[idxLvl]][[TblName[idxSupDp]]]) <- c("timeBgn", "timeEnd", "ucrtCal95")}
    #Adding unit attributes and naming them
    if (TblName[idxSupDp] %in% c("ionSoilVol", "presCor")) {attributes(rpt$ucrt[[idxLvl]][[TblName[idxSupDp]]])$unit <- outAttr$ucrt[[TblName[idxSupDp]]]
    } else {
      attributes(rpt$ucrt[[idxLvl]][[TblName[idxSupDp]]])$unit <- outAttr$ucrt[[DpName]]
      }
    names(attributes(rpt$ucrt[[idxLvl]][[TblName[idxSupDp]]])$unit) <- names(rpt$ucrt[[idxLvl]][[TblName[idxSupDp]]])
  }
}
} #End of else statement
##############################################################################
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

for (idxSupDp in TblName){
#Write output data
rhdf5::h5writeDataset.data.frame(obj = rpt$data[[names(LvlMeasOut[idx])]][[idxSupDp]], h5loc = idLvlMeasData, name = idxSupDp, DataFrameAsCompound = TRUE)
# Writing attributes to the data
if(!is.null(attributes(rpt$data[[names(LvlMeasOut[idx])]][[idxSupDp]])$unit) == TRUE){
  dgid <- rhdf5::H5Dopen(idLvlMeasData, idxSupDp)
  rhdf5::h5writeAttribute(attributes(rpt$data[[names(LvlMeasOut[idx])]][[idxSupDp]])$unit, h5obj = dgid, name = "unit")
}

#Write output data
rhdf5::h5writeDataset.data.frame(obj = rpt$qfqm[[names(LvlMeasOut[idx])]][[idxSupDp]], h5loc = idLvlMeasQfqm, name = idxSupDp, DataFrameAsCompound = TRUE)

# Writing attributes to the qfqm
if(!is.null(attributes(rpt$qfqm[[names(LvlMeasOut[idx])]][[idxSupDp]])$unit) == TRUE){
  dgid <- rhdf5::H5Dopen(idLvlMeasQfqm, idxSupDp)
  rhdf5::h5writeAttribute(attributes(rpt$qfqm[[names(LvlMeasOut[idx])]][[idxSupDp]])$unit, h5obj = dgid, name = "unit")
}

  #Write output data
  rhdf5::h5writeDataset.data.frame(obj = rpt$ucrt[[names(LvlMeasOut[idx])]][[idxSupDp]], h5loc = idLvlMeasUcrt, name = idxSupDp, DataFrameAsCompound = TRUE)

  # Writing attributes to the data
  if(!is.null(attributes(rpt$ucrt[[names(LvlMeasOut[idx])]][[idxSupDp]])$unit) == TRUE){
    dgid <- rhdf5::H5Dopen(idLvlMeasUcrt, idxSupDp)
    rhdf5::h5writeAttribute(attributes(rpt$ucrt[[names(LvlMeasOut[idx])]][[idxSupDp]])$unit, h5obj = dgid, name = "unit")
  }
}


} #End of for loop around measurement levels
#Close all the HDF5 connections
h5closeAll()

return(rpt)

} #End of dp01 ingest function
