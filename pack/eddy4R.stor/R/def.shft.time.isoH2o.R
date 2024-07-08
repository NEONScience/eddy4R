##############################################################################################
#' @title Definition function: Identify the time offset and adjust the crdH2o data accordingly.

#' @author 
#' Natchaya Pingintha-Durden  \email{ndurden@battelleecology.org}

#' @description 
#' Definition function. Identify the time offset and adjust the crdH2o data accordingly.

#' @param \code{dataList} Input data. 
#' @param \code{qfqmList} Input quality flags. 
#' @param \code{site} Site location in character format.
#' @param \code{lvls} Number of measurement level.

#' @return \code{rpt} is list returned that consists of the  corrected timestamps of data and qfqm and time offset in seconds. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords Picarro, time offset, crdH2o

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya Pingintha-Durden (2024-06-10)
#     original creation
####################################################################################################
def.shft.time.isoH2o <- function (
  dataList, 
  qfqmList, 
  valvVali,
  site, 
  lvls
){
  #dataList <- DATA$crdH2o
  #qfqmList <- qfqmFlag$crdH2o
  #valvVali <- DATA$crdH2oValvVali$`700_000`
  #site <- Para$Flow$Loc
  #lvls <- Para$Flow$Site$LvlMeasTow
  library(dplyr)
  library(xts)
  #define report output (dataList and qfqmList)
  rpt <- list()
  rpt$dataList <- dataList
  rpt$qfqmList <- qfqmList
  rpt$timeOfstMean <- NaN
  
  #determine number of measurement levels 
  lvlTow <- lvls
  
  lvlMeasTow <- as.list(1:lvlTow)
  
  lvlMeasTow <- lapply(lvlMeasTow, function(x) {
    paste0("000_0", x, "0")
  })
  
  #remove time from valvVali
  #valvVali <- subset(valvVali, select=-c(time))
  #get validation data, qfqm, fake qfHeat, and valvVali
  lowData <- cbind(subset(dataList$h2oLow, select=-c(time)), qfqmList$h2oLow, qfHeat = qfqmList$h2oLow$qfRngTemp, valvVali)
  medData <- cbind(subset(dataList$h2oMed, select=-c(time)), qfqmList$h2oMed, qfHeat = qfqmList$h2oMed$qfRngTemp, valvVali)
  highData <- cbind(subset(dataList$h2oHigh, select=-c(time)), qfqmList$h2oHigh, qfHeat = qfqmList$h2oHigh$qfRngTemp, valvVali)
  #remove when row all data and qfqm are NA in every columns
  lowData <- lowData %>% 
    filter(if_any(dlta18OH2o:qfStusN2, complete.cases))
  medData <- medData %>% 
    filter(if_any(dlta18OH2o:qfStusN2, complete.cases))
  highData <- highData %>% 
    filter(if_any(dlta18OH2o:qfStusN2, complete.cases))
  
  # need to stop if some df are missing:
  if (nrow(lowData) == 0 || nrow(medData) == 0 || nrow(highData) == 0) {
    return(rpt) # some reference data missing, following steps will fail,
    # so just return the input list
  }
  
  #remove colname in validation to match with sampling periods
  lowData <- subset(lowData, select=-c(dlta18OH2oRefeLow, dlta2HH2oRefeLow))
  medData <- subset(medData, select=-c(dlta18OH2oRefeMed, dlta2HH2oRefeMed))
  highData <- subset(highData, select=-c(dlta18OH2oRefeHigh, dlta2HH2oRefeHigh))
  
  # add level name
  lowData  <- dplyr::mutate(lowData, level = 3)       
  medData  <- dplyr::mutate(medData, level = 2)
  highData <- dplyr::mutate(highData, level = 1)
  
  #Determine time offset ############################
  #get data from all level
  wrkData <- lapply(lvlMeasTow, function (x){
    
    lvlData = cbind(subset(dataList[[x]], select=-c(time)), qfqmList[[x]], valvVali)
  })
  #remove when row all data and qfqm are NA in every columns
  #adding level in sampling periods
  for (idxLvl in 1:length(wrkData)){
    wrkData[[idxLvl]] <- wrkData[[idxLvl]] %>% 
      filter(if_any(dlta18OH2o:qfStusN2, complete.cases))
    wrkData[[idxLvl]]$level <- idxLvl + 3
  }
  
  #recompile list of ML dataframes
  wrkLvlData <- do.call(rbind, wrkData)
  
  # combine
  allData <- do.call(rbind, list(highData, medData, lowData, wrkLvlData))
  allData <- allData[order(allData$time), ]
  
  ###############################################################################
  #get first index when vaporizer 3-way valve turn on (1)
  idxValvHead <- head(which(allData$valv == 1), n=1)
  #get first index when ValvCrdH2o turn on (not equal to 0)
  idxValvCrdH2oHead <-  head(which(allData$valvCrdH2o != 0), n=1)
  #get last index when vaporizer 3-way valve turn on (1)
  idxValvTail <- tail(which(allData$valv == 1), n=1)
  #get last index when ValvCrdH2o turn on (not equal to 0)
  idxValvCrdH2oTail <-  tail(which(allData$valvCrdH2o != 0), n=1)
  
  
  #calculate time difference between valvCrdH2o and vaporizer 3-way valve 
  if ((idxValvHead == 1 | idxValvCrdH2oHead == 1) & allData$injNum[1] != 1){
    #assign NA to time difference between valvCrdH2o and vaporizer 3-way valve 
    #if the first injection occurred in previous day and the time difference cannot determine
    timeOfstHead  <- NA
    } else {
      timeOfstHead  <- hms::as_hms(difftime(as.POSIXct(allData$time[idxValvCrdH2oHead], format="%Y-%m-%dT%H:%M:%S", tz="GMT"),
                                            as.POSIXct(allData$time[idxValvHead], format="%Y-%m-%dT%H:%M:%S", tz="GMT")))
      }
  
  if ((idxValvTail == nrow(allData) | idxValvCrdH2oTail == nrow(allData)) & allData$injNum[nrow(allData)] != 18){
    #assign NA to time difference between valvCrdH2o and vaporizer 3-way valve 
    #if the last injection (injNum = 18) occurred in next day and the time difference cannot determine
    timeOfstTail  <- NA
    } else {
      timeOfstTail  <- hms::as_hms(difftime(as.POSIXct(allData$time[idxValvCrdH2oTail], format="%Y-%m-%dT%H:%M:%S", tz="GMT"), 
                                            as.POSIXct(allData$time[idxValvTail], format="%Y-%m-%dT%H:%M:%S", tz="GMT")))
      }
  
  #get mean ofset
  timeOfstMean <- as.numeric(mean(c(timeOfstHead, timeOfstTail), na.rm = TRUE))
  
  
  #output timeOfstMean
  rpt$timeOfstMean <- timeOfstMean
  
  if (!is.na(timeOfstMean) & base::Sys.getenv("OFFSET") == "") { # this file has offset, but no previous files.
    # save offset
    base::Sys.setenv("OFFSET" = as.character(timeOfstMean))
  } else if (is.na(timeOfstMean) & base::Sys.getenv("OFFSET") != "") { # offset exists in environment.
    timeOfstMean <- as.numeric(base::Sys.getenv("OFFSET"))
  } else {
    print("Missing offset in file and environment...getting ready to skip.")
  }
  
  #only proceed if timeOffset is greater than +/- 60 s
  if (is.na(timeOfstMean) | (timeOfstMean < 60 & timeOfstMean > -60)) {
    return(rpt) 
  }
  
 #Time shift correction###################################################### 
  # constrain to standard time vector (e.g., seconds in day of interest)
  timeSkeleton <- as.POSIXct(dataList$`000_010`$time,
                             format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  names(timeSkeleton) <- "time"
  
  timeSkeleton.xts <- xts::xts(timeSkeleton,order.by=timeSkeleton)
  allData.xts     <- xts::xts(allData[,!(names(allData) %in% c("time"))], order.by= allData$time) # prevents duplication of time var.
  
  allDataComb <- xts::merge.xts(timeSkeleton.xts, allData.xts, join = "left", tzone="UTC")
  
  # fill level classification prior to cutting back out to preserve structure.
  allDataComb$level <- zoo::na.locf(allDataComb$level, fromLast = TRUE)
  
  # extract data back to a data frame.
  allDataCorr <- data.frame(time = as.POSIXct(zoo::index(allDataComb)), zoo::coredata(allDataComb))
  
  # cut out skeleton variable
  allDataCorr <- dplyr::select(allDataCorr, -timeSkeleton.xts)
  
  # shift by appropriate number of rows.
  #timeOffset<- 350
  # hard coded in this example. since allDataCorr is a data frame w/ 86400 rows,
  # this should represent a number of seconds to shift the time series. sign is a bit
  # tricky! if < 0, the medium ref gas has encroached on the low ref gas (for example)
  # and need to push the time series *forward* in time (and vice versa)
  
  ofst <- floor(timeOfstMean) # needs to be an integer
  tmpLvl <- allDataCorr$level
  tmpTime <- allDataCorr$time
  
  for (idxCol in 1:length(allDataCorr)){
    if (ofst > 0) {
      allDataCorr[[idxCol]][1:(nrow(allDataCorr) - ofst + 1)] <- allDataCorr[[idxCol]][ofst:nrow(allDataCorr)]
      allDataCorr[[idxCol]][(nrow(allDataCorr) - ofst + 1):nrow(allDataCorr)] <- NaN # in future, need to pull in next day's data
      
    } else {
      offInd <- -1*ofst
      allDataCorr[[idxCol]][1:offInd] <- NaN
      allDataCorr[[idxCol]][(offInd + 1):nrow(allDataCorr)] <- allDataCorr[[idxCol]][1:(nrow(allDataCorr)-offInd)]
    }
  }#end for loop idxCol
  
  #replace time and level back
  allDataCorr$time <- tmpTime
  allDataCorr$level <- 	tmpLvl
  #get column names for dataList
  colData <- colnames(rpt$dataList$`000_010`)
  colData <- colData[!(colData %in% c("time"))]
  #get column names for qfqmList
  colQfqmSamp <- colnames(rpt$qfqmList$`000_010`)
  colQfqmVali <- colnames(rpt$qfqmList$h2oHigh)
  
  #loop through measurement levels and extract data, compile into data frame 
  #Data and qfqm
  for (idxList in c("dataList", "qfqmList")){
    #idxList <- c("dataList", "qfqmList")[2]
    # Convert back to level structure: validation periods
    if (idxList %in% c("dataList")){colName <- colData} else{colName <- colQfqmVali}
    for (idxColName in colName){
      
      rpt[[idxList]]$h2oLow[[idxColName]]    <- ifelse(allDataCorr$level == 3,
                                                       allDataCorr[[idxColName]], NaN)

      
      rpt[[idxList]]$h2oMed[[idxColName]]     <- ifelse(allDataCorr$level == 2,
                                                        allDataCorr[[idxColName]], NaN)
      
      rpt[[idxList]]$h2oHigh[[idxColName]]     <- ifelse(allDataCorr$level == 1,
                                                         allDataCorr[[idxColName]], NaN)
    } #end for loop
    
    # Convert back to level structure: sampling periods
    if (idxList %in% c("dataList")){colName00 <- colData} else{colName00 <- colQfqmSamp}
    for (i in 1:lvlTow) {
      lvlName <- base::paste0("000_0",i,"0")
      for (idxColName in colName00){
        rpt[[idxList]][[lvlName]][[idxColName]]      <- ifelse(allDataCorr$level == (i + 3),
                                                               allDataCorr[[idxColName]], NaN)
      }
    }
    
  }#end for loop idxList
  
  return(rpt)
  
}