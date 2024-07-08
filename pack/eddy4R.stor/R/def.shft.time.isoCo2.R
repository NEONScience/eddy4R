##############################################################################################
#' @title Definition function: Identify the time offset and adjust the crdCo2 data accordingly.

#' @author 
#' Rich Fiorella  \email{rich.fiorella@utah.edu}
#' Chris Florian  \email{cflorian@battelleecology.org}
#' Natchaya Pingintha-Durden  \email{ndurden@battelleecology.org}

#' @description 
#' Definition function. Identify the time offset and adjust the crdCo2 data accordingly.

#' @param \code{dataList} Input data. 
#' @param \code{qfqmList} Input quality flags. 
#' @param \code{site} Site location in character format.
#' @param \code{lvls} Number of measurement level.
#' @param \code{test} A logical stating if this is a test. Defaults to FALSE. 
#' @param \code{ofstTest} A user provides a time offset in seconds when test sets to TRUE.

#' @return \code{rpt} is list returned that consists of the  corrected timestamps of data and qfqm and time offset in seconds. 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords Picarro, time offset, crdCo2

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Rich Fiorella (2020-10-21)
#     original creation
#   Natchaya Pingintha-Durden (2023-12-06)
#     modified original function to be able to shift the whole data in dataList
#   Natchaya Pingintha-Durden (2024-01-25)
#     input qfqm
#   Natchaya Pingintha-Durden (2024-03-06)
#     include k-means clustering method 
#   Natchaya Pingintha-Durden (2024-03-07)
#     add header and apply eddy4R terms
####################################################################################################
def.shft.time.isoCo2 <- function (
  dataList, 
  qfqmList, 
  site, 
  lvls, 
  test = FALSE, 
  ofstTest = NA
  ){

  # changelog and author contributions / copyrights
  # shift_times_dp0p.R
  # rich fiorella 201021
  
  # In this initial version, script has been hardcoded to
  # work only on ABBY (or anywhere with 4 levels probably)
  # future work needs to generalize this script to apply to 
  # all sites. I've also hardcoded the offset in this example
  # from Chris Florian's flow.time.offset script.
  
  # recommended workflow to build toward:
  # 1) use chris's flow.time.offset to determine offset in L0p file
  # 2) use this script to correct offset in L0p file (and write out new L0 double prime file)
  # 3) run new L0 double prime file through standard eddy4R processing routines to generate new L4 file.
  # 
  # site <- "ABBY"
  # domain <- "D16"
  # start_date <- as.Date("2019-05-20")
  # end_date <- as.Date("2019-06-20")
  # date_seq <- as.character(as.character(seq.Date(start_date, end_date, by = "weeks")))
  # dateIdx <- as.list(date_seq)
  # date <- dateIdx[1]
  # FileInp <- "~/Desktop/eddy/storTowr/inpRefe/ECSE_dp0p_ABBY_2019-01-31.h5"
  # timeOffset <- -262
  
  # dataList <- DATA$crdCo2
  # site <- "ABBY"
  # lvls <- 5
  #It probably makes more sense to change this to work off an input file rather than specify site and domain
	# RPF - agree, but we need to redefine the date for the time series alignment below, as w/o redefining it refers
	  # to the base R date function.
	#h5data <- rhdf5::h5read(file = paste0("NEON.", domain,".", site, ".IP0.00200.001.ecse.", date, ".l0p.h5"), name = paste0("/", site))

	  #-----------------------------
	  # Chris - if the ultimate goal is to deploy this as a function in the flow.stor.towr.neon workflow,
	  # does it still make sense for a file name as a function argument? or better to pass the crdCo2 dataframes directly?
	  # the former makes sense if you see this as a posthoc correction where necessary - the latter makes
	  # sense if you think it should be applied to all files, regardless of whether there's an offset for consistency.
	  #----------------------------
  
  #define report output (dataList and qfqmList)
  rpt <- list()
  rpt$dataList <- dataList
  rpt$qfqmList <- qfqmList
  rpt$timeOfstMean <- NA
  
	date <- substr(dataList$co2Low$time[1], 1, 10) # get date from first element of time vector.

	#determine number of measurement levels 
	lvlTow <- lvls

	lvlMeasTow <- as.list(1:lvlTow)

	lvlMeasTow <- lapply(lvlMeasTow, function(x) {
	  paste0("000_0", x, "0")
	})
	#get validation data that will be used for offset determination
	lowTmp <- dataList$co2Low
	medTmp <- dataList$co2Med
	highTmp <- dataList$co2High
	#==========================================================
	# rf 230819 - not sure about this section. might not be necessary
	# remove NAs. <- this might not be necessary anymore with the tmp_filt steps?
	# lowData <- na.omit(lowData)
	# medData <- na.omit(medData)
	# highData <- na.omit(highData)
	lowTmp <- lowTmp[complete.cases(lowTmp$rtioMoleDryCo2), ]
	medTmp <- medTmp[complete.cases(medTmp$rtioMoleDryCo2), ]
	highTmp <- highTmp[complete.cases(highTmp$rtioMoleDryCo2), ]
	
	# need to stop if some df are missing:
	if (nrow(lowTmp) == 0 || nrow(medTmp) == 0 || nrow(highTmp) == 0) {
		return(rpt) # some reference data missing, following steps will fail,
						 # so just return the input list
	}
	#===========================================================

	# determine if there is a time offset, and exit if there is not.
	#calculate median (width = 10)
	tmpFiltLow  <- zoo::rollapply(lowTmp$rtioMoleDryCo2, width = 10, median, na.rm = TRUE, fill = NA)
	tmpFiltMed  <- zoo::rollapply(medTmp$rtioMoleDryCo2, width = 10, median, na.rm = TRUE, fill = NA)
	tmpFiltHigh <- zoo::rollapply(highTmp$rtioMoleDryCo2, width = 10, median, na.rm = TRUE, fill = NA)

	# find index of maximum absolute dCO2/dt that is at least 10 ppm
	
	ofstLow <- which(abs(diff(tmpFiltLow)) == max(abs(diff(tmpFiltLow)), na.rm=TRUE) &
	                   abs(diff(tmpFiltLow)) > 1e-5)
	ofstMed <- which(abs(diff(tmpFiltMed)) == max(abs(diff(tmpFiltMed)), na.rm=TRUE) &
	                   abs(diff(tmpFiltMed)) > 1e-5)
	ofstHigh <- which(abs(diff(tmpFiltHigh)) == max(abs(diff(tmpFiltHigh)), na.rm=TRUE) &
	                   abs(diff(tmpFiltHigh)) > 1e-5)
	
	#when ofstLow, ofstMed or ofstHigh is NA using  k-mean clustering method determine the index
	#using k-mean clustering method determine if there is a time offset, and exit if there is not.
	kmeanLow <- stats::kmeans(lowTmp$rtioMoleDryCo2, centers = 2)
	kmeanMed <- stats::kmeans(medTmp$rtioMoleDryCo2, centers = 2)
	kmeanHigh <- stats::kmeans(highTmp$rtioMoleDryCo2, centers = 2)
	
	#get index when cluster group changed
	ofstKmeanLow <- which(kmeanLow$cluster != kmeanLow$cluster[1])[1]
	ofstKmeanMed <- which(kmeanMed$cluster != kmeanMed$cluster[1])[1]
	ofstKmeanHigh <- which(kmeanHigh$cluster != kmeanHigh$cluster[1])[1]
	
	#replacing ofstLow, ofstMed or ofstHigh when it can not determine using the maximum absolute dCO2/dt method
	ofstLow <- ifelse(length(ofstLow) == 0, ofstKmeanLow, ofstLow)
	ofstMed <- ifelse(length(ofstMed) == 0, ofstKmeanMed, ofstMed)
	ofstHigh <- ifelse(length(ofstHigh) == 0, ofstKmeanHigh, ofstHigh)
	
	#select 1st value when there is more than one value in ofstLow, ofstMed, and ofstHigh
	if (length(ofstLow) > 1) {ofstLow <- ofstLow[1]}
	if (length(ofstMed) > 1) {ofstMed <- ofstMed[1]}
	if (length(ofstHigh) > 1) {ofstHigh <- ofstHigh[1]}
	
	
	# get step and time offsets.
	stepOffsetLow <- hms::as_hms(difftime(as.POSIXct(lowTmp$time[ofstLow], format="%Y-%m-%dT%H:%M:%S", tz="GMT"), 
	                                      as.POSIXct(lowTmp$time[1], format="%Y-%m-%dT%H:%M:%S", tz="GMT")))
	stepOffsetMed <- hms::as_hms(difftime(as.POSIXct(medTmp$time[ofstMed], format="%Y-%m-%dT%H:%M:%S", tz="GMT"), 
	                                      as.POSIXct(medTmp$time[1], format="%Y-%m-%dT%H:%M:%S", tz="GMT")))
	stepOffsetHigh <- hms::as_hms(difftime(as.POSIXct(highTmp$time[ofstHigh], format="%Y-%m-%dT%H:%M:%S", tz="GMT"), 
	                                       as.POSIXct(highTmp$time[1], format="%Y-%m-%dT%H:%M:%S", tz="GMT")))
	
	#determine if the Picarro timeStamp is ahead (lead) or behind (lag) compared to correct timeStamp
	#for lead scenario the difference between cluster center should be positive, positive, and negative
	#for lag scenario the difference between cluster center should be negative, positive, and positive
	diffCntrLow <- (kmeanLow$centers[kmeanLow$cluster[ofstKmeanLow]] - kmeanLow$centers[kmeanLow$cluster[1]])*10^6
	diffCntrMed <- (kmeanMed$centers[kmeanMed$cluster[ofstKmeanMed]] - kmeanMed$centers[kmeanMed$cluster[1]])*10^6
	diffCntrHigh <- (kmeanHigh$centers[kmeanHigh$cluster[ofstKmeanHigh]] - kmeanHigh$centers[kmeanHigh$cluster[1]])*10^6
	
	if (diffCntrLow != 0 & diffCntrMed  != 0 & diffCntrHigh != 0){
	  if (diffCntrLow < 0 &  diffCntrMed > 0 & diffCntrHigh > 0 ){
	    timeOffsetLow <- hms::as_hms(stepOffsetLow)
	    timeOffsetMed <- hms::as_hms(stepOffsetMed)
	    timeOffsetHigh <- hms::as_hms(stepOffsetHigh)
	  } else if (diffCntrLow > 0 &  diffCntrMed > 0 & diffCntrHigh < 0 ){
	    timeOffsetLow <- hms::as_hms(stepOffsetLow - hms::as_hms("00:10:00"))
	    timeOffsetMed <- hms::as_hms(stepOffsetMed - hms::as_hms("00:10:00"))
	    timeOffsetHigh <- hms::as_hms(stepOffsetHigh - hms::as_hms("00:10:00"))
	  }
	  else {
	    timeOffsetLow <- NA
	    timeOffsetMed <- NA
	    timeOffsetHigh <- NA
	    }
	} else {
	  timeOffsetLow <- NA
	  timeOffsetMed <- NA
	  timeOffsetHigh <- NA
	}

	# define a mean time offset (in seconds) to use in remainder of code.
	timeOfstMean <- as.numeric(mean(c(timeOffsetLow, timeOffsetMed, timeOffsetHigh), na.rm = TRUE))
	
	#reset timeOfstMean for testing purpose
	timeOfstMean <- ifelse(test == TRUE, ofstTest, timeOfstMean)
	
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

	#only proceed if timeOffset is between +/- 1 and 9 minutes (e.g., 60 < x < 540 or -540 > x > -60)
	if (is.na(timeOfstMean) | timeOfstMean < -540 | timeOfstMean > 540 | (timeOfstMean < 60 & timeOfstMean > -60)) {
	  return(rpt) 
	}

	#combine data and qfqm during validation; prepare for correcting
	#get validation data, qfqm, fake qfHeat, and move time to the last column
	lowData <- cbind(subset(dataList$co2Low, select=-c(time)), qfqmList$co2Low, qfHeat = qfqmList$co2Low$qfRngTemp, time = dataList$`000_010`$time)
	medData <- cbind(subset(dataList$co2Med, select=-c(time)), qfqmList$co2Med, qfHeat = qfqmList$co2Med$qfRngTemp, time = dataList$`000_010`$time)
	highData <- cbind(subset(dataList$co2High, select=-c(time)), qfqmList$co2High, qfHeat = qfqmList$co2High$qfRngTemp, time = dataList$`000_010`$time)
	#remove when row all data and qfqm are NA in every columns
	lowData <- lowData %>% 
	  filter(if_any(dlta13CCo2:qfStepTempWbox, complete.cases))
	medData <- medData %>% 
	  filter(if_any(dlta13CCo2:qfStepTempWbox, complete.cases))
	highData <- highData %>% 
	  filter(if_any(dlta13CCo2:qfStepTempWbox, complete.cases))
	
	#remove colname in validation to match with sampling periods
	lowData <- subset(lowData, select=-c(dlta13CCo2Refe, rtioMoleDry12CCo2Refe, rtioMoleDry13CCo2Refe, rtioMoleDryCh4Refe, rtioMoleDryCo2Refe))
	medData <- subset(medData, select=-c(dlta13CCo2Refe, rtioMoleDry12CCo2Refe, rtioMoleDry13CCo2Refe, rtioMoleDryCh4Refe, rtioMoleDryCo2Refe))
	highData <- subset(highData, select=-c(dlta13CCo2Refe, rtioMoleDry12CCo2Refe, rtioMoleDry13CCo2Refe, rtioMoleDryCh4Refe, rtioMoleDryCo2Refe))
	
	# add level name
	lowData  <- dplyr::mutate(lowData, level = 1)       
	medData  <- dplyr::mutate(medData, level = 2)
	highData <- dplyr::mutate(highData, level = 3)
	
	#get data from all level
	wrkData <- lapply(lvlMeasTow, function (x){
	  
	  lvlData = cbind(subset(dataList[[x]], select=-c(time)), qfqmList[[x]], time = dataList[[x]]$time)
	})
	#remove when row all data and qfqm are NA in every columns
	#adding level in sampling periods
	for (idxLvl in 1:length(wrkData)){
	  wrkData[[idxLvl]] <- wrkData[[idxLvl]] %>% 
	    filter(if_any(dlta13CCo2:qfStepTempWbox, complete.cases))
	  wrkData[[idxLvl]]$level <- idxLvl + 3
	}
	
	#recompile list of ML dataframes
	wrkLvlData <- do.call(rbind, wrkData)
	
	# combine
	allData <- do.call(rbind, list(highData, medData, lowData, wrkLvlData))
	allData <- allData[order(allData$time), ]
	
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
	colQfqmVali <- colnames(rpt$qfqmList$co2High)
	
	#loop through measurement levels and extract data, compile into data frame 
	#Data and qfqm
	for (idxList in c("dataList", "qfqmList")){
	  #idxList <- c("dataList", "qfqmList")[2]
	  # Convert back to level structure: validation periods
	  if (idxList %in% c("dataList")){colName <- colData} else{colName <- colQfqmVali}
	  for (idxColName in colName){
	    
	    rpt[[idxList]]$co2Low[[idxColName]]    <- ifelse(allDataCorr$level == 1,
	                                                     allDataCorr[[idxColName]], NaN)
	    
	    
	    rpt[[idxList]]$co2Med[[idxColName]]     <- ifelse(allDataCorr$level == 2,
	                                                      allDataCorr[[idxColName]], NaN)
	    
	    rpt[[idxList]]$co2High[[idxColName]]     <- ifelse(allDataCorr$level == 3,
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
