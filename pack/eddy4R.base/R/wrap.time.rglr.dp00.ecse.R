##############################################################################################
#' @title Wrapper function: Time regularization for ECSE dp00
#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description  
#'  Wrapper function. Time regularization for ECSE dp00

#' @param \code{dataList} A list of data frame containing the input dp00 data. [User-defined]
#' @param \code{site} Character: Site location. [-]
#' @param \code{dom} Character: Domain. [-]
#' @param \code{timeReg} A dataframe including the desired frequency of the regularized times. Of class "POSIXlt". [-] 
#' @param \code{idDp00} Character: dp00 data product number. [-]
#' @param \code{horVer} Character: Horizontal and vertical location of dp00. [-]

#' @return  A dataframe including the regularized dp00. [User-defined]

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords eddy-covariance, NEON, storage flux, L0, dp00, ECSE

#' @examples Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2017-10-20)
#     original creation
##############################################################################################
wrap.time.rglr.dp00.ecse <- function(
  dataList,
  site = "CPER",
  dom = "D10",
  timeReg,
  idDp00,
  horVer
){

#call Library
require(zoo)

#add domain and site into idDp00
numDp00 <- paste0("NEON.",dom,".",site,".",idDp00, sep="")

#perform time regularize for irga #######################################
if (idDp00 %in% c("DP0.00105")){
  subDp00 <- c("001.02316.700.000.000",#fwMoleCO2
               "001.02348.700.000.000",#fwMoleH2O
               "001.02349.700.000.000",#tempCell
               "001.02350.700.000.000",#presCell
               "001.02189.700.000.000",#asrpCO2
               "001.02184.700.000.000")#asrpH2O
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  #empty dataIn for time regularization
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  #calculate CO2 dry mole fraction 
  dataIn[[subDp00[1]]]$rtioMoleDryCo2 <- ((dataIn[[subDp00[1]]]$data)/10^6)/(1-(dataIn[[subDp00[2]]]$data/10^3))
  #calculate H2o dry mole fraction
  dataIn[[subDp00[2]]]$rtioMoleDryH2o <- ((dataIn[[subDp00[2]]]$data)/10^3)/(1-(dataIn[[subDp00[2]]]$data/10^3))
  
  #combine regularize data for irga
  dataTmp <- data.frame(dataIn[[subDp00[5]]]$data,
                        dataIn[[subDp00[6]]]$data,
                        dataIn[[subDp00[1]]]$data,
                        dataIn[[subDp00[1]]]$rtioMoleDryCo2,
                        dataIn[[subDp00[2]]]$data,
                        dataIn[[subDp00[2]]]$rtioMoleDryH2o,
                        dataIn[[subDp00[3]]]$data,
                        dataIn[[subDp00[4]]]$data)
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("asrpCo2","asrpH2o", "rtioMoleWetCo2", "rtioMoleDryCo2", "rtioMoleWetH2o", 
                         "rtioMoleDryH2o","temp", "pres")
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("NA", #asrpCO2
                                     "NA", #asrpH2O"C", 
                                     "umolCo2 mol-1", #fwMoleCO2
                                     "molCo2 mol-1", #rtioMoleDryCo2
                                     "mmolH2o mol-1", #fwMoleH2O 
                                     "molH2o mol-1", #rtioMoleDryH2o
                                     "C", #tempCell
                                     "kPa")#tempCell
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  
  #report output
  rpt <- dataTmp   
  #remove dataframe
  rm(dataTmp)
}#end of DP0.00105

#perform time regularize for profSnd ########################################################################
if (idxDp00 %in% c("DP0.00113")){
  convTime <- list()
  diffTime <-list()
  qfDiffTime <- list()
  dataIn <- list()
  #convert time
  lapply(names(dataList), function(x) convTime[[x]] <<- base::as.POSIXlt(dataList[[x]]$timeNew, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
  #calculate time difference
  lapply(names(dataList), function(x)  diffTime[[x]] <<- abs(difftime(convTime[[x]][1:(length(convTime[[x]])-1)], convTime[[x]][2:length(convTime[[x]])])))
  
  #add the first qfDiffTime row with NA
  #qfDiffTime[1] <- NA
  #generate the flag (-1) if time different is more than 6 s
  lapply(names(dataList), function(x) qfDiffTime[[x]] <<- ifelse (diffTime[[x]] > 6, -1, NA))
  
  lapply(names(dataList), function(x) qfDiffTime[[x]][length(dataList[[x]]$timeNew)] <<- NA)
  
  lapply(names(dataList), function(x) dataList[[x]]$qfDiffTime <<- qfDiffTime[[x]])
  
  #replace valve data that have qfDiffTime equal to -1 to -1
  lapply(names(dataList), function(x) dataList[[x]]$data <<- ifelse (is.na(dataList[[x]]$qfDiffTime), dataList[[x]]$data, -1))
  
  #if dataList is not exist, create an empty data frame irgaValvLvl
  if (horVer %in% c("701.000")){
  subDp00 <- c("001.02360.701.000.000", #valvCmd1 
               "001.02361.701.000.000", #valvCmd2
               "001.02362.701.000.000", #valvCmd3
               "001.02364.701.000.000", #valvCmd4
               "001.02365.701.000.000", #valvCmd5
               "001.02366.701.000.000", #valvCmd6 
               "001.02367.701.000.000", #valvCmd7
               "001.02368.701.000.000") #valvCmd8
               
  
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = 0, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
               
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc",
                                                               PosWndw = "PosWndwMax"
  )$dataRglr)  
  #generate 0 value for unused valve
  #dataIn$valv <- as.integer (0)
  
  #combine regularize time and data for irgaValvLvl
  dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,
                        dataIn[[subDp00[2]]]$data,
                        dataIn[[subDp00[3]]]$data,
                        dataIn[[subDp00[4]]]$data,
                        dataIn[[subDp00[5]]]$data,
                        dataIn[[subDp00[6]]]$data,
                        dataIn[[subDp00[7]]]$data,
                        dataIn[[subDp00[8]]]$data)
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("valv01", "valv02", "valv03", "valv04", "valv05", "valv06", "valv07", "valv08")
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  #filled NA with previous value
  dataTmp[,1:8] <- zoo::na.locf(dataTmp[,1:8], na.rm = FALSE)
  #report output
  rpt <- dataTmp      
  #remove dataframe
  rm(dataTmp)
  }#end of horVer == 701.000
  
  #if dataList is not exist, create an empty data frame valvVali
  if (horVer %in% c("703.000")){
    subDp00 <- c("001.02360.703.000.000", #valvCmd1 
                 "001.02361.703.000.000", #valvCmd2
                 "001.02362.703.000.000", #valvCmd3
                 "001.02364.703.000.000", #valvCmd4
                 "001.02365.703.000.000", #valvCmd5
                 "001.02366.703.000.000", #valvCmd6 
                 "001.02367.703.000.000", #valvCmd7
                 "001.02368.703.000.000") #valvCmd8
    #create full name for subDp00
    subDp00 <- paste0(numDp00,".",subDp00, sep="")
    #if dataList is not exist, create an empty data frame
    for (idxSubDp00 in subDp00){
      #idxSubDp00 <- subDp00[1]
      if (!(idxSubDp00 %in% names(dataList))) {
        dataList[[idxSubDp00]] <- data.frame(matrix(data = 0, ncol = 4, nrow = length(timeReg)))
        names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
        dataList[[idxSubDp00]]$timeNew <- timeReg 
      }
    }#end of for loop in subDp00
    
    lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                                 dataMeas = dataList[[x]],
                                                                 BgnRglr = as.POSIXlt(min(timeReg)),
                                                                 EndRglr = as.POSIXlt(max(timeReg)),
                                                                 FreqRglr = Freq,
                                                                 MethRglr = "CybiEc",
                                                                 PosWndw = "PosWndwMax"
    )$dataRglr)  
    #generate 0 value for unused valve
    #dataIn$valv <- as.integer (0)
    
    #combine regularize time and data for valvVali
    dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,
                          dataIn[[subDp00[2]]]$data,
                          dataIn[[subDp00[3]]]$data,
                          dataIn[[subDp00[4]]]$data,
                          dataIn[[subDp00[5]]]$data,
                          dataIn[[subDp00[6]]]$data,
                          dataIn[[subDp00[7]]]$data,
                          dataIn[[subDp00[8]]]$data)
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("valv01", "valv02", "valv03", "valv04", "valv05", "valv06", "valv07", "valv08")
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  #filled NA with previous value
  dataTmp[,1:8] <- zoo::na.locf(dataTmp[,1:8], na.rm = FALSE)
  #report output
  rpt <- dataTmp  
  #remove dataframe
  rm(dataTmp)
  }#end of horVer == 703.000
  
  #if dataList is not exist, create an empty data frame crdCo2ValvLvl
  if (horVer %in% c("702.000")){
    subDp00 <- c("001.02360.702.000.000", #valvCmd1
                 "001.02361.702.000.000", #valvCmd2
                 "001.02362.702.000.000", #valvCmd3
                 "001.02364.702.000.000", #valvCmd4
                 "001.02365.702.000.000", #valvCmd5
                 "001.02366.702.000.000", #valvCmd6 
                 "001.02367.702.000.000", #valvCmd7
                 "001.02368.702.000.000") #valvCmd8
    
    #create full name for subDp00
    subDp00 <- paste0(numDp00,".",subDp00, sep="")
    #if dataList is not exist, create an empty data frame
    for (idxSubDp00 in subDp00){
      #idxSubDp00 <- subDp00[1]
      if (!(idxSubDp00 %in% names(dataList))) {
        dataList[[idxSubDp00]] <- data.frame(matrix(data = 0, ncol = 4, nrow = length(timeReg)))
        names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
        dataList[[idxSubDp00]]$timeNew <- timeReg 
      }
    }#end of for loop in subDp00
    
    lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                                 dataMeas = dataList[[x]],
                                                                 BgnRglr = as.POSIXlt(min(timeReg)),
                                                                 EndRglr = as.POSIXlt(max(timeReg)),
                                                                 FreqRglr = Freq,
                                                                 MethRglr = "CybiEc",
                                                                 PosWndw = "PosWndwMax"
    )$dataRglr)  
    #generate 0 value for unused valve
    #dataIn$valv <- as.integer (0)
    
    #combine regularize time and data for crdCo2ValvLvl
    dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,
                          dataIn[[subDp00[2]]]$data,
                          dataIn[[subDp00[3]]]$data,
                          dataIn[[subDp00[4]]]$data,
                          dataIn[[subDp00[5]]]$data,
                          dataIn[[subDp00[6]]]$data,
                          dataIn[[subDp00[7]]]$data,
                          dataIn[[subDp00[8]]]$data)
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("valv01", "valv02", "valv03", "valv04", "valv05", "valv06", "valv07", "valv08")
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  #filled NA with previous value
  dataTmp[,1:8] <- zoo::na.locf(dataTmp[,1:8], na.rm = FALSE)
  #report output
  rpt <- dataTmp    
  #remove dataframe
  rm(dataTmp)
  }#end of horVer == 702.000
  
  #if dataList is not exist, create an empty data frame crdH2oValvLvl
  if (horVer %in% c("704.000")){
    subDp00 <- c("001.02360.704.000.000", #valvCmd1 
                 "001.02361.704.000.000", #valvCmd2
                 "001.02362.704.000.000", #valvCmd3
                 "001.02364.704.000.000", #valvCmd4
                 "001.02365.704.000.000", #valvCmd5
                 "001.02366.704.000.000", #valvCmd6 
                 "001.02367.704.000.000", #valvCmd7
                 "001.02368.704.000.000") #valvCmd8
    
    
    #create full name for subDp00
    subDp00 <- paste0(numDp00,".",subDp00, sep="")
    #if dataList is not exist, create an empty data frame
    for (idxSubDp00 in subDp00){
      #idxSubDp00 <- subDp00[1]
      if (!(idxSubDp00 %in% names(dataList))) {
        dataList[[idxSubDp00]] <- data.frame(matrix(data = 0, ncol = 4, nrow = length(timeReg)))
        names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
        dataList[[idxSubDp00]]$timeNew <- timeReg 
      }
    }#end of for loop in subDp00
    
    lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                                 dataMeas = dataList[[x]],
                                                                 BgnRglr = as.POSIXlt(min(timeReg)),
                                                                 EndRglr = as.POSIXlt(max(timeReg)),
                                                                 FreqRglr = Freq,
                                                                 MethRglr = "CybiEc",
                                                                 PosWndw = "PosWndwMax"
    )$dataRglr)  
    #generate 0 value for unused valve
    #dataIn$valv <- as.integer (0)
    
    #combine regularize time and data for crdH2oValvLvl
    dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,
                          dataIn[[subDp00[2]]]$data,
                          dataIn[[subDp00[3]]]$data,
                          dataIn[[subDp00[4]]]$data,
                          dataIn[[subDp00[5]]]$data,
                          dataIn[[subDp00[6]]]$data,
                          dataIn[[subDp00[7]]]$data,
                          dataIn[[subDp00[8]]]$data)
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("valv01", "valv02", "valv03", "valv04", "valv05", "valv06", "valv07", "valv08")
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  #filled NA with previous value
  dataTmp[,1:8] <- zoo::na.locf(dataTmp[,1:8], na.rm = FALSE)
  #report output
  rpt <- dataTmp  
  #remove dataframe
  rm(dataTmp)
  }#end of horVer == 704.000
}#end of DP0.00113

#perform time regularize for profSndAux ########################################################################
if (idxDp00 %in% c("DP0.00114")){
  convTime <- list()
  diffTime <-list()
  qfDiffTime <- list()
  dataIn <- list()
  #convert time
  lapply(names(dataList), function(x) convTime[[x]] <<- base::as.POSIXlt(dataList[[x]]$timeNew, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
  #calculate time difference
  lapply(names(dataList), function(x)  diffTime[[x]] <<- abs(difftime(convTime[[x]][1:(length(convTime[[x]])-1)], convTime[[x]][2:length(convTime[[x]])])))
  
  #add the first qfDiffTime row with NA
  #qfDiffTime[1] <- NA
  #generate the flag (-1) if time different is more than 6 s
  lapply(names(dataList), function(x) qfDiffTime[[x]] <<- ifelse (diffTime[[x]] > 6, -1, NA))
  
  lapply(names(dataList), function(x) qfDiffTime[[x]][length(dataList[[x]]$timeNew)] <<- NA)
  
  lapply(names(dataList), function(x) dataList[[x]]$qfDiffTime <<- qfDiffTime[[x]])
  
  #replace valve data that have qfDiffTime equal to -1 to -1
  lapply(names(dataList), function(x) dataList[[x]]$data <<- ifelse (is.na(dataList[[x]]$qfDiffTime), dataList[[x]]$data, -1))
  
  #if dataList is not exist, create an empty data frame for valvAux
  subDp00 <- c("001.02360.700.000.000", #valvCmd1 
               "001.02361.700.000.000", #valvCmd2
               "001.02362.700.000.000", #valvCmd3
               "001.02364.700.000.000") #valvCmd4
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = 0, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc",
                                                               PosWndw = "PosWndwMax"
  )$dataRglr) 
  
  #combine regularize data for irgaValvLvl
  dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,
                        dataIn[[subDp00[2]]]$data,
                        dataIn[[subDp00[3]]]$data,
                        dataIn[[subDp00[4]]]$data)
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("valv01", "valv02", "valv03", "valv04")
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("NA", "NA", "NA", "NA")
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  #filled NA with previous value
  dataTmp[,1:4] <- zoo::na.locf(dataTmp[,1:4], na.rm = FALSE)
  #report output
  rpt <- dataTmp    
  #remove dataframe
  rm(dataTmp)
}

#HutEnv####################################################################################################    
if (idxDp00 %in% c("DP0.00104")){
  subDp00 <- c("001.02344.700.000.000",#tempHut
               "001.02345.700.000.000",#RHHut
               "001.02346.700.000.000",#baroPresHut
               "001.02347.700.000.000") #H2OMixRatioHut
               
  #if dataList is not exist, create an empty data frame 
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  #calculate rtioMoleWetH2o from H2OMixRatioHut
  dataIn[[subDp00[4]]]$rtioMoleWetH2o <- (dataIn[[subDp00[4]]]$data)*(eddy4R.base::IntlNatu$RtioMolmDryH2o)*(dataIn[[subDp00[3]]]$data/eddy4R.base::IntlNatu$Pres00)
  
  #combine regularize data for envHut
  dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,
                        dataIn[[subDp00[2]]]$data,
                        dataIn[[subDp00[3]]]$data,
                        dataIn[[subDp00[4]]]$rtioMoleWetH2o)
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("temp", "rh", "pres", "rtioMoleWetH2o")
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("C", #tempHut
                                     "%", #RHHut
                                     "kPa", #baroPresHut
                                     "molH2o mol-1") #rtioMoleDryH2o
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  
  #report output
  rpt <- dataTmp    
  #remove dataframe
  rm(dataTmp)
  
}# clsed loop for dp  

#profPresValiRegDel########################################################################################
if (idxDp00 %in% c("DP0.00110")){
  subDp00 <- c("001.02196.709.000.000",#presGage at cylinder 1
               "001.02196.710.000.000",#presGage at cylinder 2
               "001.02196.711.000.000",#presGage at cylinder 3
               "001.02196.712.000.000",#presGage at cylinder 4
               "001.02196.713.000.000",#presGage at cylinder 5
               "001.02196.714.000.000")#presGage at cylinder 6
  #if dataList is not exist, create an empty data frame 
  
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
    if (horVer %in% "709.000") {
      dataTmp <- data.frame(dataIn[[subDp00[1]]]$data)}
    if (horVer %in% "710.000") {
      dataTmp <- data.frame(dataIn[[subDp00[2]]]$data)}
    if (horVer %in% "711.000") {
      dataTmp <- data.frame(dataIn[[subDp00[3]]]$data)}
    if (horVer %in% "712.000") {
      dataTmp <- data.frame(dataIn[[subDp00[4]]]$data)}
    if (horVer %in% "713.000") {
      dataTmp <- data.frame(dataIn[[subDp00[5]]]$data)}
    if (horVer %in% "714.000") {
      dataTmp <- data.frame(dataIn[[subDp00[6]]]$data)}
    
    
    #assign eddy4R name style to the output variables
    colnames(dataTmp) <- c("presDiff")
    
    #Creating the index to organize the variables in alphabetical order
    #idxOrd <- order(colnames(dataTmp))
    #Changing the order of the variables to alphabetical order using the index
    #dataTmp <- dataTmp[idxOrd]
    
    #provide in original order
    attr(dataTmp, which = "unit") <- c("kPa")
    
    #Sorting the order of units to match the colnames
    #attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
    
    #unit conversion using eddy4R internal unit
    dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                                 unitFrom = attributes(dataTmp)$unit,
                                                                 unitTo = "intl"))
    #combine regularize time and data
    dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
    
    #report output
    rpt <- dataTmp   
    #remove dataframe
    rm(dataTmp)
  
}# clsed loop for dp

#profPresValiRegTank########################################################################################
if (idxDp00 %in% c("DP0.00111")){
  subDp00 <- c("001.02196.709.000.000",#presGage at cylinder 1
               "001.02196.710.000.000",#presGage at cylinder 2
               "001.02196.711.000.000",#presGage at cylinder 3
               "001.02196.712.000.000",#presGage at cylinder 4
               "001.02196.713.000.000",#presGage at cylinder 5
               "001.02196.714.000.000")#presGage at cylinder 6
  
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  if (horVer %in% "709.000") {
    dataTmp <- data.frame(dataIn[[subDp00[1]]]$data)}
  if (horVer %in% "710.000") {
    dataTmp <- data.frame(dataIn[[subDp00[2]]]$data)}
  if (horVer %in% "711.000") {
    dataTmp <- data.frame(dataIn[[subDp00[3]]]$data)}
  if (horVer %in% "712.000") {
    dataTmp <- data.frame(dataIn[[subDp00[4]]]$data)}
  if (horVer %in% "713.000") {
    dataTmp <- data.frame(dataIn[[subDp00[5]]]$data)}
  if (horVer %in% "714.000") {
    dataTmp <- data.frame(dataIn[[subDp00[6]]]$data)}
    
    
    #assign eddy4R name style to the output variables
    colnames(dataTmp) <- c("presDiff")
    
    #Creating the index to organize the variables in alphabetical order
    #idxOrd <- order(colnames(dataTmp))
    #Changing the order of the variables to alphabetical order using the index
    #dataTmp <- dataTmp[idxOrd]
    
    #provide in original order
    attr(dataTmp, which = "unit") <- c("kPa")
    
    #Sorting the order of units to match the colnames
    #attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
    
    #unit conversion using eddy4R internal unit
    dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                                 unitFrom = attributes(dataTmp)$unit,
                                                                 unitTo = "intl"))
    #combine regularize time and data
    dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
    
    #report output
    rpt <- dataTmp   
    #remove dataframe
    rm(dataTmp)
  
}# clsed loop for dp

#profPresInlet########################################################################################
if (idxDp00 %in% c("DP0.00109")){
  subDp00 <- c("001.02196.000.010.000", #presGage at ML1
               "001.02196.000.020.000", #presGage at ML2
               "001.02196.000.030.000", #presGage at ML3
               "001.02196.000.040.000", #presGage at ML4
               "001.02196.000.050.000", #presGage at ML5
               "001.02196.000.060.000", #presGage at ML6
               "001.02196.000.070.000", #presGage at ML7
               "001.02196.000.080.000") #presGage at ML8
  
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  
  if (horVer %in% "010.000") {
    dataTmp <- data.frame(dataIn[[subDp00[1]]]$data)}
  if (horVer %in% "020.000") {
    dataTmp <- data.frame(dataIn[[subDp00[2]]]$data)}
  if (horVer %in% "030.000") {
    dataTmp <- data.frame(dataIn[[subDp00[3]]]$data)}
  if (horVer %in% "040.000") {
    dataTmp <- data.frame(dataIn[[subDp00[4]]]$data)}
  if (horVer %in% "050.000") {
    dataTmp <- data.frame(dataIn[[subDp00[5]]]$data)}
  if (horVer %in% "060.000") {
    dataTmp <- data.frame(dataIn[[subDp00[6]]]$data)}
  if (horVer %in% "070.000") {
    dataTmp <- data.frame(dataIn[[subDp00[7]]]$data)}
  if (horVer %in% "080.000") {
    dataTmp <- data.frame(dataIn[[subDp00[8]]]$data)}
    
    #assign eddy4R name style to the output variables
    colnames(dataTmp) <- c("presDiff")
    
    #Creating the index to organize the variables in alphabetical order
    #idxOrd <- order(colnames(dataTmp))
    #Changing the order of the variables to alphabetical order using the index
    #dataTmp <- dataTmp[idxOrd]
    
    #provide in original order
    attr(dataTmp, which = "unit") <- c("kPa")
    
    #Sorting the order of units to match the colnames
    #attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
    
    #unit conversion using eddy4R internal unit
    dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                                 unitFrom = attributes(dataTmp)$unit,
                                                                 unitTo = "intl"))
    #combine regularize time and data
    dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
    
    #report output
    rpt <- dataTmp    
    #remove dataframe
    rm(dataTmp)
  
}# clsed loop for dp

#profMfcSamp ###############################################################################  
if (idxDp00 %in% c("DP0.00106")){
  subDp00 <- c("001.01952.700.000.000",#frtSet0
               "001.01951.700.000.000",#frt0
               "001.01950.700.000.000",#frt
               "001.01949.700.000.000",#temp
               "001.01948.700.000.000")#presAtm
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  #combine regularize data 
  dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,
                        dataIn[[subDp00[2]]]$data,
                        dataIn[[subDp00[3]]]$data,
                        dataIn[[subDp00[4]]]$data,
                        dataIn[[subDp00[5]]]$data)
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("frtSet00", "frt00", "frt", "temp", "presAtm")
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("dm3 min-1", #frtSet0
                                     "dm3 min-1", #frt0
                                     "dm3 min-1", #frt
                                     "C", # temp
                                     "kPa") #presAtm 
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  
  #report output
  rpt <- dataTmp   
  #remove dataframe
  rm(dataTmp)
  
}# clsed loop for dp   

#profMfcVali############################################################################### 
if (idxDp00 %in% c("DP0.00107")){
  subDp00 <- c("001.01952.700.000.000",#frtSet0
               "001.01951.700.000.000",#frt0
               "001.01950.700.000.000",#frt
               "001.01949.700.000.000",#temp
               "001.01948.700.000.000")#presAtm
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  #combine regularize data
  dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,
                        dataIn[[subDp00[2]]]$data,
                        dataIn[[subDp00[3]]]$data,
                        dataIn[[subDp00[4]]]$data,
                        dataIn[[subDp00[5]]]$data)
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("frtSet00", "frt00", "frt", "temp", "presAtm")
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("dm3 min-1", #frtSet0
                                     "dm3 min-1", #frt0
                                     "dm3 min-1", #frt
                                     "C", # temp
                                     "kPa") #presAtm 
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  
  #report output
  rpt <- dataTmp 
  #remove dataframe
  rm(dataTmp)
  
}# clsed loop for dp   

#profMfm ########################################################################################
if (idxDp00 %in% c("DP0.00108")){
  subDp00 <- c("001.01951.700.010.000",#frt0 at ML1
               "001.01950.700.010.000",#frt at ML1
               "001.01949.700.010.000",#temp at ML1
               "001.01948.700.010.000",#presAtm at ML1
               "001.01951.700.020.000",#frt0 at ML2
               "001.01950.700.020.000",#frt at ML2
               "001.01949.700.020.000",#temp at ML2
               "001.01948.700.020.000",#presAtm at ML2
               "001.01951.700.030.000",#frt0 at ML3
               "001.01950.700.030.000",#frt at ML3
               "001.01949.700.030.000",#temp at ML3
               "001.01948.700.030.000",#presAtm at ML3
               "001.01951.700.040.000",#frt0 at ML4
               "001.01950.700.040.000",#frt at ML4
               "001.01949.700.040.000",#temp at ML4
               "001.01948.700.040.000",#presAtm at ML4
               "001.01951.700.050.000",#frt0 at ML5
               "001.01950.700.050.000",#frt at ML5
               "001.01949.700.050.000",#temp at ML5
               "001.01948.700.050.000",#presAtm at ML5
               "001.01951.700.060.000",#frt0 at ML6
               "001.01950.700.060.000",#frt at ML6
               "001.01949.700.060.000",#temp at ML6
               "001.01948.700.060.000",#presAtm at ML6
               "001.01951.700.070.000",#frt0 at ML7
               "001.01950.700.070.000",#frt at ML7
               "001.01949.700.070.000",#temp at ML7
               "001.01948.700.070.000",#presAtm at ML7
               "001.01951.700.080.000",#frt0 at ML8
               "001.01950.700.080.000",#frt at ML8
               "001.01949.700.080.000",#temp at ML8
               "001.01948.700.080.000")#presAtm at ML8
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  if (horVer %in% "700.010") {
    dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,
                          dataIn[[subDp00[2]]]$data,
                          dataIn[[subDp00[3]]]$data,
                          dataIn[[subDp00[4]]]$data)}
  if (horVer %in% "700.020") {
    dataTmp <- data.frame(dataIn[[subDp00[5]]]$data,
                          dataIn[[subDp00[6]]]$data,
                          dataIn[[subDp00[7]]]$data,
                          dataIn[[subDp00[8]]]$data)}
  if (horVer %in% "700.030") {
    dataTmp <- data.frame(dataIn[[subDp00[9]]]$data,
                          dataIn[[subDp00[10]]]$data,
                          dataIn[[subDp00[11]]]$data,
                          dataIn[[subDp00[12]]]$data)}
  if (horVer %in% "700.040") {
    dataTmp <- data.frame(dataIn[[subDp00[13]]]$data,
                          dataIn[[subDp00[14]]]$data,
                          dataIn[[subDp00[15]]]$data,
                          dataIn[[subDp00[16]]]$data)}
  if (horVer %in% "700.050") {
    dataTmp <- data.frame(dataIn[[subDp00[17]]]$data,
                          dataIn[[subDp00[18]]]$data,
                          dataIn[[subDp00[19]]]$data,
                          dataIn[[subDp00[20]]]$data)}
  if (horVer %in% "700.060") {
    dataTmp <- data.frame(dataIn[[subDp00[21]]]$data,
                          dataIn[[subDp00[22]]]$data,
                          dataIn[[subDp00[23]]]$data,
                          dataIn[[subDp00[24]]]$data)}
  if (horVer %in% "700.070") {
    dataTmp <- data.frame(dataIn[[subDp00[25]]]$data,
                          dataIn[[subDp00[26]]]$data,
                          dataIn[[subDp00[27]]]$data,
                          dataIn[[subDp00[28]]]$data)}
  if (horVer %in% "700.080") {
    dataTmp <- data.frame(dataIn[[subDp00[29]]]$data,
                          dataIn[[subDp00[30]]]$data,
                          dataIn[[subDp00[31]]]$data,
                          dataIn[[subDp00[32]]]$data)}
    
    #assign eddy4R name style to the output variables
    colnames(dataTmp) <- c("frt00", "frt", "temp", "presAtm")
    
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(colnames(dataTmp))
    #Changing the order of the variables to alphabetical order using the index
    dataTmp <- dataTmp[idxOrd]
    
    #provide in original order
    attr(dataTmp, which = "unit") <- c("dm3 min-1", #frt
                                       "dm3 min-1", #frt0
                                       "C", # temp
                                       "kPa") #presAtm
    
    #Sorting the order of units to match the colnames
    attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
    
    #unit conversion using eddy4R internal unit
    dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                                 unitFrom = attributes(dataTmp)$unit,
                                                                 unitTo = "intl"))
    #combine regularize time and data
    dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
    
    #report output
    rpt <- dataTmp     
    #remove dataframe
    rm(dataTmp)
  
}# clsed loop for dp

#profPumpSmp###############################################################################  
if (idxDp00 %in% c("DP0.00112")){
  subDp00 <- c("001.02351.700.000.000",#irga pumpVoltage
               "001.02351.700.010.000",#pumpVoltage at ML1
               "001.02351.700.020.000",#pumpVoltage at ML2
               "001.02351.700.030.000",#pumpVoltage at ML3
               "001.02351.700.040.000",#pumpVoltage at ML4
               "001.02351.700.050.000",#pumpVoltage at ML5
               "001.02351.700.060.000",#pumpVoltage at ML6
               "001.02351.700.070.000",#pumpVoltage at ML7
               "001.02351.700.080.000")#pumpVoltage at ML8
  
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  if (horVer %in% "700.000") {
    dataTmp <- data.frame(dataIn[[subDp00[1]]]$data)}
  if (horVer %in% "700.010") {
    dataTmp <- data.frame(dataIn[[subDp00[2]]]$data)}
  if (horVer %in% "700.020") {
    dataTmp <- data.frame(dataIn[[subDp00[3]]]$data)}
  if (horVer %in% "700.030") {
    dataTmp <- data.frame(dataIn[[subDp00[4]]]$data)}
  if (horVer %in% "700.040") {
    dataTmp <- data.frame(dataIn[[subDp00[5]]]$data)}
  if (horVer %in% "700.050") {
    dataTmp <- data.frame(dataIn[[subDp00[6]]]$data)}
  if (horVer %in% "700.060") {
    dataTmp <- data.frame(dataIn[[subDp00[7]]]$data)}
  if (horVer %in% "700.070") {
    dataTmp <- data.frame(dataIn[[subDp00[8]]]$data)}
  if (horVer %in% "700.080") {
    dataTmp <- data.frame(dataIn[[subDp00[9]]]$data)}
    
    #assign eddy4R name style to the output variables
    colnames(dataTmp) <- c("pumpVolt")
    
    #Creating the index to organize the variables in alphabetical order
    #idxOrd <- order(colnames(dataTmp))
    #Changing the order of the variables to alphabetical order using the index
    #dataTmp <- dataTmp[idxOrd]
    
    #provide in original order
    attr(dataTmp, which = "unit") <- c("V")
    
    #Sorting the order of units to match the colnames
    #attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
    
    #unit conversion using eddy4R internal unit
    dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                                 unitFrom = attributes(dataTmp)$unit,
                                                                 unitTo = "intl"))
    #combine regularize time and data
    dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
    #filled NA with previous value
    dataTmp[,1] <- zoo::na.locf(dataTmp[,1], na.rm = FALSE)
    #report output
    rpt <- dataTmp      
    #remove dataframe
    rm(dataTmp)
  
}# clsed loop for dp

#profSndVapor###################################################  
if (idxDp00 %in% c("DP0.00115")){
  subDp00 <- c("001.02352.700.000.000")#valvStat1
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  dataIn <- list()
  
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc",
                                                               PosWndw = "PosWndwMax"
  )$dataRglr)
  
  dataTmp <- data.frame(dataIn[[subDp00[1]]]$data)
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("valv")
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("NA")
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  #filled NA with previous value
  dataTmp[,1] <- zoo::na.locf(dataTmp[,1], na.rm = FALSE)
  #report output
  rpt <- dataTmp   
  #remove dataframe
  rm(dataTmp)
}# clsed loop for dp

#CO2Iso#################################################################################################
if (idxDp00 %in% c("DP0.00102")){
  subDp00 <- c("001.02306.700.000.000",#instStat
               "001.02307.700.000.000",#presCavi
               "001.02308.700.000.000",#tempCavi
               "001.02311.700.000.000",#tempWarmBox
               "001.02315.700.000.000",#specID
               "001.02316.700.000.000",#fwMoleCO2
               "001.02191.700.000.000",#fdMoleCO2
               "001.02317.700.000.000", #fwMole12CO2
               "001.02318.700.000.000",#fdMole12CO2
               "001.02319.700.000.000",#fwMole13CO2
               "001.02320.700.000.000", #fdMole13CO2
               "001.02324.700.000.000",#d13CO2
               "001.02325.700.000.000") #percentFwMoleH2O
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  #determine qfSensStus
  dataList[[subDp00[1]]]$qfSensStus <- ifelse(dataList[[subDp00[1]]]$data %in% 963, 0, 1)
  #convert time
  convTime <- base::as.POSIXlt(dataList[[subDp00[1]]]$timeNew, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
  #calculate time difference
  diffTime <- abs(difftime(convTime[1:(length(convTime)-1)], convTime[2:length(convTime)]))
  #add the first qfDiffTime row with NA
  qfDiffTime <- c()
  #qfDiffTime[1] <- NA
  #per ATBD generate the flag (-1) if time different is more than 3 s
  qfDiffTime <- ifelse (diffTime > 3, -1, NA)
  
  qfDiffTime[length(dataList[[subDp00[1]]]$timeNew)] <- NA
  
  dataList[[subDp00[1]]]$qfDiffTime <- qfDiffTime  
  
  #replace qfSensStus that have qfDiffTime equal to -1 to -1
  dataList[[subDp00[1]]]$qfSensStus <- ifelse(is.na(dataList[[subDp00[1]]]$qfDiffTime), dataList[[subDp00[1]]]$qfSensStus, -1)
  #empty dataIn for time regularization
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  #calculate H2o dry mole fraction in mol mol-1
  dataIn[[subDp00[13]]]$rtioMoleDryH2o <- ((dataIn[[subDp00[13]]]$data)/10^2)/(1-(dataIn[[subDp00[13]]]$data/10^2))
  
  #combine regularize data for crdCo2
  dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,#sensStus
                        dataIn[[subDp00[2]]]$data,#pres
                        dataIn[[subDp00[3]]]$data,#temp
                        dataIn[[subDp00[4]]]$data,#tempWbox
                        dataIn[[subDp00[5]]]$data,#idGas
                        dataIn[[subDp00[6]]]$data,#rtioMoleWetCo2
                        dataIn[[subDp00[7]]]$data,#rtioMoleDryCo2
                        dataIn[[subDp00[8]]]$data,#rtioMoleWet12CCo2
                        dataIn[[subDp00[9]]]$data,#rtioMoleDry12CCo2
                        dataIn[[subDp00[10]]]$data,#rtioMoleWet13CCo2
                        dataIn[[subDp00[11]]]$data,#rtioMoleDry13CCo2
                        dataIn[[subDp00[12]]]$data,#dlta13CCo2
                        dataIn[[subDp00[13]]]$data,#rtioMoleWetH2o
                        dataIn[[subDp00[13]]]$rtioMoleDryH2o,#rtioMoleDryH2o
                        dataIn[[subDp00[1]]]$qfSensStus)#qfSensStus

  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("sensStus", "pres", "temp", "tempWbox", "idGas", "rtioMoleWetCo2",
                         "rtioMoleDryCo2", "rtioMoleWet12CCo2", "rtioMoleDry12CCo2", "rtioMoleWet13CCo2",
                         "rtioMoleDry13CCo2", "dlta13CCo2", "rtioMoleWetH2o", "rtioMoleDryH2o", "qfSensStus")
  
  #filter data streams using specId
  dataTmp$rtioMoleWetCo2 <- ifelse(dataTmp$idGas %in% 105, dataTmp$rtioMoleWetCo2, NA)
  dataTmp$rtioMoleDryCo2 <- ifelse(dataTmp$idGas %in% 105, dataTmp$rtioMoleDryCo2 , NA)
  dataTmp$rtioMoleWet12CCo2 <- ifelse(dataTmp$idGas %in% 105, dataTmp$rtioMoleWet12CCo2, NA)
  dataTmp$rtioMoleDry12CCo2 <- ifelse(dataTmp$idGas %in% 105, dataTmp$rtioMoleDry12CCo2, NA)
  dataTmp$rtioMoleWet13CCo2 <- ifelse(dataTmp$idGas %in% 105, dataTmp$rtioMoleWet13CCo2, NA)
  dataTmp$rtioMoleDry13CCo2 <- ifelse(dataTmp$idGas %in% 105, dataTmp$rtioMoleDry13CCo2, NA)
  dataTmp$dlta13CCo2 <- ifelse(dataTmp$idGas %in% 105, dataTmp$dlta13CCo2, NA)
  dataTmp$rtioMoleWetH2o <- ifelse(dataTmp$idGas %in% 11, dataTmp$rtioMoleWetH2o, NA)
  dataTmp$rtioMoleDryH2o <- ifelse(dataTmp$idGas %in% 11, dataTmp$rtioMoleDryH2o, NA)
  
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("NA", #sensStus
                                     "Torr", #pres
                                     "C", #temp
                                     "C", #tempWbox
                                     "NA", #idGas
                                     "umolCo2 mol-1", #rtioMoleWetCo2
                                     "umolCo2 mol-1", #rtioMoleDryCo2
                                     "umolCo2 mol-1", #rtioMoleWet12CCo2
                                     "umolCo2 mol-1", #rtioMoleDry12CCo2
                                     "umolCo2 mol-1", #rtioMoleWet13CCo2
                                     "umolCo2 mol-1", #rtioMoleDry13CCo2
                                     "permill", #dlta13CCo2
                                     "%", #rtioMoleWetH2o
                                     "molH2o mol-1", #rtioMoleDryH2o
                                     "NA") #"qfSensStus"
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  
  #report output
  rpt <- dataTmp    
  #remove dataframe
  rm(dataTmp)
}

#H2OIso#################################################################################################
if (idxDp00 %in% c("DP0.00103")){
  subDp00 <- c("001.02306.700.000.000",#instStat
               "001.02307.700.000.000",#presCavi
               "001.02308.700.000.000",#tempCavi
               "001.02311.700.000.000",#tempWarmBox
               "001.02338.700.000.000",#valvMask
               "001.02339.700.000.000",#ppmvFwMoleH2O
               "001.02369.700.000.000",#d18OWater
               "001.02370.700.000.000",#d2HWater
               "001.02340.700.000.000") #N2Flag
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  convTime <- list()
  diffTime <-list()
  qfDiffTime <- list()
  #convert time
  lapply(names(dataList), function(x) convTime[[x]] <<- base::as.POSIXlt(dataList[[x]]$timeNew, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
  #calculate time difference
  lapply(names(dataList), function(x)  diffTime[[x]] <<- abs(difftime(convTime[[x]][1:(length(convTime[[x]])-1)], convTime[[x]][2:length(convTime[[x]])])))
  
  #add the first qfDiffTime row with NA
  #qfDiffTime[1] <- NA
  #generate the flag (-1) if time different is more than 3 s
  lapply(names(dataList), function(x) qfDiffTime[[x]] <<- ifelse (diffTime[[x]] > 3, -1, NA))
  
  lapply(names(dataList), function(x) qfDiffTime[[x]][length(dataList[[x]]$timeNew)] <<- NA)
  
  lapply(names(dataList), function(x) dataList[[x]]$qfDiffTime <<- qfDiffTime[[x]])
  
  #determine qfSensStus
  dataList[[subDp00[1]]]$qfSensStus <- ifelse(dataList[[subDp00[1]]]$data %in% 963, 0, 1)
  #determine qfStusN2
  dataList[[subDp00[9]]]$qfStusN2 <- ifelse(dataList[[subDp00[9]]]$data %in% 0, 0, 1)
  #determine qfLowRtioMoleWetH2o
  dataList[[subDp00[6]]]$qfLowRtioMoleWetH2o <- ifelse(dataList[[subDp00[6]]]$data > 5000, 0, 1)
  #calculate H2o dry mole fraction in mol mol-1
  dataList[[subDp00[6]]]$rtioMoleDryH2o <- ((dataList[[subDp00[6]]]$data)/10^6)/(1-(dataList[[subDp00[6]]]$data/10^6)/1000)
  
  #replace qfSensStus, qfStusN2 and qfLowRtioMoleWetH2o that have qfDiffTime equal to -1 to -1
  dataList[[subDp00[1]]]$qfSensStus <- ifelse(is.na(dataList[[subDp00[1]]]$qfDiffTime), dataList[[subDp00[1]]]$qfSensStus, -1)
  dataList[[subDp00[9]]]$qfStusN2 <- ifelse(is.na(dataList[[subDp00[9]]]$qfDiffTime), dataList[[subDp00[9]]]$qfStusN2, -1)
  dataList[[subDp00[6]]]$qfLowRtioMoleWetH2o <- ifelse(is.na(dataList[[subDp00[6]]]$qfDiffTime), dataList[[subDp00[6]]]$qfLowRtioMoleWetH2o, -1)
  
  #empty dataIn for time regularization
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  #combine regularize data for crdH20
  dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,#sensStus
                        dataIn[[subDp00[2]]]$data,#pres
                        dataIn[[subDp00[3]]]$data,#temp
                        dataIn[[subDp00[4]]]$data,#tempWbox
                        dataIn[[subDp00[5]]]$data,#valvCrdH2o
                        dataIn[[subDp00[6]]]$data,#rtioMoleWetH2o
                        dataIn[[subDp00[7]]]$data,#dlta18OH2o
                        dataIn[[subDp00[8]]]$data,#dlta2HH2o
                        dataIn[[subDp00[9]]]$data,#stusN2
                        dataIn[[subDp00[6]]]$rtioMoleDryH2o,#rtioMoleDryH2o
                        dataIn[[subDp00[1]]]$qfSensStus,#qfSensStus
                        dataIn[[subDp00[9]]]$qfStusN2,#qfStusN2
                        dataIn[[subDp00[6]]]$qfLowRtioMoleWetH2o)#qfLowRtioMoleWetH2o
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("sensStus", "pres", "temp", "tempWbox", "valvCrdH2o" , "rtioMoleWetH2o",
                         "dlta18OH2o", "dlta2HH2o", "stusN2", "rtioMoleDryH2o", "qfSensStus", "qfStusN2", "qfLowRtioMoleWetH2o")
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp[idxOrd]
  #provide in original order
  attr(dataTmp, which = "unit") <- c("NA", #sensStus
                                     "Torr", #pres
                                     "C", #temp
                                     "C", #tepWbox
                                     "NA", #valvCrdH2o
                                     "umolH2o mol-1", #rtioMoleWetH2o
                                     "permill", #dlta18OH2o
                                     "permill", #dlta2HH2o
                                     "NA", #stusN2
                                     "molH2o mol-1", #rtioMoleDryH2o
                                     "NA", #qfSensStus
                                     "NA", #qfStusN2
                                     "NA") #qfLowRtioMoleWetH2o
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                               unitFrom = attributes(dataTmp)$unit,
                                                               unitTo = "intl"))
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)
  
  #report output
  rpt <- dataTmp     
  #remove dataframe
  rm(dataTmp)
}

#HMP155 tower top and soil plot####################################################################################################    
if (idxDp00 %in% c("DP0.00098")){
  subDp00 <- c("001.01357.000.040.000",#rh tower top
               "001.01309.000.040.000",#temp tower top
               "001.01358.000.040.000",#tempDew tower top
               "001.01359.000.040.000",#qfSens tower top
               "001.01357.000.050.000",#rh tower top
               "001.01309.000.050.000",#temp tower top
               "001.01358.000.050.000",#tempDew tower top
               "001.01359.000.050.000",#qfSens tower top
               "001.01357.000.060.000",#rh tower top
               "001.01309.000.060.000",#temp tower top
               "001.01358.000.060.000",#tempDew tower top
               "001.01359.000.060.000",#qfSens tower top
               "001.01357.000.070.000",#rh tower top
               "001.01309.000.070.000",#temp tower top
               "001.01358.000.070.000",#tempDew tower top
               "001.01359.000.070.000",#qfSens tower top
               "001.01357.000.080.000",#rh tower top
               "001.01309.000.080.000",#temp tower top
               "001.01358.000.080.000",#tempDew tower top
               "001.01359.000.080.000",#qfSens tower top
               "001.01357.003.000.000",#rh soil plot
               "001.01309.003.000.000",#temp soil plot
               "001.01358.003.000.000",#tempDew soil plot
               "001.01359.003.000.000") #qfSens soil plot
  #create full name for subDp00
  subDp00 <- paste0(numDp00,".",subDp00, sep="")
  #if dataList is not exist, create an empty data frame
  for (idxSubDp00 in subDp00){
    #idxSubDp00 <- subDp00[1]
    if (!(idxSubDp00 %in% names(dataList))) {
      dataList[[idxSubDp00]] <- data.frame(matrix(data = NaN, ncol = 4, nrow = length(timeReg)))
      names(dataList[[idxSubDp00]]) <- c("time", "data", "exst", "timeNew")
      dataList[[idxSubDp00]]$timeNew <- timeReg 
    }
  }#end of for loop in subDp00
  
  dataIn <- list()
  lapply(names(dataList), function(x) dataIn[[x]] <<- eddy4R.base::def.rglr(timeMeas = base::as.POSIXlt(dataList[[x]][,"timeNew"], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
                                                               dataMeas = dataList[[x]],
                                                               BgnRglr = as.POSIXlt(min(timeReg)),
                                                               EndRglr = as.POSIXlt(max(timeReg)),
                                                               FreqRglr = Freq,
                                                               MethRglr = "CybiEc"
  )$dataRglr)
  
  if (horVer %in% "000.040") {
    dataTmp <- data.frame(dataIn[[subDp00[1]]]$data,#rh
                          dataIn[[subDp00[2]]]$data,#temp
                          dataIn[[subDp00[3]]]$data,#tempDew
                          dataIn[[subDp00[4]]]$data)#qfSens
  }
  if (horVer %in% "000.050") {
    dataTmp <- data.frame(dataIn[[subDp00[5]]]$data,#rh
                         dataIn[[subDp00[6]]]$data,#temp
                         dataIn[[subDp00[7]]]$data,#tempDew
                         dataIn[[subDp00[8]]]$data)#qfSens
  }
  if (horVer %in% "000.060") {
    dataTmp <- data.frame(dataIn[[subDp00[9]]]$data,#rh
                         dataIn[[subDp00[10]]]$data,#temp
                         dataIn[[subDp00[11]]]$data,#tempDew
                         dataIn[[subDp00[12]]]$data)#qfSens
  }
  if (horVer %in% "000.070") {
    dataTmp <- data.frame(dataIn[[subDp00[13]]]$data,#rh
                         dataIn[[subDp00[14]]]$data,#temp
                         dataIn[[subDp00[15]]]$data,#tempDew
                         dataIn[[subDp00[16]]]$data)#qfSens
  }
  if (horVer %in% "000.080") {
    dataTmp <- data.frame(dataIn[[subDp00[17]]]$data,#rh
                         dataIn[[subDp00[18]]]$data,#temp
                         dataIn[[subDp00[19]]]$data,#tempDew
                         dataIn[[subDp00[20]]]$data)#qfSens
  }
  if (horVer %in% "003.000") {
    dataTmp <- data.frame(dataIn[[subDp00[21]]]$data,#rh
                         dataIn[[subDp00[22]]]$data,#temp
                         dataIn[[subDp00[23]]]$data,#tempDew
                         dataIn[[subDp00[24]]]$data)#qfSens
  }
  
  #assign eddy4R name style to the output variables
  colnames(dataTmp) <- c("rh", "temp", "tempDew", "qfSens")
 
  #Creating the index to organize the variables in alphabetical order
  idxOrd <- order(colnames(dataTmp))
  
  #Changing the order of the variables to alphabetical order using the index
  dataTmp <- dataTmp01[idxOrd]
  
  #provide in original order
  attr(dataTmp, which = "unit") <- c("%", #rh
                                       "C", #temp
                                       "C", #tempDew
                                       "NA") #qfSens
  
  #Sorting the order of units to match the colnames
  attributes(dataTmp)$unit <- attributes(dataTmp)$unit[idxOrd]
  
  #unit conversion using eddy4R internal unit
  dataTmp <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = dataTmp,
                                                                 unitFrom = attributes(dataTmp)$unit,
                                                                 unitTo = "intl"))
  
  #combine regularize time and data
  dataTmp <- cbind(dataTmp, time = strftime(timeReg, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = F)

  #report output
  rpt <- dataTmp  
  #remove dataframe
  rm(dataTmp)
  
}# clsed loop for dp  

#return results
return(rpt)

} # function end
