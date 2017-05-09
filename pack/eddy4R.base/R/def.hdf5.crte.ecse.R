##############################################################################################
#' @title Work flow to generate HDF5 file of ECSE dp0p for all sensors
#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Work flow to generate HDF5 file of ECSE L0p

#' @param \code

#' @return

#' @references


#' @keywords 

#' @examples

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   David Durden (2016-04-24)
#     original creation for ECTE
#   Natchaya P-Durden (2017-02-01)
#     original creation for ECSE
##############################################################################################

def.hdf5.crte.ecse <- function(
  date = "20161023", 
  Site = "CPER", 
  #LevlTowr, 
  DirOut, 
  DirIn,
  dateIn ="2016-10-23",
  LevlDp = "dp01"
) {
  
  
  library("rhdf5")
  library(eddy4R.base)
  library(XLConnect)
  
  
  date <- c("20161023", "20161024", "20161025", "20161026")
  #date <- c("20161023")
  dateIn <- c("2016-10-23", "2016-10-24", "2016-10-25", "2016-10-26")
  #dateIn <- c("2016-10-23")
  DirIn <- "~/eddy/data/CPER/out/dp0pH5/V02"
  DirOut <- "~/eddy/data/CPER/out/HDF5/V02/"
  #DirOut <- "~/eddy/data/CPER/out/HDF5/V02/iso/" #for iso community
  
  #Meta Data################################################################################################
  #Create a connection to the workbook
  wk <- loadWorkbook("~/eddy/data/CPER/wrk/metaData/NEON_HDF5_metadata_ECSE_CPER_20170112.xlsx") 
  #Read the workbook into a data frame
  metaList <- readWorksheet(wk, sheet="TIS", check.names = F) 
  #attribute list for site level
  attrSiteList <- metaList[which(metaList$`HDF5 Site Group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p"), c("fieldName","Values")]
  
  #attribute list for sensor/data product group level
  #irga
  attrIrgaList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "irga"), c("fieldName","Values")]
  #irgaValvLvl, valvAux, valvVali 
  attrValvList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "valv"), c("fieldName","Values")]
  #envHut
  attrEnvHutList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "envHut"), c("fieldName","Values")]
  #irgaPresValiRegIn
  attrRegInList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "irgaPresValiRegIn"), c("fieldName","Values")]
  #irgaPresValiRegOut
  attrRegOutList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "irgaPresValiRegOut"), c("fieldName","Values")]
  #presInlt
  attrPresInltList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "presInlt"), c("fieldName","Values")]
  #mfm
  attrMfmList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "mfm"), c("fieldName","Values")]
  #mfcVali
  attrMfcValiList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "mfcVali"), c("fieldName","Values")]
  #irgaMfcSamp
  attrMfcSampList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "irgaMfcSamp"), c("fieldName","Values")]
  #irgaMfcSamp
  attrPumpList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "pump"), c("fieldName","Values")]
  #crdCo2
  attrCrdCo2List <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "crdCo2"), c("fieldName","Values")]
  #crdH2o
  attrCrdH2oList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "crdH2o"), c("fieldName","Values")]
  #crdH2oValvVali
  attrCrdH2oValvValiList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "crdH2oValvVali"), c("fieldName","Values")]
  #gasRefe
  attrGasRefeList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "gasRefe"), c("fieldName","Values")]
  #h2o90Refe
  attrH2o90RefeList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "h2o90Refe"), c("fieldName","Values")]
  #saats
  attrTempAirLvlList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "saats"), c("fieldName","Values")]
  #traats
  attrTempAirTop <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "traats"), c("fieldName","Values")]
  
  
  #attribute list for data table level
  #gasRefe co2Arch
  attrCo2ArchList <- metaList[which(metaList$`Data table specific` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "co2Arch"), c("fieldName","Values")]
  #gasRefe co2Low
  attrCo2LowList <- metaList[which(metaList$`Data table specific` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "co2Low"), c("fieldName","Values")]
  #gasRefe co2Med
  attrCo2MedList <- metaList[which(metaList$`Data table specific` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "co2Med"), c("fieldName","Values")]
  #gasRefe co2High
  attrCo2HighList <- metaList[which(metaList$`Data table specific` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "co2High"), c("fieldName","Values")]
  #h2o90Refe
  attrH2o90RefeValiList <- metaList[which(metaList$`Data table specific` == "X" & metaList$`ECSE Eddy4R-processing-level` == "L0p" & metaList$`Sensor` == "h2o90Refe"), c("fieldName","Values")]
  ##########################################################################################################
  for (i in 1:length(date)){
    #i <- 1
    DirIn00 <- paste0(DirIn ,"/",date[i])
    if (dir.exists(DirOut) == FALSE) dir.create(DirOut, recursive = TRUE)
    #File list for CPER
    fileList <- list.files(path = DirIn00, pattern = ".csv")
    #Creating full path filenames
    fileList2 <- paste(DirIn00,fileList, sep = "/")
    
    #Create a vector of variable names from the filenames
    #Need to take out the hard code and make it more flexible using regex
    varName <- gsub(".csv","",fileList)
    
    #Read in the data for the day into a list of dataframes
    dataList <- lapply(fileList2, read.table, header = T, stringsAsFactors = F, sep = ",", row.names = NULL)
    #Using the data names provided with the data to name the lists
    names(dataList) <- varName     
    #create a group called location1 within the H5 file
    #h5createGroup("HDF5TIS_L0_SERCtest.h5", "SERC")
    
    #Create the file, create a class
    idFile <- H5Fcreate(paste0(DirOut,"ECSE_dp01_", site, "_", dateIn[i], ".h5"))
    #If the file is already created use:
    #idFile <- H5Fopen("HDF5TIS_L0_prototype.h5")
    
    #Create a group level for SERC
    idSite <- H5Gcreate(idFile, site) 
    #If the group is already created use:
    #idSite <- H5Gopen(idFile,site)
    idDp0p <- H5Gcreate(idSite,"dp0p")
    idDp01 <- H5Gcreate(idSite,"dp01")
    
    idDataLvlDp0p <- H5Gcreate(idDp0p,"data")
    idDataLvlDp01 <- H5Gcreate(idDp01,"data")
    
    idQfqmLvlDp0p <- H5Gcreate(idDp0p,"qfqm")
    idQfqmLvlDp01 <- H5Gcreate(idDp01,"qfqm")
    
    idUcrt <-H5Gcreate(idDp01,"ucrt")
    
    #Create a function to create group levels for each L0 DP
    #grpCrte <- function(x) H5Gcreate(idSite, x)
    #grpCrte <- function(x) H5Gcreate(dplid, x)
    
    #Apply the function to produce the group levels in the data file
    #lapply(grpList, grpCrte)
    
    #Creating level 0p file structures
    grpList <- c("irga_001","irgaValvLvl_001","valvAux_001","valvVali_001",
                 "gasRefe_001", "envHut_001", "irgaPresValiRegIn_001",
                 "irgaPresValiRegOut_001", "presInlt_001", "mfm_001",
                 "mfcVali_001", "irgaMfcSamp_001", "pump_001",
                 "crdCo2_001", "crdCo2ValvLvl_001","crdH2o_001",
                 "crdH2oValvVali_001", "crdH2oValvLvl_001", "h2o90Refe_001",
                 "tempAirLvl_001", "tempAirTop_001")
    
    #pick only sensors for sci community
    # grpList <- c("valvAux_001","valvVali_001",
    #              "gasRefe_001", "crdCo2_001",
    #              "crdCo2ValvLvl_001","crdH2o_001",
    #              "crdH2oValvVali_001", "crdH2oValvLvl_001",
    #              "h2o90Refe_001")
    
    # #qfqm group level
    grpListQf <- c("irga_001","valvAux_001", "envHut_001",
                   "irgaPresValiRegIn_001", "irgaPresValiRegOut_001", "presInlt_001",
                   "mfm_001", "mfcVali_001", "irgaMfcSamp_001",
                   "pump_001", "crdCo2_001", "crdH2o_001", "tempAirLvl_001", "tempAirTop_001")
    
    #pick only sensors for sci community
    # grpListQf <- c("valvAux_001", "crdCo2_001", "crdH2o_001")
    
    lapply(grpList, function(x) H5Gcreate(idDataLvlDp0p, x))
    lapply(grpListQf, function(x) H5Gcreate(idQfqmLvlDp0p, x))
    
    #Creating level 1 file structures
    
    grpListDp01 <- c("irgaCo2", "irgaH2o", "isoCo2", "isoH2o", "envHut", "tempAirLvl", "tempAirTop")
    grpListQfDp01 <- c("irgaCo2", "irgaH2o", "isoCo2", "isoH2o", "envHut", "tempAirLvl", "tempAirTop")
    lapply(grpListDp01, function(x) H5Gcreate(idDataLvlDp01, x))
    lapply(grpListDp01, function(x) H5Gcreate(idQfqmLvlDp01, x))
    
    
    #write attribute to the site level
    lapply(seq_len(nrow(attrSiteList)), function(x) h5writeAttribute(attrSiteList[x,"Values"], h5obj = idSite, name = attrSiteList[x,"fieldName"]))
    
    #Used to write the datasets into the groups with attributes attached.
    #h5writeDataset.character(obj = dataList$soni[,"time"], file = idSite, name = "DP0_soni/000_000_ts", write.attributes = TRUE)
    
    #Writing dp0p for irga############################################################
    #replace NA with NaN
    #dataList$irga_co2Arch[is.na(dataList$irga_co2Arch)] <- NaN
    #lapply(names(dataList), function(x) dataList[[x]][,1:length(dataList[[x]])] <<- dataList[[x]][is.na(dataList[[x]][,1:length(dataList[[x]])])] <- NaN)
    lapply(names(dataList), function(x)  dataList[[x]][is.na(dataList[[x]][,1:length(dataList[[x]])])] <<- NaN)
    idData <- H5Oopen(idDataLvlDp0p,"irga_001")
    #write attribute to the data table level for each measurement level
    tempLoc <- c("000_010", "000_020", "000_030", "000_040")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$irga_000_010}
      if (i == 2) { dataIn <- dataList$irga_000_020}
      if (i == 3) { dataIn <- dataList$irga_000_030}
      if (i == 4) { dataIn <- dataList$irga_000_040}
      #write data table for each measurement level
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("-", "-", "Pa", "molCo2 mol-1", "molH2o mol-1",
                                        "molCo2 mol-1", "molH2o mol-1", "K", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrIrgaList)), function(x) h5writeAttribute(attrIrgaList[x,"Values"], h5obj = idData, name = attrIrgaList[x,"fieldName"]))
    
    #write attribute to the data table level for each gas concentration
    tempLoc <- c("co2Arch", "co2Low", "co2Med", "co2High", "co2Zero")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$irga_co2Arch}
      if (i == 2) { dataIn <- dataList$irga_co2Low}
      if (i == 3) { dataIn <- dataList$irga_co2Med}
      if (i == 4) { dataIn <- dataList$irga_co2High}
      if (i == 5) { dataIn <- dataList$irga_co2Zero}
      #write data table for each measurement level
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("-", "-", "permill", "Pa", "molCo2 mol-1", "molCo2 mol-1", "molCo2 mol-1",
                                        "molCo2 mol-1", "molH2o mol-1", "molCo2 mol-1", "molH2o mol-1", "K", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    
    #qf table for each measurement level
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"irga_001")
    #write attribute to the data table level for each mesurment level and gas concentration
    tempLoc <- c("000_010", "000_020", "000_030", "000_040", "co2Arch", "co2Low", "co2Med", "co2High", "co2Zero")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$qfIrga_000_010}
      if (i == 2) { dataIn <- dataList$qfIrga_000_020}
      if (i == 3) { dataIn <- dataList$qfIrga_000_030}
      if (i == 4) { dataIn <- dataList$qfIrga_000_040}
      if (i == 5) { dataIn <- dataList$qfIrga_co2Arch}
      if (i == 6) { dataIn <- dataList$qfIrga_co2Low}
      if (i == 7) { dataIn <- dataList$qfIrga_co2Med}
      if (i == 8) { dataIn <- dataList$qfIrga_co2High}
      if (i == 9) { dataIn <- dataList$qfIrga_co2Zero}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idQfqm, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA", "NA", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idQfqmHorVer <- H5Oopen(idQfqm,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrIrgaList)), function(x) h5writeAttribute(attrIrgaList[x,"Values"], h5obj = idQfqm, name = attrIrgaList[x,"fieldName"]))
    
    #Writing dp0p for irgaValvLvl############################################################
    idData <- H5Oopen(idDataLvlDp0p,"irgaValvLvl_001")
    #replace NA with NaN
    #dataList$irgaValvLvl_701_000$lvlIrga[dataList$irgaValvLvl_701_000$lvlIrga %in% NA] <- NaN
    h5writeDataset.data.frame(obj = dataList$irgaValvLvl_701_000, h5loc = idData, name = "701_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrValvList)), function(x) h5writeAttribute(attrValvList[x,"Values"], h5obj = idData, name = attrValvList[x,"fieldName"]))
    #write attribute to the data table level
    dataIn <- dataList$irgaValvLvl_701_000
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"701_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    
    
    #Writing dp0p for valvAux############################################################
    #data table
    idData <- H5Oopen(idDataLvlDp0p,"valvAux_001")
    h5writeDataset.data.frame(obj = dataList$valvAux_700_000, h5loc = idData, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrValvList)), function(x) h5writeAttribute(attrValvList[x,"Values"], h5obj = idData, name = attrValvList[x,"fieldName"]))
    
    #write attribute to the data table level 
    dataIn <- dataList$valvAux_700_000
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    #qf table
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"valvAux_001")
    h5writeDataset.data.frame(obj = dataList$qfValvAux_700_000, h5loc = idQfqm, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrValvList)), function(x) h5writeAttribute(attrValvList[x,"Values"], h5obj = idQfqm , name = attrValvList[x,"fieldName"]))
    
    #write attribute to the data table level 
    dataIn <- dataList$qfValvAux_700_000
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idQfqmHorVer <- H5Oopen(idQfqm,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    
    
    #Writing dp0p for valvVali############################################################
    idData <- H5Oopen(idDataLvlDp0p,"valvVali_001")
    h5writeDataset.data.frame(obj = dataList$valvVali_703_000, h5loc = idData, name = "703_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrValvList)), function(x) h5writeAttribute(attrValvList[x,"Values"], h5obj = idData, name = attrValvList[x,"fieldName"]))
    
    #write attribute to the data table level 
    dataIn <- dataList$valvVali_703_000
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"703_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    
    #Writing dp0p for gasRefe############################################################
    idData <- H5Oopen(idDataLvlDp0p,"gasRefe_001")
    #write attribute to the data table level for each mesurment level and gas concentration
    tempLoc <- c("710_000", "712_000", "713_000", "714_000")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { 
        dataIn <- dataList$gasRefe_710_000
        listIn <- attrCo2ArchList}
      if (i == 2) { 
        dataIn <- dataList$gasRefe_712_000
        listIn <- attrCo2LowList}
      if (i == 3) { 
        dataIn <- dataList$gasRefe_713_000
        listIn <- attrCo2MedList}
      if (i == 4) { 
        dataIn <- dataList$gasRefe_714_000
        listIn <- attrCo2HighList}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("permill", "molCo2 mol-1", "molCo2 mol-1", "molCo2 mol-1", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
      #write attribute to the gas level
      lapply(seq_len(nrow(listIn)), function(x) h5writeAttribute(listIn[x,"Values"], h5obj = idDataHorVer, name = listIn[x,"fieldName"]))
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrGasRefeList)), function(x) h5writeAttribute(attrGasRefeList[x,"Values"], h5obj = idData, name = attrGasRefeList[x,"fieldName"]))
    
    
    #Writing Data for envHut ##################################################################################
    #data table
    idData <- H5Oopen(idDataLvlDp0p,"envHut_001")
    h5writeDataset.data.frame(obj = dataList$envHut, h5loc = idData, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrEnvHutList)), function(x) h5writeAttribute(attrEnvHutList[x,"Values"], h5obj = idData, name = attrEnvHutList[x,"fieldName"]))
    
    #write attribute to the data table level
    dataIn <- dataList$envHut
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("Pa", "-", "molH2o mol-1", "K", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    #qf table
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"envHut_001")
    h5writeDataset.data.frame(obj = dataList$qfEnvHut, h5loc = idQfqm, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrEnvHutList)), function(x) h5writeAttribute(attrEnvHutList[x,"Values"], h5obj = idQfqm, name = attrEnvHutList[x,"fieldName"]))
    
    #write attribute to the data table level
    dataIn <- dataList$qfEnvHut
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                      "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idQfqmHorVer <- H5Oopen(idQfqm,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    
    #Writing Data for irgaPresValiRegIn############################################################
    idData <- H5Oopen(idDataLvlDp0p,"irgaPresValiRegIn_001")
    #write attribute to the data table level for each location
    tempLoc <- c("709_000", "710_000", "711_000", "712_000", "713_000", "714_000")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$irgaPresValiRegIn_709_000}
      if (i == 2) { dataIn <- dataList$irgaPresValiRegIn_710_000}
      if (i == 3) { dataIn <- dataList$irgaPresValiRegIn_711_000}
      if (i == 4) { dataIn <- dataList$irgaPresValiRegIn_712_000}
      if (i == 5) { dataIn <- dataList$irgaPresValiRegIn_713_000}
      if (i == 6) { dataIn <- dataList$irgaPresValiRegIn_714_000}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("Pa", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrRegInList)), function(x) h5writeAttribute(attrRegInList[x,"Values"], h5obj = idData, name = attrRegInList[x,"fieldName"]))
    
    #qf table for each measurement level
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"irgaPresValiRegIn_001")
    #write attribute to the data table level for each location
    tempLoc <- c("709_000", "710_000", "711_000", "712_000", "713_000", "714_000")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$qfIrgaPresValiRegIn_709_000}
      if (i == 2) { dataIn <- dataList$qfIrgaPresValiRegIn_710_000}
      if (i == 3) { dataIn <- dataList$qfIrgaPresValiRegIn_711_000}
      if (i == 4) { dataIn <- dataList$qfIrgaPresValiRegIn_712_000}
      if (i == 5) { dataIn <- dataList$qfIrgaPresValiRegIn_713_000}
      if (i == 6) { dataIn <- dataList$qfIrgaPresValiRegIn_714_000}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idQfqm, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idQfqmHorVer <- H5Oopen(idQfqm,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrRegInList)), function(x) h5writeAttribute(attrRegInList[x,"Values"], h5obj = idQfqm, name = attrRegInList[x,"fieldName"]))
    
    #Writing Data for irgaPresValiRegOut############################################################
    idData <- H5Oopen(idDataLvlDp0p,"irgaPresValiRegOut_001")
    #write attribute to the data table level for each location
    tempLoc <- c("709_000", "710_000", "711_000", "712_000", "713_000", "714_000")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$irgaPresValiRegOut_709_000}
      if (i == 2) { dataIn <- dataList$irgaPresValiRegOut_710_000}
      if (i == 3) { dataIn <- dataList$irgaPresValiRegOut_711_000}
      if (i == 4) { dataIn <- dataList$irgaPresValiRegOut_712_000}
      if (i == 5) { dataIn <- dataList$irgaPresValiRegOut_713_000}
      if (i == 6) { dataIn <- dataList$irgaPresValiRegOut_714_000}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("Pa", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrRegOutList)), function(x) h5writeAttribute(attrRegOutList[x,"Values"], h5obj = idData, name = attrRegOutList[x,"fieldName"]))
    
    #qf table for each measurement level
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"irgaPresValiRegOut_001")
    #write attribute to the data table level for each location
    tempLoc <- c("709_000", "710_000", "711_000", "712_000", "713_000", "714_000")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$qfIrgaPresValiRegOut_709_000}
      if (i == 2) { dataIn <- dataList$qfIrgaPresValiRegOut_710_000}
      if (i == 3) { dataIn <- dataList$qfIrgaPresValiRegOut_711_000}
      if (i == 4) { dataIn <- dataList$qfIrgaPresValiRegOut_712_000}
      if (i == 5) { dataIn <- dataList$qfIrgaPresValiRegOut_713_000}
      if (i == 6) { dataIn <- dataList$qfIrgaPresValiRegOut_714_000}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idQfqm, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      if (i == 3) {unitList <- c("NA", "NA", "NA", "NA", "NA", "NA")} else {unitList <- c("NA", "NA", "NA", "NA", "NA")}
      attr(dataIn, which = "unit") <- unitList
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idQfqmHorVer <- H5Oopen(idQfqm,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrRegOutList)), function(x) h5writeAttribute(attrRegOutList[x,"Values"], h5obj = idQfqm, name = attrRegOutList[x,"fieldName"]))
    
    #Writing Data for presInlt############################################################
    idData <- H5Oopen(idDataLvlDp0p,"presInlt_001")
    #write attribute to the data table level for each location
    tempLoc <- c("000_010", "000_020", "000_030", "000_040")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$presInlt_000_010}
      if (i == 2) { dataIn <- dataList$presInlt_000_020}
      if (i == 3) { dataIn <- dataList$presInlt_000_030}
      if (i == 4) { dataIn <- dataList$presInlt_000_040}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("Pa", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrPresInltList)), function(x) h5writeAttribute(attrPresInltList[x,"Values"], h5obj = idData, name = attrPresInltList[x,"fieldName"]))
    
    #qf table for each measurement level
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"presInlt_001")
    #write attribute to the data table level for each location
    tempLoc <- c("000_010", "000_020", "000_030", "000_040")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$qfPresInlt_000_010}
      if (i == 2) { dataIn <- dataList$qfPresInlt_000_020}
      if (i == 3) { dataIn <- dataList$qfPresInlt_000_030}
      if (i == 4) { dataIn <- dataList$qfPresInlt_000_040}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idQfqm, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <-  c("NA", "NA", "NA", "NA", "NA", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idQfqmHorVer <- H5Oopen(idQfqm,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrPresInltList)), function(x) h5writeAttribute(attrPresInltList[x,"Values"], h5obj = idQfqm, name = attrPresInltList[x,"fieldName"]))
    
    #Writing Data for mfm ############################################################
    idData <- H5Oopen(idDataLvlDp0p,"mfm_001")
    #write attribute to the data table level for each location
    tempLoc <- c("700_010", "700_020", "700_030", "700_040")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$mfm_700_010}
      if (i == 2) { dataIn <- dataList$mfm_700_020}
      if (i == 3) { dataIn <- dataList$mfm_700_030}
      if (i == 4) { dataIn <- dataList$mfm_700_040}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("m3 s-1", "m3 s-1", "Pa", "K", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrMfmList)), function(x) h5writeAttribute(attrMfmList[x,"Values"], h5obj = idData, name = attrMfmList[x,"fieldName"]))
    
    #qf table for each measurement level
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"mfm_001")
    #write attribute to the data table level for each location
    tempLoc <- c("700_010", "700_020", "700_030", "700_040")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$qfMfm_700_010}
      if (i == 2) { dataIn <- dataList$qfMfm_700_020}
      if (i == 3) { dataIn <- dataList$qfMfm_700_030}
      if (i == 4) { dataIn <- dataList$qfMfm_700_040}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idQfqm, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <-  c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                         "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idQfqmHorVer <- H5Oopen(idQfqm,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrMfmList)), function(x) h5writeAttribute(attrMfmList[x,"Values"], h5obj = idQfqm, name = attrMfmList[x,"fieldName"]))
    
    #Writing Data for mfcVali############################################################
    #data table
    idData <- H5Oopen(idDataLvlDp0p,"mfcVali_001")
    h5writeDataset.data.frame(obj = dataList$mfcVali, h5loc = idData, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrMfcValiList)), function(x) h5writeAttribute(attrMfcValiList[x,"Values"], h5obj = idData, name = attrMfcValiList[x,"fieldName"]))
    
    #write attribute to the data table level
    dataIn <- dataList$mfcVali
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("m3 s-1", "m3 s-1", "m3 s-1", "Pa", "K", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    #qf table
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"mfcVali_001")
    h5writeDataset.data.frame(obj = dataList$qfMfcVali, h5loc = idQfqm, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrMfcValiList)), function(x) h5writeAttribute(attrMfcValiList[x,"Values"], h5obj = idQfqm , name = attrMfcValiList[x,"fieldName"]))
    
    #write attribute to the data table level
    dataIn <- dataList$qfMfcVali
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                      "NA", "NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idQfqmHorVer <- H5Oopen(idQfqm,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    
    #Writing Data for irgaMfcSamp ############################################################
    #data table
    idData <- H5Oopen(idDataLvlDp0p,"irgaMfcSamp_001")
    h5writeDataset.data.frame(obj = dataList$mfcVali, h5loc = idData, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrMfcSampList)), function(x) h5writeAttribute(attrMfcSampList[x,"Values"], h5obj = idData, name = attrMfcSampList[x,"fieldName"]))
    
    #write attribute to the data table level
    dataIn <- dataList$irgaMfcSamp
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("m3 s-1", "m3 s-1", "m3 s-1", "Pa", "K", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    #qf table
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"irgaMfcSamp_001")
    h5writeDataset.data.frame(obj = dataList$qfMfcVali, h5loc = idQfqm, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrMfcSampList)), function(x) h5writeAttribute(attrMfcSampList[x,"Values"], h5obj = idQfqm , name = attrMfcSampList[x,"fieldName"]))
    
    #write attribute to the data table level
    dataIn <- dataList$qfIrgaMfcSamp
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                      "NA", "NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idQfqmHorVer <- H5Oopen(idQfqm,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    
    #Writing Data for pump ############################################################
    idData <- H5Oopen(idDataLvlDp0p,"pump_001")
    #write attribute to the data table level for each location
    tempLoc <- c("700_000", "700_010", "700_020", "700_030", "700_040")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$pump_700_000}
      if (i == 2) { dataIn <- dataList$pump_700_010}
      if (i == 3) { dataIn <- dataList$pump_700_020}
      if (i == 4) { dataIn <- dataList$pump_700_030}
      if (i == 5) { dataIn <- dataList$pump_700_040}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("V", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrPumpList)), function(x) h5writeAttribute(attrPumpList[x,"Values"], h5obj = idData, name = attrPumpList[x,"fieldName"]))
    
    #qf table for each measurement level
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"pump_001")
    #write attribute to the data table level for each location
    tempLoc <- c("700_000", "700_010", "700_020", "700_030", "700_040")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$qfPump_700_000}
      if (i == 2) { dataIn <- dataList$qfPump_700_010}
      if (i == 3) { dataIn <- dataList$qfPump_700_020}
      if (i == 4) { dataIn <- dataList$qfPump_700_030}
      if (i == 5) { dataIn <- dataList$qfPump_700_040}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idQfqm, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <-  c("NA", "NA", "NA", "NA", "NA", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idQfqmHorVer <- H5Oopen(idQfqm,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrPumpList)), function(x) h5writeAttribute(attrPumpList[x,"Values"], h5obj = idQfqm, name = attrPumpList[x,"fieldName"]))
    
    #Writing dp0p for crdCo2############################################################
    idData <- H5Oopen(idDataLvlDp0p,"crdCo2_001")
    #write attribute to the data table level for each measurement level
    tempLoc <- c("000_010", "000_020", "000_030", "000_040")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$crdCo2_000_010}
      if (i == 2) { dataIn <- dataList$crdCo2_000_020}
      if (i == 3) { dataIn <- dataList$crdCo2_000_030}
      if (i == 4) { dataIn <- dataList$crdCo2_000_040}
      #write data table for each measurement level
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("permill", "NA", "Pa", "molCo2 mol-1",
                                        "molCo2 mol-1", "molCo2 mol-1", "molCo2 mol-1", "molCo2 mol-1",
                                        "molCo2 mol-1", "molCo2 mol-1", "molH2o mol-1", "NA",
                                        "K", "K", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrCrdCo2List)), function(x) h5writeAttribute(attrCrdCo2List[x,"Values"], h5obj = idData, name = attrCrdCo2List[x,"fieldName"]))
    
    #write attribute to the data table level for each gas concentration
    tempLoc <- c("co2Arch", "co2Low", "co2Med", "co2High")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$crdCo2_co2Arch}
      if (i == 2) { dataIn <- dataList$crdCo2_co2Low}
      if (i == 3) { dataIn <- dataList$crdCo2_co2Med}
      if (i == 4) { dataIn <- dataList$crdCo2_co2High}
      
      #write data table for each measurement level
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("permill", "permill", "NA",
                                        "Pa", "molCo2 mol-1", "molCo2 mol-1",
                                        "molCo2 mol-1", "molCo2 mol-1", "molCo2 mol-1",
                                        "molCo2 mol-1", "molH2o mol-1", "molCo2 mol-1",
                                        "molCo2 mol-1", "molCo2 mol-1", "molH2o mol-1",
                                        "NA", "K", "K",
                                        "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    
    #qf table for each measurement level
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"crdCo2_001")
    #write attribute to the data table level for each mesurment level and gas concentration
    tempLoc <- c("000_010", "000_020", "000_030", "000_040", "co2Arch", "co2Low", "co2Med", "co2High")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$qfCrdCo2_000_010}
      if (i == 2) { dataIn <- dataList$qfCrdCo2_000_020}
      if (i == 3) { dataIn <- dataList$qfCrdCo2_000_030}
      if (i == 4) { dataIn <- dataList$qfCrdCo2_000_040}
      if (i == 5) { dataIn <- dataList$qfCrdCo2_co2Arch}
      if (i == 6) { dataIn <- dataList$qfCrdCo2_co2Low}
      if (i == 7) { dataIn <- dataList$qfCrdCo2_co2Med}
      if (i == 8) { dataIn <- dataList$qfCrdCo2_co2High}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idQfqm, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idQfqmHorVer <- H5Oopen(idQfqm,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrCrdCo2List)), function(x) h5writeAttribute(attrCrdCo2List[x,"Values"], h5obj = idQfqm, name = attrCrdCo2List[x,"fieldName"]))
    
    
    #Writing dp0p for crdCo2ValvLvl############################################################
    idData <- H5Oopen(idDataLvlDp0p,"crdCo2ValvLvl_001")
    
    h5writeDataset.data.frame(obj = dataList$crdCo2ValvLvl_702_000, h5loc = idData, name = "702_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrValvList)), function(x) h5writeAttribute(attrValvList[x,"Values"], h5obj = idData, name = attrValvList[x,"fieldName"]))
    #write attribute to the data table level 
    dataIn <- dataList$crdCo2ValvLvl_702_000
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"702_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    #Writing dp0p for crdH2o############################################################
    idData <- H5Oopen(idDataLvlDp0p,"crdH2o_001")
    #write attribute to the data table level for each measurement level
    tempLoc <- c("000_010", "000_020", "000_030", "000_040")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$crdH2o_000_010}
      if (i == 2) { dataIn <- dataList$crdH2o_000_020}
      if (i == 3) { dataIn <- dataList$crdH2o_000_030}
      if (i == 4) { dataIn <- dataList$crdH2o_000_040}
      #write data table for each measurement level
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("permill", "permill", "Pa", "molH2o mol-1", "molH2o mol-1",
                                        "NA", "NA", "K", "K", "NA",
                                        "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrCrdH2oList)), function(x) h5writeAttribute(attrCrdH2oList[x,"Values"], h5obj = idData, name = attrCrdH2oList[x,"fieldName"]))
    
    #for validation period
    h5writeDataset.data.frame(obj = dataList$crdH2o_700_000, h5loc = idData, name = "700_000", DataFrameAsCompound = FALSE)
    
    #write attribute to the data table level 
    dataIn <- dataList$crdH2o_700_000
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("permill", "permill", "permill", "permill", "permill",
                                      "permill", "permill", "permill", "Pa", "molH2o mol-1", 
                                      "molH2o mol-1", "NA", "NA", "K", "K", 
                                      "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    #qf table for each measurement level
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"crdH2o_001")
    #write attribute to the data table level for each mesurment level and gas concentration
    tempLoc <- c("000_010", "000_020", "000_030", "000_040", "700_000")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$qfCrdH2o_000_010}
      if (i == 2) { dataIn <- dataList$qfCrdH2o_000_020}
      if (i == 3) { dataIn <- dataList$qfCrdH2o_000_030}
      if (i == 4) { dataIn <- dataList$qfCrdH2o_000_040}
      if (i == 5) { dataIn <- dataList$qfCrdH2o_700_000}
      
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idQfqm, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                                        "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idQfqmHorVer <- H5Oopen(idQfqm,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrCrdH2oList)), function(x) h5writeAttribute(attrCrdH2oList[x,"Values"], h5obj = idQfqm, name = attrCrdH2oList[x,"fieldName"]))
    
    #Writing dp0p for crdH2oValvLvl############################################################
    idData <- H5Oopen(idDataLvlDp0p,"crdH2oValvLvl_001")
    
    h5writeDataset.data.frame(obj = dataList$crdH2oValvLvl_704_000, h5loc = idData, name = "704_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrValvList)), function(x) h5writeAttribute(attrValvList[x,"Values"], h5obj = idData, name = attrValvList[x,"fieldName"]))
    #write attribute to the data table level 
    dataIn <- dataList$crdH2oValvLvl_704_000
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"704_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    #Writing dp0p for crdH2oValvVali############################################################
    idData <- H5Oopen(idDataLvlDp0p,"crdH2oValvVali_001")
    
    h5writeDataset.data.frame(obj = dataList$crdH2oValvVali_700_000, h5loc = idData, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrCrdH2oValvValiList)), function(x) h5writeAttribute(attrCrdH2oValvValiList[x,"Values"], h5obj = idData, name = attrCrdH2oValvValiList[x,"fieldName"]))
    #write attribute to the data table level 
    dataIn <- dataList$crdH2oValvVali_700_000
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    #Writing dp0p for h2o90Refe############################################################
    idData <- H5Oopen(idDataLvlDp0p,"h2o90Refe_001")
    h5writeDataset.data.frame(obj = dataList$h2o90Refe_700_000, h5loc = idData, name = "700_000", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrH2o90RefeList)), function(x) h5writeAttribute(attrH2o90RefeList[x,"Values"], h5obj = idData, name = attrH2o90RefeList[x,"fieldName"]))
    #write attribute to the data table level 
    dataIn <- dataList$h2o90Refe_700_000
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("permill", "permill", "permill", "permill", "permill", 
                                      "permill", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"700_000")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    #write attribute to the validation level
    lapply(seq_len(nrow(attrH2o90RefeValiList)), function(x) h5writeAttribute(attrH2o90RefeValiList[x,"Values"], h5obj = idDataHorVer, name = attrH2o90RefeValiList[x,"fieldName"]))
    
    #Writing L0p for saats ############################################################ 
    idData <- H5Oopen(idDataLvlDp0p,"tempAirLvl_001")
    #write attribute to the data table level for each measurement level
    tempLoc <- c("000_010", "000_020", "000_030")
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$tempAirLvl_000_010}
      if (i == 2) { dataIn <- dataList$tempAirLvl_000_020}
      if (i == 3) { dataIn <- dataList$tempAirLvl_000_030}
      
      #write data table for each measurement level
      h5writeDataset.data.frame(obj = dataIn, h5loc = idData, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("K", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idDataHorVer <- H5Oopen(idData,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrTempAirLvlList)), function(x) h5writeAttribute(attrTempAirLvlList[x,"Values"], h5obj = idData, name = attrTempAirLvlList[x,"fieldName"]))
    
    #qf table for each measurement level
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"tempAirLvl_001")
    #write attribute to the data table level for each mesurment level and gas concentration
    tempLoc <- c("000_010", "000_020", "000_030")
    
    for (i in 1:length(tempLoc)){
      if (i == 1) { dataIn <- dataList$qfTempAirLvl_000_010}
      if (i == 2) { dataIn <- dataList$qfTempAirLvl_000_020}
      if (i == 3) { dataIn <- dataList$qfTempAirLvl_000_030}
      
      h5writeDataset.data.frame(obj = dataIn, h5loc = idQfqm, name = tempLoc[i], DataFrameAsCompound = FALSE)
      names(dataIn) <- colnames(dataIn)
      #Creating the index to organize the variables in alphabetical order
      idxOrd <- order(names(dataIn))
      #Changing the order of the variables to alphabetical order using the index
      dataIn <- dataIn[,idxOrd]
      #add attribute to data table
      attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA")
      #Sorting the order of units to match the colnames
      attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
      #write attribute to each measurement level
      idQfqmHorVer <- H5Oopen(idQfqm,tempLoc[i])
      h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
      h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    }
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrTempAirLvlList)), function(x) h5writeAttribute(attrTempAirLvlList[x,"Values"], h5obj = idQfqm, name = attrTempAirLvlList[x,"fieldName"]))
    
    #Writing L0p for traats ############################################################   
    idData <- H5Oopen(idDataLvlDp0p,"tempAirTop_001")
    h5writeDataset.data.frame(obj = dataList$tempAirTop, h5loc = idData, name = "000_040", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrTempAirTop)), function(x) h5writeAttribute(attrTempAirTop[x,"Values"], h5obj = idData, name = attrTempAirTop[x,"fieldName"]))
    
    #write attribute to the data table level 
    dataIn <- dataList$tempAirTop
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("K", "K", "K", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idDataHorVer <- H5Oopen(idData,"000_040")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idDataHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idDataHorVer, name = "Name")
    
    #Writing qf table for traats
    idQfqm <- H5Oopen(idQfqmLvlDp0p,"tempAirTop_001")
    h5writeDataset.data.frame(obj = dataList$qfTempAirTop, h5loc = idQfqm, name = "000_040", DataFrameAsCompound = FALSE)
    #write attribute to the sensor level
    lapply(seq_len(nrow(attrTempAirTop)), function(x) h5writeAttribute(attrTempAirTop[x,"Values"], h5obj = idQfqm , name = attrTempAirTop[x,"fieldName"]))
    
    #write attribute to the data table level 
    dataIn <- dataList$qfTempAirTop
    names(dataIn) <- colnames(dataIn)
    #Creating the index to organize the variables in alphabetical order
    idxOrd <- order(names(dataIn))
    #Changing the order of the variables to alphabetical order using the index
    dataIn <- dataIn[,idxOrd]
    #add attribute to data table
    attr(dataIn, which = "unit") <- c("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
    #Sorting the order of units to match the colnames
    attributes(dataIn)$unit <- attributes(dataIn)$unit[idxOrd]
    #write attribute to each location
    idQfqmHorVer <- H5Oopen(idQfqm,"000_040")
    h5writeAttribute(attributes(dataIn)$unit, h5obj = idQfqmHorVer, name = "Unit")
    h5writeAttribute(attributes(dataIn)$names, h5obj = idQfqmHorVer, name = "Name")
    
    ##################################### dp01 ############################################
    #Writing dp01 for irgaCo2############################################################
    idDataDp01 <- H5Oopen(idDataLvlDp01,"irgaCo2")
    #write attribute to the data table level for each measurement level
    tempLoc <- c("000_010", "000_020", "000_030", "000_040")
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(rtioMoleWetCo2 = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   rtioMoleDryCo2 = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   temp = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   pres = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   frt00 = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)))
    for (i in 1:length(tempLoc)){
      idDataHorVer <- H5Gcreate(idDataDp01,tempLoc[i])
      H5Gclose(idDataHorVer)
      if (i == 1) { dataIn <- dataIn}
      if (i == 2) { dataIn <- dataIn}
      if (i == 3) { dataIn <- dataIn}
      if (i == 4) { dataIn <- dataIn}
      
      #write data table for each measurement level
      lapply(names(dataIn), function(x) {
        idDataHorVer <-H5Gopen(idDataDp01,tempLoc[i])
        rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
      })}
    #write attribute to the data table level for each gas concentration
    tempLoc <- c("co2Arch", "co2Low", "co2Med", "co2High", "co2Zero")
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(rtioMoleWetCo2 = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   rtioMoleDryCo2 = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   temp = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   pres = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   frt00 = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   rtioMoleDryCo2Refe = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)))
    
    for (i in 1:length(tempLoc)){
      idDataHorVer <- H5Gcreate(idDataDp01,tempLoc[i])
      H5Gclose(idDataHorVer)
      if (i == 1) { dataIn <- dataIn}
      if (i == 2) { dataIn <- dataIn}
      if (i == 3) { dataIn <- dataIn}
      if (i == 4) { dataIn <- dataIn}
      if (i == 5) { dataIn <- dataIn}
      #write data table for each measurement level
      lapply(names(dataIn), function(x) {
        idDataHorVer <-H5Gopen(idDataDp01,tempLoc[i])
        rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
      })}
    
    #qf table for each measurement level
    idQfqmDp01 <- H5Oopen(idQfqmLvlDp01,"irgaCo2")
    #write attribute to the data table level for each measurement level
    tempLoc <- c("000_010", "000_020", "000_030", "000_040")
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(rtioMoleWetCo2 = data.frame(qfFinl=c(0), qfSci =c(0)),
                   rtioMoleDryCo2 = data.frame(qfFinl=c(0), qfSci =c(0)),
                   temp = data.frame(qfFinl=c(0), qfSci =c(0)),
                   pres = data.frame(qfFinl=c(0), qfSci =c(0)),
                   frt00 = data.frame(qfFinl=c(0), qfSci =c(0)))
    for (i in 1:length(tempLoc)){
      idDataHorVer <- H5Gcreate(idQfqmDp01,tempLoc[i])
      H5Gclose(idDataHorVer)
      if (i == 1) { dataIn <- dataIn}
      if (i == 2) { dataIn <- dataIn}
      if (i == 3) { dataIn <- dataIn}
      if (i == 4) { dataIn <- dataIn}
      #write data table for each measurement level
      lapply(names(dataIn), function(x) {
        idDataHorVer <-H5Gopen(idQfqmDp01,tempLoc[i])
        rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
      })}
    
    #write attribute to the data table level for each gas concentration
    tempLoc <- c("co2Arch", "co2Low", "co2Med", "co2High", "co2Zero")
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(rtioMoleWetCo2 = data.frame(qfFinl=c(0), qfSci =c(0)),
                   rtioMoleDryCo2 = data.frame(qfFinl=c(0), qfSci =c(0)),
                   temp = data.frame(qfFinl=c(0), qfSci =c(0)),
                   pres = data.frame(qfFinl=c(0), qfSci =c(0)),
                   frt00 = data.frame(qfFinl=c(0), qfSci =c(0)),
                   rtioMoleDryCo2Refe = data.frame(qfFinl=c(NaN), qfSci =c(NaN)))
    
    for (i in 1:length(tempLoc)){
      idDataHorVer <- H5Gcreate(idQfqmDp01,tempLoc[i])
      H5Gclose(idDataHorVer)
      if (i == 1) { dataIn <- dataIn}
      if (i == 2) { dataIn <- dataIn}
      if (i == 3) { dataIn <- dataIn}
      if (i == 4) { dataIn <- dataIn}
      if (i == 5) { dataIn <- dataIn}
      #write data table for each measurement level
      lapply(names(dataIn), function(x) {
        idDataHorVer <-H5Gopen(idQfqmDp01,tempLoc[i])
        rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
      })}
    
    #Writing dp01 for irgaH2o############################################################
    idDataDp01 <- H5Oopen(idDataLvlDp01,"irgaH2o")
    #write attribute to the data table level for each measurement level
    tempLoc <- c("000_010", "000_020", "000_030", "000_040", "co2Arch", "co2Low", "co2Med", "co2High", "co2Zero")
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(rtioMoleWetH2o = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   rtioMoleDryH2o = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   temp = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   pres = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)),
                   frt00 = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)))
    for (i in 1:length(tempLoc)){
      idDataHorVer <- H5Gcreate(idDataDp01,tempLoc[i])
      H5Gclose(idDataHorVer)
      if (i == 1) { dataIn <- dataIn}
      if (i == 2) { dataIn <- dataIn}
      if (i == 3) { dataIn <- dataIn}
      if (i == 4) { dataIn <- dataIn}
      if (i == 5) { dataIn <- dataIn}
      if (i == 6) { dataIn <- dataIn}
      if (i == 7) { dataIn <- dataIn}
      if (i == 8) { dataIn <- dataIn}
      if (i == 9) { dataIn <- dataIn}
      #write data table for each measurement level
      lapply(names(dataIn), function(x) {
        idDataHorVer <-H5Gopen(idDataDp01,tempLoc[i])
        rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
      })}  
    
    #qf table for each measurement level
    idQfqmDp01 <- H5Oopen(idQfqmLvlDp01,"irgaH2o")
    #write attribute to the data table level for each measurement level
    tempLoc <- c("000_010", "000_020", "000_030", "000_040", "co2Arch", "co2Low", "co2Med", "co2High", "co2Zero")
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(rtioMoleWetH2o = data.frame(qfFinl=c(0), qfSci =c(0)),
                   rtioMoleDryH2o = data.frame(qfFinl=c(0), qfSci =c(0)),
                   temp = data.frame(qfFinl=c(0), qfSci =c(0)),
                   pres = data.frame(qfFinl=c(0), qfSci =c(0)),
                   frt00 = data.frame(qfFinl=c(0), qfSci =c(0)))
    for (i in 1:length(tempLoc)){
      idDataHorVer <- H5Gcreate(idQfqmDp01,tempLoc[i])
      H5Gclose(idDataHorVer)
      if (i == 1) { dataIn <- dataIn}
      if (i == 2) { dataIn <- dataIn}
      if (i == 3) { dataIn <- dataIn}
      if (i == 4) { dataIn <- dataIn}
      if (i == 5) { dataIn <- dataIn}
      if (i == 6) { dataIn <- dataIn}
      if (i == 7) { dataIn <- dataIn}
      if (i == 8) { dataIn <- dataIn}
      if (i == 9) { dataIn <- dataIn}
      
      #write data table for each measurement level
      lapply(names(dataIn), function(x) {
        idDataHorVer <-H5Gopen(idQfqmDp01,tempLoc[i])
        rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
      })}# close loop for  
    
    #Writing dp01 for saats ############################################################ 
    idDataDp01 <- H5Oopen(idDataLvlDp01,"tempAirLvl") 
    #write attribute to the data table level for each measurement level
    tempLoc <- c("000_010", "000_020", "000_030")
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(temp = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)))
    for (i in 1:length(tempLoc)){
      idDataHorVer <- H5Gcreate(idDataDp01,tempLoc[i])
      H5Gclose(idDataHorVer)
      if (i == 1) { dataIn <- dataIn}
      if (i == 2) { dataIn <- dataIn}
      if (i == 3) { dataIn <- dataIn} 
      
      #write data table for each measurement level
      lapply(names(dataIn), function(x) {
        idDataHorVer <-H5Gopen(idDataDp01,tempLoc[i])
        rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
      })}# close loop for 
    
    #qf table for each measurement level
    idQfqmDp01 <- H5Oopen(idQfqmLvlDp01,"tempAirLvl")
    tempLoc <- c("000_010", "000_020", "000_030")
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(temp = data.frame(qfFinl=c(0), qfSci =c(0)))
    for (i in 1:length(tempLoc)){
      idDataHorVer <- H5Gcreate(idQfqmDp01 ,tempLoc[i])
      H5Gclose(idDataHorVer)
      if (i == 1) { dataIn <- dataIn}
      if (i == 2) { dataIn <- dataIn}
      if (i == 3) { dataIn <- dataIn} 
      
      #write data table for each measurement level
      lapply(names(dataIn), function(x) {
        idDataHorVer <-H5Gopen(idQfqmDp01,tempLoc[i])
        rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
      })}# close loop for
    
    #Writing dp01 for traats ############################################################   
    idDataDp01 <- H5Oopen(idDataLvlDp01,"tempAirTop")
    idDataHorVer <- H5Gcreate(idDataDp01,"000_040")
    H5Gclose(idDataHorVer)
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(temp = data.frame(mean=c(0), min = c(0), max = c(0), vari =c(0), numSamp = c(0), se =c(0)))
    lapply(names(dataIn), function(x) {
      idDataHorVer <-H5Gopen(idDataDp01,"000_040")
      rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
    })
    
    #qf table for each measurement level
    idQfqmDp01 <- H5Oopen(idQfqmLvlDp01,"tempAirTop")
    idDataHorVer <- H5Gcreate(idQfqmDp01,"000_040")
    H5Gclose(idDataHorVer)
    #generate a fake dataframe for now and will replace with the real data when they are available
    dataIn <- list(temp = data.frame(qfFinl=c(0), qfSci =c(0)))
    #write data table for each measurement level
    lapply(names(dataIn), function(x) {
      idDataHorVer <-H5Gopen(idQfqmDp01,"000_040")
      rhdf5::h5writeDataset.data.frame(obj = dataIn[[x]], h5loc = idDataHorVer, name = x, DataFrameAsCompound = TRUE)
    })
    #Close all the connections to the file before exiting #####################################################
    H5Gclose(idDataHorVer)
    H5Gclose(idQfqmHorVer) 
    H5Gclose(idData)
    H5Gclose(idSite)
    H5Fclose(idFile)
    H5close()
    
  }
  
}
