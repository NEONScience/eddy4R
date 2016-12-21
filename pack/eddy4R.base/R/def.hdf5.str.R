######################################################################################################################################################################################
#create an HDF5 file with NEON group structure

library("rhdf5")
library(eddy4R.base)
library(XLConnect)

######################################################################################################################
#Examples
######################################################################################################################
#Example run to create L0 gold files for 4/24/2016 at SERC
#Setting site
#site <- "SERC"

#levlTowr <- "000_060"

#levlDp <- "dp01"

#Setting date to be processed
#date <- "2016-04-24"


##############################################################################################################
#Start of function call to generate NEON HDF5 files
##############################################################################################################
def.hdf5.crte <- function(date, site = "SERC", levlTowr, levlDp = "dp01")
  
  
  #fomatting date for file names
  dateFileIn <- gsub(pattern = "-", replacement = "", x = date)
  
  #Directory where the data is being written, need to change locally to add N:
  datDirOut <- paste("/home/ddurden/eddy/data/L0prime_gold/", site,"/", dateFileIn,"/", sep = "")
  
  
  #Check to see if the directory exists, if not create the directory. Recursive required to write nested file directories
  if (dir.exists(datDirOut) == FALSE) dir.create(datDirOut, recursive = TRUE)
  
  #Create a connection to the workbook
  wk <- loadWorkbook("/home/ddurden/eddy/data/Thresholds_EC/NEON_HDF5_metadata.xlsx") 
  
  #Read the workbook into a data frame
  metaList <- readWorksheet(wk, sheet="TIS", check.names = F) 
  
  
  attrSiteList <- metaList[which(metaList$`HDF5 Site Group` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  attrDpNameList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  attrDataList <- metaList[which(metaList$`Data table specific` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  
  #Create a list of all the L0 DPs for creation of group hierarchy (fard)
  grpList <- c("irga","soni","soniAmrs","irgaMfcSamp")
  #              ,"irgaGasCyl","irgaMfcVali",
  #              "irgaPresTrap","irgaPresValiLine","irgaPresValiRegIn",
  #              "irgaPresValiRegOut","irgaPump","irgaSndLeakHeat",
  #              "irgaSndValiHut","irgaSndValiNema",)
  #The DP level, the data product ID and the Rev number
  grpList <- paste(grpList, "_001", sep = "")
  
  #Create the file, create a class
  #Create the file, create a class
  fid <- H5Fcreate(paste0(datDirOut,"ECTE_",levlDp,"_", site, "_", date, "_new_format.h5"))
  #If the file is already created use:
  #fid <- H5Fopen("HDF5TIS_L0_prototype.h5")
  
  #Create a group level for SERC
  gid <- H5Gcreate(fid, site) 
  #If the group is already created use:
  #gid <- H5Gopen(fid,"SERC")
  dp0pId <- H5Gcreate(gid,"dp0p")
  dp01Id <- H5Gcreate(gid,"dp01")
  
  dlid <- H5Gcreate(dp0pId,"data")
  dl1id <- H5Gcreate(dp01Id,"data")
  
  qflid <- H5Gcreate(dp0pId,"qfqm")
  qf1id <- H5Gcreate(dp01Id,"qfqm")
  
  #Create a function to create group levels for each L0 DP
  #grpCrte <- function(x) H5Gcreate(gid, x)
  #grpCrte <- function(x) H5Gcreate(dplid, x)
  
  #Apply the function to produce the group levels in the data file
  #lapply(grpList, grpCrte)
  
  #Creating level 0p file structures
  lapply(grpList, function(x) H5Gcreate(dlid, x))
  lapply(grpList, function(x) H5Gcreate(qflid, x))
  
  #Creating level 1 file structures
  
  grpListDp01 <- c("amrs", "irgaCo2", "irgaH2o", "soni")
  lapply(grpListDp01, function(x) H5Gcreate(dl1id, x))
  lapply(grpListDp01, function(x) H5Gcreate(qf1id, x))
  
  
  lapply(seq_len(nrow(attrSiteList)), function(x) h5writeAttribute(attrSiteList[x,"Field Description"], h5obj = gid, name = attrSiteList[x,"fieldName"]))
  
  #Used to write the datasets into the groups with attributes attached.
  #h5writeDataset.character(obj = dataList$soni[,"time"], file = gid, name = "DP0_soni/000_000_ts", write.attributes = TRUE)
  dgid <- H5Oopen(dlid,"soni_001")
  #h5writeDataset.data.frame(obj = dataSoni[,which(!names(dataSoni) == c("diag"))], h5loc = dgid, name = levlTowr, DataFrameAsCompound = FALSE)
  
  qfid <- H5Oopen(qflid,"soni_001")
  #h5writeDataset.data.frame(obj = qfSoniOut, h5loc = qfid, name = levlTowr, DataFrameAsCompound = FALSE)
  #h5writeAttribute(attributes(dataList$soni)$unit, h5obj = dgid, name = "unit")
  #h5writeAttribute(attributes(dataList$soni)$names, h5obj = dgid, name = "names")
  lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = dgid, name = attrDpNameList[x,"fieldName"]))
  d02gid <- H5Gcreate(dgid,levlTowr)
  lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = d02gid, name = attrDataList[x,"fieldName"]))
  #H5Gclose(dgid)
  
  #Writing Data for SoniAmrs############################################################
  dgid <- H5Oopen(dlid,"soniAmrs_001")
  h5writeDataset.data.frame(obj = dataSoniAmrs[,which(!names(dataSoniAmrs) == c("diag32"))], h5loc = dgid, name = levlTowr, DataFrameAsCompound = FALSE)
  
  qfid <- H5Oopen(qflid,"soniAmrs_001")
  #h5writeDataset.data.frame(obj = qfSoniAmrsOut, h5loc = qfid, name = levlTowr, DataFrameAsCompound = FALSE)
  #h5writeAttribute(attributes(dataList$soniAmrs)$unit, h5obj = dgid, name = "unit")
  #h5writeAttribute(attributes(dataList$soniAmrs)$names, h5obj = dgid, name = "names")
  lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = dgid, name = attrDpNameList[x,"fieldName"]))
  d02gid <- H5Gcreate(dgid,levlTowr)
  lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = d02gid, name = attrDataList[x,"fieldName"]))
  
  #Writing IRGA data####################################################################
  dgid <- H5Oopen(dlid,"irga_001") #Open H5 connection
  #Write out dataset to data
  h5writeDataset.data.frame(obj = dataIrga[,which(!names(dataIrga) == c("diag01", "diag02"))], h5loc = dgid, name = levlTowr, DataFrameAsCompound = FALSE) 
  
  qfid <- H5Oopen(qflid,"irga_001")
  #h5writeDataset.data.frame(obj = qfIrgaOut, h5loc = qfid, name = levlTowr, DataFrameAsCompound = FALSE)
  #h5writeAttribute(attributes(dataList$irga)$unit, h5obj = dgid, name = "unit")
  #h5writeAttribute(attributes(dataList$irga)$names, h5obj = dgid, name = "names")
  lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = dgid, name = attrDpNameList[x,"fieldName"]))
  d02gid <- H5Gcreate(dgid,levlTowr)
  lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = d02gid, name = attrDataList[x,"fieldName"]))
  
  #Used to write the datasets into the groups with attributes attached.
  #h5writeDataset.character(obj = dataList$irgaMfcSamp[,"time"], file = gid, name = "DP0_irgaMfcSamp/000_000_ts", write.attributes = TRUE)
  dgid <- H5Oopen(dlid,"irgaMfcSamp_001")
  #h5writeDataset.data.frame(obj = dataIrgaMfcSamp, h5loc = dgid, name = levlTowr, DataFrameAsCompound = FALSE)
  
  qfid <- H5Oopen(qflid,"irgaMfcSamp_001")
  #h5writeDataset.data.frame(obj = qfIrgaMfcSampOut, h5loc = qfid, name = levlTowr, DataFrameAsCompound = FALSE)
  #h5writeAttribute(attributes(dataList$irgaMfcSamp)$unit, h5obj = dgid, name = "unit")
  #h5writeAttribute(attributes(dataList$irgaMfcSamp)$names, h5obj = dgid, name = "names")
  lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = dgid, name = attrDpNameList[x,"fieldName"]))
  d02gid <- H5Gcreate(dgid,levlTowr)
  lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = d02gid, name = attrDataList[x,"fieldName"]))
  
  
  #Close all the connections to the file before exiting
  H5Gclose(dgid)
  H5Gclose(gid)
  H5Fclose(fid)
  H5close()