##############################################################################################
#' @title Definition function: Create the ECTE HDF5 file structure

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. Function creates the standard NEON HDF5 file structure for the ECTE data products.
#' @param \code{Date} is the date for the output file being generated.
#' @param \code{Site} is the site for which the output file is being generated.
#' @param \code{LevlTowr} is the measurement level of the tower top to determine the VER number of the NEON DP naming convention.
#' @param \code{DirOut} is the output directory where the file being generated is stored.
#' @param \code{LevlDp} is output file DP level for the file naming.

#' @return A NEON formatted HDF5 file that is output to /code{DirOut} 

#' @references Currently none.

#' @keywords NEON, HDF5, eddy-covariance, ECTE

#' @examples 

#'#Example run to create L0 gold files for 4/24/2016 at SERC
#'#Setting Site
#'Site <- "SERC"
#'LevlTowr <- "000_060"
#'LevlDp <- "dp01"

#'#Setting Date to be processed
#'Date <- "2016-04-24"

#'#Set directory for example output to working directory
#'DirOut <- getwd()

#'#Running example
#'def.hdf5.crte(Date = Date, Site = Site, LevlTowr = LevlTowr, DirOut = DirOut, LevlDp)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-12-22)
#     original creation

##############################################################################################################
#Start of function call to generate NEON HDF5 files
##############################################################################################################

def.hdf5.crte <- function(
  Date, 
  Site = "SERC", 
  LevlTowr, 
  DirOut, 
  LevlDp = "dp01"
  ) {
  
  
  #fomatting Date for file names
  dateFileIn <- gsub(pattern = "-", replacement = "", x = Date)
  
  #Directory where the data is being written, need to change locally to add N:
  #datDirOut <- paste("/home/ddurden/eddy/data/L0prime_gold/", Site,"/", dateFileIn,"/", sep = "")
  
  
  #Check to see if the directory exists, if not create the directory. Recursive required to write nested file directories
  if (dir.exists(DirOut) == FALSE) dir.create(DirOut, recursive = TRUE)
  
  #Create a connection to the workbook
  #wk <- loadWorkbook("/home/ddurden/eddy/data/Thresholds_EC/NEON_HDF5_metadata.xlsx") 
  
  #Read the workbook into a data frame
  #metaList <- readWorksheet(wk, sheet="TIS", check.names = F) 
  
  
  #attrSiteList <- metaList[which(metaList$`HDF5 Site Group` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  #attrDpNameList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  #attrDataList <- metaList[which(metaList$`Data table specific` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  
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
  fileGrpId <- rhdf5::H5Fcreate(paste0(DirOut,"/","ECTE_",LevlDp,"_", Site, "_", Date, "_new_format.h5"))
  #If the file is already created use:
  #fileGrpId <- H5Fopen("HDF5TIS_L0_prototype.h5")
  
  #Create a group level for SERC
  siteGrpId <- rhdf5::H5Gcreate(fileGrpId, Site) 
  #If the group is already created use:
  #siteGrpId <- H5Gopen(fileGrpId,"SERC")
  dp0pId <- rhdf5::H5Gcreate(siteGrpId,"dp0p")
  dp01Id <- rhdf5::H5Gcreate(siteGrpId,"dp01")
  
  dataLvlDp0pId <- rhdf5::H5Gcreate(dp0pId,"data")
  dataLvlDp01Id <- rhdf5::H5Gcreate(dp01Id,"data")
  
  qfqmLvlDp0pId <- rhdf5::H5Gcreate(dp0pId,"qfqm")
  qfqmLvlDp01Id <- rhdf5::H5Gcreate(dp01Id,"qfqm")
  
  #Create a function to create group levels for each L0 DP
  #grpCrte <- function(x) H5Gcreate(siteGrpId, x)
  #grpCrte <- function(x) H5Gcreate(dplid, x)
  
  #Apply the function to produce the group levels in the data file
  #lapply(grpList, grpCrte)
  
  #Creating level 0p file structures
  lapply(grpList, function(x) rhdf5::H5Gcreate(dataLvlDp0pId, x))
  lapply(grpList, function(x) rhdf5::H5Gcreate(qfqmLvlDp0pId, x))
  
  #Creating level 1 file structures
  
  grpListDp01 <- c("soniAmrs", "irgaCo2", "irgaH2o", "soni")
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(dataLvlDp01Id, x))
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(qfqmLvlDp01Id, x))
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(dataLvlDp01Id, paste0(x,"/",LevlTowr,"_30m")))
  lapply(grpListDp01, function(x) {
    print(x)
    if(x == "soni"){rhdf5::H5Gcreate(dataLvlDp01Id, paste0(x,"/",LevlTowr,"_02m"))} else {rhdf5::H5Gcreate(dataLvlDp01Id, paste0(x,"/",LevlTowr,"_01m"))}})
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(qfqmLvlDp01Id, paste0(x,"/",LevlTowr,"_30m")))
  lapply(grpListDp01, function(x) {
    if(x == "soni") {rhdf5::H5Gcreate(qfqmLvlDp01Id, paste0(x,"/",LevlTowr,"_02m"))} else {rhdf5::H5Gcreate(qfqmLvlDp01Id, paste0(x,"/",LevlTowr,"_01m"))}})
  #lapply(seq_len(nrow(attrSiteList)), function(x) h5writeAttribute(attrSiteList[x,"Field Description"], h5obj = siteGrpId, name = attrSiteList[x,"fieldName"]))
  
  #Used to write the datasets into the groups with attributes attached.
  dataGrpId <- rhdf5::H5Oopen(dataLvlDp0pId,"soni_001")
  qfqmGrpId <- rhdf5::H5Oopen(qfqmLvlDp0pId,"soni_001")
  #h5writeAttribute(attributes(dataList$soni)$unit, h5obj = dataGrpId, name = "unit")
  #h5writeAttribute(attributes(dataList$soni)$names, h5obj = dataGrpId, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = dataGrpId, name = attrDpNameList[x,"fieldName"]))
  dataHorVerGrpId <- rhdf5::H5Gcreate(dataGrpId,LevlTowr)
  qfqmHorVerGrpId <- rhdf5::H5Gcreate(qfqmGrpId,LevlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = dataHorVerGrpId, name = attrDataList[x,"fieldName"]))
  #H5Gclose(dataGrpId)
  
  #Writing Data for SoniAmrs############################################################
  dataGrpId <- rhdf5::H5Oopen(dataLvlDp0pId,"soniAmrs_001")
  qfqmGrpId <- rhdf5::H5Oopen(qfqmLvlDp0pId,"soniAmrs_001")
  #h5writeAttribute(attributes(dataList$soniAmrs)$unit, h5obj = dataGrpId, name = "unit")
  #h5writeAttribute(attributes(dataList$soniAmrs)$names, h5obj = dataGrpId, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = dataGrpId, name = attrDpNameList[x,"fieldName"]))
  dataHorVerGrpId <- rhdf5::H5Gcreate(dataGrpId,LevlTowr)
  qfqmHorVerGrpId <- rhdf5::H5Gcreate(qfqmGrpId,LevlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = dataHorVerGrpId, name = attrDataList[x,"fieldName"]))
  
  #Writing IRGA data####################################################################
  dataGrpId <- rhdf5::H5Oopen(dataLvlDp0pId,"irga_001") #Open H5 connection
  qfqmGrpId <- rhdf5::H5Oopen(qfqmLvlDp0pId,"irga_001")
  #h5writeAttribute(attributes(dataList$irga)$unit, h5obj = dataGrpId, name = "unit")
  #h5writeAttribute(attributes(dataList$irga)$names, h5obj = dataGrpId, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = dataGrpId, name = attrDpNameList[x,"fieldName"]))
  dataHorVerGrpId <- rhdf5::H5Gcreate(dataGrpId,LevlTowr)
  qfqmHorVerGrpId <- rhdf5::H5Gcreate(qfqmGrpId,LevlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = dataHorVerGrpId, name = attrDataList[x,"fieldName"]))
  
  #Used to write the datasets into the groups with attributes attached.
  dataGrpId <- rhdf5::H5Oopen(dataLvlDp0pId,"irgaMfcSamp_001")
  qfqmGrpId <- rhdf5::H5Oopen(qfqmLvlDp0pId,"irgaMfcSamp_001")
  #h5writeDataset.data.frame(obj = qfIrgaMfcSampOut, h5loc = qfqmGrpId, name = LevlTowr, DataFrameAsCompound = FALSE)
  #h5writeAttribute(attributes(dataList$irgaMfcSamp)$unit, h5obj = dataGrpId, name = "unit")
  #h5writeAttribute(attributes(dataList$irgaMfcSamp)$names, h5obj = dataGrpId, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = dataGrpId, name = attrDpNameList[x,"fieldName"]))
  dataHorVerGrpId <- rhdf5::H5Gcreate(dataGrpId,LevlTowr)
  qfqmHorVerGrpId <- rhdf5::H5Gcreate(qfqmGrpId,LevlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = dataHorVerGrpId, name = attrDataList[x,"fieldName"]))
  
  
  #Close all the connections to the file before exiting
  rhdf5::H5Gclose(dataGrpId)
  rhdf5::H5Gclose(siteGrpId)
  rhdf5::H5Fclose(fileGrpId)
  rhdf5::H5close()
}