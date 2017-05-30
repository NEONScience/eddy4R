##############################################################################################
#' @title Definition function: Create the ECTE HDF5 file structure

#' @author 
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. Function creates the standard NEON HDF5 file structure for the ECTE data products.
#' @param \code{Date} is the date for the output file being generated.
#' @param \code{Site} is the site for which the output file is being generated.
#' @param \code{LevlTowr} is the measurement level of the tower top to determine the VER number of the NEON DP naming convention.
#' @param \code{DirOut} is the output directory where the file being generated is stored.
#' @param \code{LevlDp} is output file DP level for the file naming.

#' @return A NEON formatted HDF5 file that is output to /code{DirOut} 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000807)

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

  
  #Download file description readme and object list  
  eddy4R.base::def.dld.zip(Inp = list(Url = "https://www.dropbox.com/s/dqq3j7epiy98y29/fileDesc.zip?dl=1",
                                      Dir = DirOut))
  
  #Store the path to the readme file
  fileNameReadMe <- list.files( path = paste0(DirOut,"/fileDesc"), pattern = ".txt", full.names = TRUE)
  #Store the path to the object description file
  fileNameObjDesc <- list.files( path = paste0(DirOut,"/fileDesc/"), pattern = ".csv", full.names = TRUE)
  #Read in the readme file
  readMe <- readChar(fileNameReadMe, file.info(fileNameReadMe)$size)
  #Read in the object description file
  objDesc <- read.csv(fileNameObjDesc,header = TRUE, stringsAsFactors = FALSE)
  
  
  #Create a connection to the workbook
  #wk <- loadWorkbook("/home/ddurden/eddy/data/Thresholds_EC/NEON_HDF5_metadata.xlsx") 
  
  #Read the workbook into a data frame
  #metaList <- readWorksheet(wk, sheet="TIS", check.names = F) 
  
  
  #attrSiteList <- metaList[which(metaList$`HDF5 Site Group` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  #attrDpNameList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  #attrDataList <- metaList[which(metaList$`Data table specific` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  
  #Create a list of all the L0 DPs for creation of group hierarchy (fard)
  grpList <- c("irga","soni","soniAmrs","irgaMfcSamp","irgaSndValiNema")
  #              ,"irgaGasCyl","irgaMfcVali",
  #              "irgaPresTrap","irgaPresValiLine","irgaPresValiRegIn",
  #              "irgaPresValiRegOut","irgaPump","irgaSndLeakHeat",
  #              "irgaSndValiHut","irgaSndValiNema",)
  #The DP level, the data product ID and the Rev number
  grpList <- paste(grpList, "_001", sep = "")
  
  #Output filename
  fileOut <- paste0(DirOut,"/","ECTE_",LevlDp,"_", Site, "_", Date, "_new_format.h5")
  #Create the file, create a class
  #Create the file, create a class
  idFile <- rhdf5::H5Fcreate(fileOut)
  #If the file is already created use:
  #idFile <- H5Fopen("HDF5TIS_L0_prototype.h5")
  
  # Write the readme as a data table to the HDF5 file
  rhdf5::h5write.default(obj = readMe, file = fileOut, name = "readMe")
  
  # Write the object description as a data table in  the HDF5 file
  rhdf5::h5writeDataset.data.frame(obj = objDesc, h5loc = idFile, name = "objDesc")
  #Create a group level for SERC
  idSite <- rhdf5::H5Gcreate(idFile, Site) 
  #If the group is already created use:
  #idSite <- H5Gopen(idFile,"SERC")
  idDp0p <- rhdf5::H5Gcreate(idSite,"dp0p")
  idDp01 <- rhdf5::H5Gcreate(idSite,"dp01")
  
  idDataLvlDp0p <- rhdf5::H5Gcreate(idDp0p,"data")
  idDataLvlDp01 <- rhdf5::H5Gcreate(idDp01,"data")
  
  idQfqmLvlDp0p <- rhdf5::H5Gcreate(idDp0p,"qfqm")
  idQfqmLvlDp01 <- rhdf5::H5Gcreate(idDp01,"qfqm")
  idUcrtLvlDp01 <- rhdf5::H5Gcreate(idDp01,"ucrt")
  
  #Create a function to create group levels for each L0 DP
  #grpCrte <- function(x) H5Gcreate(idSite, x)
  #grpCrte <- function(x) H5Gcreate(dplid, x)
  
  #Apply the function to produce the group levels in the data file
  #lapply(grpList, grpCrte)
  
  #Creating level 0p file structures
  lapply(grpList, function(x) rhdf5::H5Gcreate(idDataLvlDp0p, x))
  lapply(grpList, function(x) rhdf5::H5Gcreate(idQfqmLvlDp0p, x))
  
  #Creating level 1 file structures
  
  grpListDp01 <- c("soniAmrs", "irgaCo2", "irgaH2o", "soni")
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idDataLvlDp01, x))
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idQfqmLvlDp01, x))
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idDataLvlDp01, paste0(x,"/",LevlTowr,"_30m")))
  lapply(grpListDp01, function(x) {
    print(x)
    if(x == "soni"){rhdf5::H5Gcreate(idDataLvlDp01, paste0(x,"/",LevlTowr,"_02m"))} else {rhdf5::H5Gcreate(idDataLvlDp01, paste0(x,"/",LevlTowr,"_01m"))}})
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idQfqmLvlDp01, paste0(x,"/",LevlTowr,"_30m")))
  lapply(grpListDp01, function(x) {
    if(x == "soni") {rhdf5::H5Gcreate(idQfqmLvlDp01, paste0(x,"/",LevlTowr,"_02m"))} else {rhdf5::H5Gcreate(idQfqmLvlDp01, paste0(x,"/",LevlTowr,"_01m"))}})
  
 # idDataLvlDp01HorVer <- H5Gopen(idDataLvlDp01, paste0("irgaCo2/",LevlTowr,"_30m"))
  #sid <- H5Screate_simple(c(0,0,0))
  #idDp01DataTbl <- H5Dcreate(idDataLvlDp01HorVer, "rtioMoleDryCo2","H5T_NATIVE_DOUBLE", sid)
 
 # idDataLvlDp01HorVer <- H5Gopen(idDataLvlDp01, paste0("irgaH2o/",LevlTowr,"_30m"))
 # sid <- H5Screate_simple(c(0,0,0))
  #idDp01DataTbl <- H5Dcreate(idDataLvlDp01HorVer, "rtioMoleDryH2o","H5T_NATIVE_DOUBLE", sid) #Used for metadata attribution, but it causes problem to write to later.
  #lapply(seq_len(nrow(attrSiteList)), function(x) h5writeAttribute(attrSiteList[x,"Field Description"], h5obj = idSite, name = attrSiteList[x,"fieldName"]))
  
  #Used to write the datasets into the groups with attributes attached.
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"soni_001")
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"soni_001")
  #h5writeAttribute(attributes(dataList$soni)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$soni)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LevlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LevlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = idDataHorVer, name = attrDataList[x,"fieldName"]))
  #H5Gclose(idData)
  
  #Writing Data for SoniAmrs############################################################
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"soniAmrs_001")
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"soniAmrs_001")
  #h5writeAttribute(attributes(dataList$soniAmrs)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$soniAmrs)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LevlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LevlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = idDataHorVer, name = attrDataList[x,"fieldName"]))
  
  #Writing IRGA data####################################################################
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"irga_001") #Open H5 connection
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"irga_001")
  #h5writeAttribute(attributes(dataList$irga)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$irga)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LevlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LevlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = idDataHorVer, name = attrDataList[x,"fieldName"]))
  
  #Used to write the datasets into the groups with attributes attached.
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"irgaMfcSamp_001")
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"irgaMfcSamp_001")
  #h5writeDataset.data.frame(obj = qfIrgaMfcSampOut, h5loc = idQfqm, name = LevlTowr, DataFrameAsCompound = FALSE)
  #h5writeAttribute(attributes(dataList$irgaMfcSamp)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$irgaMfcSamp)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LevlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LevlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = idDataHorVer, name = attrDataList[x,"fieldName"]))
  
  
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"irgaSndValiNema_001") #Open H5 connection
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"irgaSndValiNema_001")
  #h5writeAttribute(attributes(dataList$irga)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$irga)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LevlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LevlTowr)
  
  #Close all the connections to the file before exiting
  rhdf5::H5Gclose(idData)
  rhdf5::H5Gclose(idSite)
  rhdf5::H5Fclose(idFile)
  rhdf5::H5close()
}