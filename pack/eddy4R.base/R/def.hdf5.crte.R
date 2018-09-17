##############################################################################################
#' @title Definition function: Create the ECTE HDF5 file structure

#' @author 
#' David Durden \email{ddurden@battelleecology.org} \cr
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Definition function. Function creates the standard NEON HDF5 file structure for the ECTE data products.
#' @param Date is the date for the output file being generated.
#' @param Site is the site for which the output file is being generated.
#' @param LvlTowr is the measurement level of the tower top to determine the VER number of the NEON DP naming convention.
#' @param DirOut is the output directory where the file being generated is stored.
#' @param FileOutBase character string indicating the base output filename, to which the date and package information will be appended.
#' @param MethExpd logical indicating if the output should be expanded or basic.
#' @param MethDp04 logical indicating if ECTE dp04 HDF5 folder structure should be included.
#' @param FileNameReadMe character indicating the filename incl. absolute path to the ReadMe file for inclusion in the output HDF5 file. Defaults to \code{NULL}, which downloads the readme file from a web location.
#' @param FileNameObjDesc character indicating the filename incl. absolute path to the object description file for inclusion in the output HDF5 file. Defaults to \code{NULL}, which downloads the object description file from a web location.
#' @param LvlGasRefe character string indicating the NEON measurement horizontal and vertical location (HORVER) number as described in the NEON DP naming convention.

#' @return A NEON formatted HDF5 file that is output to /code{DirOut} with a readme and object description included.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000807)

#' @keywords NEON, HDF5, eddy-covariance, ECTE

#' @examples 

#'#Example run to create L0 gold files for 4/24/2016 at SERC
#'#Setting Site
#'Site <- "SERC"
#'LvlTowr <- "000_060"
#'FileOutBase <- "NEON.D02.SERC.DP4.00200.001.ec-flux"
#'MethExpd <- TRUE

#'#Setting Date to be processed
#'Date <- "2016-04-24"

#'#Set directory for example output to working directory
#'DirOut <- getwd()

#'#Running example
#'def.hdf5.crte(Date = Date, Site = Site, LvlTowr = LvlTowr, DirOut = DirOut, FileOutBase = FileOutBase, MethExpd = MethExpd)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2016-12-22)
#     original creation
#   Dave Durden (2017-05-30)
#     Added readme and object description to generated files
#   Dave Durden (2017-06-04)
#     Formatting output name to align with NEON DPS
#   Dave Durden (2017-06-05)
#     Adding uncertainty level output
#   Stefan Metzger (2017-07-01)
#     added switch for readme and object description file
#   Dave Durden (2017-09-07)
#     Adding dp04 method switch
#   Dave Durden (2017-09-20)
#     Changing function to work with new workflow parameters
#   Dave Durden (2017-12-13)
#     Updating naming conventions for ECTE and removing dp0p rev numbers
#   Natchaya P-Durden (2018-03-30)
#     applied term name convention; replace Levl by Lvl
#   Dave Durden (2018-08-05)
#     Adding irga validation system sensors for ECTE
##############################################################################################################
#Start of function call to generate NEON HDF5 files
##############################################################################################################

def.hdf5.crte <- function(
  Date, 
  Site = "SERC", 
  LvlTowr, 
  DirOut,
  FileOutBase = NULL,
  MethExpd = TRUE,
  MethDp04 = FALSE,
  FileNameReadMe = NULL,
  FileNameObjDesc = NULL,
  LvlGasRefe = c("702_000", "704_000","705_000", "706_000")
  ) {
  
  #Determine basic vs. expanded
  base::ifelse(MethExpd == TRUE, MethOut <- "expanded", MethOut <- "basic")
  
  
  #Check to see if the directory exists, if not create the directory. Recursive required to write nested file directories
  if (base::dir.exists(DirOut) == FALSE) base::dir.create(DirOut, recursive = TRUE)

  # download readme and object description file only if not previously provided
  if(base::is.null(FileNameReadMe) && base::is.null(FileNameObjDesc)) {

    DirTmp <- tempdir()
    #Download file description readme and object list  
    eddy4R.base::def.dld.zip(Inp = list(Url = "https://www.dropbox.com/s/dqq3j7epiy98y29/fileDesc.zip?dl=1",
                                        Dir = DirTmp))
    
    #Store the path to the readme file
    FileNameReadMe <- base::list.files( path = base::paste0(DirTmp,"/fileDesc"), pattern = ".txt", full.names = TRUE)
    #Store the path to the object description file
    FileNameObjDesc <- base::list.files( path = base::paste0(DirTmp,"/fileDesc/"), pattern = ".csv", full.names = TRUE)

  }
  
  #Read in the readme file
  readMe <- base::readChar(FileNameReadMe, base::file.info(FileNameReadMe)$size)
  #Read in the object description file
  objDesc <- utils::read.csv(FileNameObjDesc,header = TRUE, stringsAsFactors = FALSE)
  
  
  #Create a connection to the workbook
  #wk <- loadWorkbook("/home/ddurden/eddy/data/Thresholds_EC/NEON_HDF5_metadata.xlsx") 
  
  #Read the workbook into a data frame
  #metaList <- readWorksheet(wk, sheet="TIS", check.names = F) 
  
  
  #attrSiteList <- metaList[which(metaList$`HDF5 Site Group` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  #attrDpNameList <- metaList[which(metaList$`HDF5 DP group` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  #attrDataList <- metaList[which(metaList$`Data table specific` == "X" & metaList$`Eddy4R-processing-level` == "L1"), c("fieldName","Field Description")]
  
  
  #Output filename - the data product number is the umbrella EC data product number
  FileOut <- base::paste0(DirOut,"/",FileOutBase,".", Date,".", MethOut,".h5")
  #Create the file, create a class
  #Create the file, create a class
  idFile <- rhdf5::H5Fcreate(FileOut)
  #If the file is already created use:
  #idFile <- H5Fopen("HDF5TIS_L0_prototype.h5")
  
  # Write the readme as a data table to the HDF5 file
  rhdf5::h5writeDataset.character(obj = readMe, h5loc = idFile, name = "readMe")
  
  # Write the object description as a data table in  the HDF5 file
  rhdf5::h5writeDataset.data.frame(obj = objDesc, h5loc = idFile, name = "objDesc")
  #Create a group level for SERC
  idSite <- rhdf5::H5Gcreate(idFile, Site) 
  #If the group is already created use:
  #idSite <- H5Gopen(idFile,"SERC")
  idDp0p <- rhdf5::H5Gcreate(idSite,"dp0p")
  idDp01 <- rhdf5::H5Gcreate(idSite,"dp01")

  #Adding structure under dp0p
  idDataLvlDp0p <- rhdf5::H5Gcreate(idDp0p,"data")
  idQfqmLvlDp0p <- rhdf5::H5Gcreate(idDp0p,"qfqm")
  
  #Adding structure under dp01
  idDataLvlDp01 <- rhdf5::H5Gcreate(idDp01,"data")
  idQfqmLvlDp01 <- rhdf5::H5Gcreate(idDp01,"qfqm")
  idUcrtLvlDp01 <- rhdf5::H5Gcreate(idDp01,"ucrt")

  

  #Create a function to create group levels for each L0 DP
  #grpCrte <- function(x) H5Gcreate(idSite, x)
  #grpCrte <- function(x) H5Gcreate(dplid, x)
  
  #Apply the function to produce the group levels in the data file
  #lapply(grpList, grpCrte)
  ########################################################################### 
  #Creating level 0' file structures########################################
  #Create a list of all the L0 DPs for creation of group hierarchy (fard)
  grpListDp0p <- c("irgaTurb","soni","amrs","mfcSampTurb","valvValiNemaTurb","mfcValiTurb","valvLeakHeatTurb","gasRefe")
  #              ,"irgaGasCyl"
  #              "presTrap","presValiLineTurb","presValiRegInTurb",
  #              "presValiRegOutTurb","pumpTurb","valvLeakHeatTurb",
  #              "valvValiHutTurb","valvValiNemaTurb",)
  #The DP level, the data product ID and the Rev number
  #grpListDp0p <- base::paste(grpListDp0p, "_001", sep = "")
  
  #Creating level 0p file structures
  lapply(grpListDp0p, function(x) rhdf5::H5Gcreate(idDataLvlDp0p, x))
  lapply(grpListDp0p[grep(pattern = "valv", x = grpListDp0p, invert = T)], function(x) rhdf5::H5Gcreate(idQfqmLvlDp0p, x))

  ###########################################################################     
  #Creating level 1 file structures########################################
  #DPs to be used to create levels
  grpListDp01 <- c("amrs", "co2Turb", "h2oTurb", "soni")
  #Create dp01 data product levels in data
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idDataLvlDp01, x))
  #Create dp01 data product levels in qfqm
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idQfqmLvlDp01, x))
  #Create dp01 data product levels in ucrt
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idUcrtLvlDp01, x))
  #Create HOR_VER_TMI level for each DP under data
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idDataLvlDp01, paste0(x,"/",LvlTowr,"_30m")))
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idDataLvlDp01, paste0(x,"/",LvlTowr,"_01m")))
  #Create HOR_VER_TMI level for each DP under qfqm
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idQfqmLvlDp01, paste0(x,"/",LvlTowr,"_30m")))
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idQfqmLvlDp01, paste0(x,"/",LvlTowr,"_01m")))
  #Create HOR_VER_TMI level for each DP under ucrt
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idUcrtLvlDp01, paste0(x,"/",LvlTowr,"_30m")))
  lapply(grpListDp01, function(x) rhdf5::H5Gcreate(idUcrtLvlDp01, paste0(x,"/",LvlTowr,"_01m")))
  
  ###########################################################################     
  #Creating level 4 file structures########################################
  
  if(MethDp04 == TRUE){
    #Create dp04 level
    idDp04 <- rhdf5::H5Gcreate(idSite,"dp04")
    #Create structure under dp04 level
    idDataLvlDp04 <- rhdf5::H5Gcreate(idDp04,"data")
    idQfqmLvlDp04 <- rhdf5::H5Gcreate(idDp04,"qfqm")
    idUcrtLvlDp04 <- rhdf5::H5Gcreate(idDp04,"ucrt")  
    
    #DPs to be used to create levels
    grpListDp04 <- c("fluxCo2", "fluxH2o", "fluxMome", "fluxTemp", "foot")
    #Create dp01 data product levels in data
    lapply(grpListDp04, function(x) rhdf5::H5Gcreate(idDataLvlDp04, x))
    #Create dp01 data product levels in qfqm
    lapply(grpListDp04, function(x) rhdf5::H5Gcreate(idQfqmLvlDp04, x))
    #Create dp01 data product levels in ucrt
    lapply(grpListDp04, function(x) rhdf5::H5Gcreate(idUcrtLvlDp04, x))
  }
  
 # idDataLvlDp01HorVer <- H5Gopen(idDataLvlDp01, paste0("co2Turb/",LvlTowr,"_30m"))
  #sid <- H5Screate_simple(c(0,0,0))
  #idDp01DataTbl <- H5Dcreate(idDataLvlDp01HorVer, "rtioMoleDryCo2","H5T_NATIVE_DOUBLE", sid)
 
 # idDataLvlDp01HorVer <- H5Gopen(idDataLvlDp01, paste0("h2oTurb/",LvlTowr,"_30m"))
 # sid <- H5Screate_simple(c(0,0,0))
  #idDp01DataTbl <- H5Dcreate(idDataLvlDp01HorVer, "rtioMoleDryH2o","H5T_NATIVE_DOUBLE", sid) #Used for metadata attribution, but it causes problem to write to later.
  #lapply(seq_len(nrow(attrSiteList)), function(x) h5writeAttribute(attrSiteList[x,"Field Description"], h5obj = idSite, name = attrSiteList[x,"fieldName"]))
  
  #Used to write the datasets into the groups with attributes attached.
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"soni")
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"soni")
  #h5writeAttribute(attributes(dataList$soni)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$soni)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LvlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LvlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = idDataHorVer, name = attrDataList[x,"fieldName"]))
  #H5Gclose(idData)
  
  #Writing Data for SoniAmrs############################################################
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"amrs")
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"amrs")
  #h5writeAttribute(attributes(dataList$amrs)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$amrs)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LvlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LvlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = idDataHorVer, name = attrDataList[x,"fieldName"]))
  
  #Writing IRGA data####################################################################
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"irgaTurb") #Open H5 connection
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"irgaTurb")
  #h5writeAttribute(attributes(dataList$irgaTurb)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$irgaTurb)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LvlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LvlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = idDataHorVer, name = attrDataList[x,"fieldName"]))
  
  #Used to write the datasets into the groups with attributes attached.
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"mfcSampTurb")
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"mfcSampTurb")
  #h5writeDataset.data.frame(obj = qfIrgaMfcSampOut, h5loc = idQfqm, name = LvlTowr, DataFrameAsCompound = FALSE)
  #h5writeAttribute(attributes(dataList$mfcSampTurb)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$mfcSampTurb)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LvlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LvlTowr)
  #lapply(seq_len(nrow(attrDataList)), function(x) h5writeAttribute(attrDataList[x,"Field Description"], h5obj = idDataHorVer, name = attrDataList[x,"fieldName"]))
  
  #Creating structure for mfcValiTurb
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"mfcValiTurb")
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"mfcValiTurb")
  #h5writeDataset.data.frame(obj = qfIrgaMfcSampOut, h5loc = idQfqm, name = LvlTowr, DataFrameAsCompound = FALSE)
  #h5writeAttribute(attributes(dataList$mfcSampTurb)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$mfcSampTurb)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LvlTowr)
  idQfqmHorVer <- rhdf5::H5Gcreate(idQfqm,LvlTowr)
  
  # Creating structure for valvValiNemaTurb
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"valvValiNemaTurb") #Open H5 connection
  #h5writeAttribute(attributes(dataList$irgaTurb)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$irgaTurb)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))

  idDataHorVer <- rhdf5::H5Gcreate(idData,LvlTowr)

  # Creating structure for valvLeakHeatTurb 
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"valvLeakHeatTurb") #Open H5 connection
  #h5writeAttribute(attributes(dataList$irgaTurb)$unit, h5obj = idData, name = "unit")
  #h5writeAttribute(attributes(dataList$irgaTurb)$names, h5obj = idData, name = "names")
  #lapply(seq_along(nrow(attrDpNameList)), function(x) h5writeAttribute(attrDpNameList[x,"Field Description"], h5obj = idData, name = attrDpNameList[x,"fieldName"]))
  idDataHorVer <- rhdf5::H5Gcreate(idData,LvlTowr)

  
  # Creating structure for gasRefe
  idData <- rhdf5::H5Oopen(idDataLvlDp0p,"gasRefe") #Open H5 connection
  idQfqm <- rhdf5::H5Oopen(idQfqmLvlDp0p,"gasRefe") 
  #Creating gasRefe HORVER level structures
  lapply(LvlGasRefe, function(x) rhdf5::H5Gcreate(idData, x))
  lapply(LvlGasRefe, function(x) rhdf5::H5Gcreate(idQfqm, x))
  
  #Close all the connections to the file before exiting
  rhdf5::H5Gclose(idData)
  rhdf5::H5Gclose(idSite)
  rhdf5::H5Fclose(idFile)
  rhdf5::H5close()
}
