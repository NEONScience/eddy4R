##############################################################################################
#' @title Definition function: Write NEON Level 1 data, qfqm, and uncertainty to output HDF5

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. To write NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points), quality flags and quality metrics, and uncertainty quantification to an output HDF5 file. 

#' @param inpList A list of including dp01 data, quality flags and quality metrics, and uncertainty calculations to package and write to an output HDF5 file.
#' @param FileOut Character: The file name for the output HDF5 file
#' @param SiteLoca Character: Site location.
#' @param LvlTowr Character: The tower level that the sensor data is being collected in NEON data product convention (HOR_VER).
#' @param MethUcrt Logical: Determines if uncertainty information is available for output.
#' @param MethSubAgr Logical: Determines if 1-minute data is available for output.
#'
#' @return An HDF5 file with dp01 data, qfqm, and uncertainty written
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords output, HDF5

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2017-06-04)
#     original creation
#   David Durden (2017-06-04)
#     Adding ucrt to dp01 output HDF5
#   David Durden (2017-06-29)
#     Adding switches for writing output
#   David Durden (2017-12-18)
#     bug fix to write unit attribute to 30 min basic output for qfqm's
#   Natchaya P-Durden (2018-03-30)
#     applied term name convention; replace LevlTowr by LvlTowr
#   Natchaya P-Durden (2018-04-12)
#    applied eddy4R term name convention; replaced fid by idFile
#   Natchaya P-Durden (2019-01-08)
#    adding logic to do not to report rtioMoleDryCo2Cor and 
#    rtioMoleDryCo2Raw data and ucrt in the basic file
#   Natchaya P-Durden (2019-01-23)
#    adding linear regression coefficients and its se to the attribute of rtioMoleDryCo2Vali
#   Natchaya P-Durden (2019-02-21)
#    adding results from MLF (scale) to the attribute of rtioMoleDryCo2Vali
#   Natchaya P-Durden (2019-01-08)
#    removed rtioMoleDryH2oCor and rtioMoleDryH2oRaw data and ucrt from the basic file
#   Natchaya P-Durden (2020-02-23)
#    removed unnecessary qfqm unit attributes when writing the basic file
#   Chris Florian (2021-08-09)
#    adding qfValiThsh to rtioMoleDryCo2Vali attributes
##############################################################################################


# start function def.hdf5.wrte.dp01()

def.hdf5.wrte.dp01 <- function(
  inpList,
  FileOut,
  SiteLoca,
  LvlTowr,
  Dp01,
  MethUcrt = TRUE,
  MethSubAgr = TRUE
){
  
#Determine if the output file should be expanded or basic by creating a logical determined from the filename
MethExpd <- grepl(pattern = "expanded", x = FileOut)

#remove rtioMoleDryCo2Cor and rtioMoleDryCo2Raw data from inpList when writing out the basic file
if(MethExpd == FALSE){
  inpList$data$co2Turb$rtioMoleDryCo2Cor <- NULL
  inpList$data$co2Turb$rtioMoleDryCo2Raw <- NULL
  inpList$data$h2oTurb$rtioMoleDryH2oCor <- NULL
  inpList$data$h2oTurb$rtioMoleDryH2oRaw <- NULL
}

#Create HDF5 connection to the output file  
idFile <- rhdf5::H5Fopen(FileOut)

# Was used to open connection to the group levels for data and qfqm for 1-min, 2-min (soni) and 30-min output
# if (Dp01 == "soni")
# {gid01 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/data/",Dp01,"/",LvlTowr,"_02m"))
# gid30 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/data/",Dp01,"/",LvlTowr,"_30m"))
# qfid01 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/qfqm/",Dp01,"/",LvlTowr,"_02m"))
# qfid30 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/qfqm/",Dp01,"/",LvlTowr,"_30m"))
# } else {

# Open connection to the group levels for data and qfqm for 1-min and 30-min output
# data group level connections
idData01 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/data/",Dp01,"/",LvlTowr,"_01m"))
idData30 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/data/",Dp01,"/",LvlTowr,"_30m"))
# qfqm group level connections
idQfqm01 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/qfqm/",Dp01,"/",LvlTowr,"_01m"))
idQfqm30 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/qfqm/",Dp01,"/",LvlTowr,"_30m"))
# ucrt group level connections
idUcrt01 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/ucrt/",Dp01,"/",LvlTowr,"_01m"))
idUcrt30 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp01/ucrt/",Dp01,"/",LvlTowr,"_30m"))
#}

##########################################################################################
#Data
##########################################################################################
#Writing 30-min data to output HDF5 file
lapply(names(inpList$data[[Dp01]]), function(x) rhdf5::h5writeDataset.data.frame(obj = inpList$data[[Dp01]][[x]], h5loc = idData30, name = x, DataFrameAsCompound = TRUE))

#Writing 30-min data unit attributes to output HDF5 file
lapply(names(inpList$data[[Dp01]]), function(x) {
  if (!is.null(attributes(inpList$data[[Dp01]][[x]])$unit) == TRUE){
    dgid <- rhdf5::H5Dopen(idData30, x)
    rhdf5::h5writeAttribute(attributes(inpList$data[[Dp01]][[x]])$unit, h5obj = dgid, name = "unit")
  }})

if(MethSubAgr == TRUE){
  #remove rtioMoleDryCo2Cor and rtioMoleDryCo2Raw data from inpList when writing out the basic file
  if(MethExpd == FALSE){
    inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Cor <- NULL
    inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Raw <- NULL
    inpList$dp01AgrSub$data$h2oTurb$rtioMoleDryH2oCor <- NULL
    inpList$dp01AgrSub$data$h2oTurb$rtioMoleDryH2oRaw <- NULL
  }
  #Writing sub-aggregated (e.g.1-min) data to output HDF5 file
  lapply(names(inpList$dp01AgrSub$data[[Dp01]]), function(x) rhdf5::h5writeDataset.data.frame(obj = inpList$dp01AgrSub$data[[Dp01]][[x]], h5loc = idData01, name = x, DataFrameAsCompound = TRUE))
  
  #Writing sub-aggregated (e.g.1-min) data unit attributes to output HDF5 file
  lapply(names(inpList$dp01AgrSub$data[[Dp01]]), function(x) {
    if (!is.null(attributes(inpList$dp01AgrSub$data[[Dp01]][[x]])$unit) == TRUE){
      dgid <- rhdf5::H5Dopen(idData01, x)
      rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$data[[Dp01]][[x]])$unit, h5obj = dgid, name = "unit")
    }})
  #Writing linear regression coefficients and its se to attribute of rtioMoleDryCo2Vali
  if (Dp01 == "co2Turb"){
  if (!is.null(attributes(inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali)$coef) == TRUE){
    dgid <- rhdf5::H5Dopen(idData01, "rtioMoleDryCo2Vali")
    rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali)$coef, h5obj = dgid, name = "valiCoef")
    rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali)$coefSe, h5obj = dgid, name = "valiCoefSe")
    rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali)$scal, h5obj = dgid, name = "valiScal")
    rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali)$qfEvalThsh, h5obj = dgid, name = "qfEvalThsh")
    rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali)$evalCoef, h5obj = dgid, name = "evalCoef")
    rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali)$evalCoefSe, h5obj = dgid, name = "evalCoefSe")
    rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali)$evalSlpThsh, h5obj = dgid, name = "evalSlpThsh")
    rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali)$evalOfstThsh, h5obj = dgid, name = "evalOfstThsh")
  }}
}
##########################################################################################
#QFQM
##########################################################################################
#If statement to determine if output should be expanded or basic
if(MethExpd == FALSE){
#Writing 30-min qfqm to output HDF5 file
lapply(names(inpList$qfqm[[Dp01]]), function(x)  {
  #convert to integer
  inpList$qfqm[[Dp01]][[x]]$qfFinl <<- as.integer(inpList$qfqm[[Dp01]][[x]]$qfFinl)
  #convert to integer
  inpList$qfqm[[Dp01]][[x]]$qfSciRevw <<- as.integer(inpList$qfqm[[Dp01]][[x]]$qfSciRevw)
  #Write 30-min qfqm output to HDF5
  rhdf5::h5writeDataset.data.frame(obj = inpList$qfqm[[Dp01]][[x]][,c("qfFinl","timeBgn","timeEnd")], h5loc = idQfqm30, name = x, DataFrameAsCompound = TRUE)})

  if(MethSubAgr == TRUE){
#Writing sub-aggregated (e.g.1-min) qfqm to output HDF5 file
lapply(names(inpList$dp01AgrSub$qfqm[[Dp01]]), function(x)  {
  #convert to integer
  inpList$dp01AgrSub$qfqm[[Dp01]][[x]]$qfFinl <<- as.integer(inpList$dp01AgrSub$qfqm[[Dp01]][[x]]$qfFinl) 
  #convert to integer
  inpList$dp01AgrSub$qfqm[[Dp01]][[x]]$qfSciRevw <<- as.integer(inpList$dp01AgrSub$qfqm[[Dp01]][[x]]$qfSciRevw) 
  #Write 1-min output to HDF5
  rhdf5::h5writeDataset.data.frame(obj = inpList$dp01AgrSub$qfqm[[Dp01]][[x]][,c("qfFinl","timeBgn","timeEnd")], h5loc = idQfqm01, name = x, DataFrameAsCompound = TRUE)})
  }
    
} else {
  
  lapply(names(inpList$qfqm[[Dp01]]), function(x)  {
    #convert to integer
    inpList$qfqm[[Dp01]][[x]]$qfFinl <<- as.integer(inpList$qfqm[[Dp01]][[x]]$qfFinl) 
    #convert to integer
    inpList$qfqm[[Dp01]][[x]]$qfSciRevw <<- as.integer(inpList$qfqm[[Dp01]][[x]]$qfSciRevw)
    #Write 30-min qfqm output to HDF5
    rhdf5::h5writeDataset.data.frame(obj = inpList$qfqm[[Dp01]][[x]], h5loc = idQfqm30, name = x, DataFrameAsCompound = TRUE)})

#Write 1-min qfqm
if(MethSubAgr == TRUE){  
  #Writing sub-aggregated (e.g.1-min) qfqm to output HDF5 file
  lapply(names(inpList$dp01AgrSub$qfqm[[Dp01]]), function(x)  {
    #convert to integer
    inpList$dp01AgrSub$qfqm[[Dp01]][[x]]$qfFinl <<- as.integer(inpList$dp01AgrSub$qfqm[[Dp01]][[x]]$qfFinl) 
    #convert to integer
    inpList$dp01AgrSub$qfqm[[Dp01]][[x]]$qfSciRevw <<- as.integer(inpList$dp01AgrSub$qfqm[[Dp01]][[x]]$qfSciRevw) 
    #Write 1-min output to HDF5
    rhdf5::h5writeDataset.data.frame(obj = inpList$dp01AgrSub$qfqm[[Dp01]][[x]], h5loc = idQfqm01, name = x, DataFrameAsCompound = TRUE)})
  }
}

#Writing 30-min qfqm unit attributes to output HDF5 file
lapply(names(inpList$qfqm[[Dp01]]), function(x) {
  if (!is.null(attributes(inpList$qfqm[[Dp01]][[x]])$unit) == TRUE){
    dgid <- rhdf5::H5Dopen(idQfqm30, x)
    if(MethExpd == FALSE){
      tmpAttr <- attributes(inpList$qfqm[[Dp01]][[x]])$unit[which(names(attributes(inpList$qfqm[[Dp01]][[x]])$unit) %in% c("qfFinl","timeBgn","timeEnd"))]    
      rhdf5::h5writeAttribute(tmpAttr, h5obj = dgid, name = "unit")
    }else{
    rhdf5::h5writeAttribute(attributes(inpList$qfqm[[Dp01]][[x]])$unit, h5obj = dgid, name = "unit")
    }
  }})

if(MethSubAgr == TRUE){
#Writing sub-aggregated (e.g.1-min) qfqm unit attributes to output HDF5 file
lapply(names(inpList$dp01AgrSub$qfqm[[Dp01]]), function(x) {
  if (!is.null(attributes(inpList$dp01AgrSub$qfqm[[Dp01]][[x]])$unit) == TRUE){
    dgid <- rhdf5::H5Dopen(idQfqm01, x)
    if(MethExpd == FALSE){
      tmpAttr <- attributes(inpList$dp01AgrSub$qfqm[[Dp01]][[x]])$unit[which(names(attributes(inpList$dp01AgrSub$qfqm[[Dp01]][[x]])$unit) %in% c("qfFinl","timeBgn","timeEnd"))]    
      rhdf5::h5writeAttribute(tmpAttr, h5obj = dgid, name = "unit")
    }else{
    rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$qfqm[[Dp01]][[x]])$unit, h5obj = dgid, name = "unit")
    }
  }})
}
##########################################################################################
#Ucrt
##########################################################################################
if(MethUcrt == TRUE){
  #remove rtioMoleDryCo2Cor and rtioMoleDryCo2Raw data from inpList when writing out the basic file
  if(MethExpd == FALSE){
    inpList$ucrt$co2Turb$rtioMoleDryCo2Cor <- NULL
    inpList$ucrt$co2Turb$rtioMoleDryCo2Raw <- NULL
    inpList$ucrt$h2oTurb$rtioMoleDryH2oCor <- NULL
    inpList$ucrt$h2oTurb$rtioMoleDryH2oRaw <- NULL
  }
  #Writing 30-min ucrt to output HDF5 file
  lapply(names(inpList$ucrt[[Dp01]]), function(x) rhdf5::h5writeDataset.data.frame(obj = inpList$ucrt[[Dp01]][[x]], h5loc = idUcrt30, name = x, DataFrameAsCompound = TRUE))
  
  #Writing 30-min ucrt unit attributes to output HDF5 file
  lapply(names(inpList$ucrt[[Dp01]]), function(x) {
    if (!is.null(attributes(inpList$ucrt[[Dp01]][[x]])$unit) == TRUE){
      dgid <- rhdf5::H5Dopen(idUcrt30, x)
      rhdf5::h5writeAttribute(attributes(inpList$ucrt[[Dp01]][[x]])$unit, h5obj = dgid, name = "unit")
    }})
  
if(MethSubAgr == TRUE){
  #remove rtioMoleDryCo2Cor and rtioMoleDryCo2Raw ucrt from inpList when writing out the basic file
  if(MethExpd == FALSE){
    inpList$dp01AgrSub$ucrt$co2Turb$rtioMoleDryCo2Cor <- NULL
    inpList$dp01AgrSub$ucrt$co2Turb$rtioMoleDryCo2Raw <- NULL
    inpList$dp01AgrSub$ucrt$h2oTurb$rtioMoleDryH2oCor <- NULL
    inpList$dp01AgrSub$ucrt$h2oTurb$rtioMoleDryH2oRaw <- NULL
  }
  #Writing sub-aggregated (e.g.1-min) ucrt to output HDF5 file
  lapply(names(inpList$dp01AgrSub$ucrt[[Dp01]]), function(x) rhdf5::h5writeDataset.data.frame(obj = inpList$dp01AgrSub$ucrt[[Dp01]][[x]], h5loc = idUcrt01, name = x, DataFrameAsCompound = TRUE))
  
  #Writing sub-aggregated (e.g.1-min) ucrt unit attributes to output HDF5 file
  lapply(names(inpList$dp01AgrSub$ucrt[[Dp01]]), function(x) {
    if (!is.null(attributes(inpList$dp01AgrSub$ucrt[[Dp01]][[x]])$unit) == TRUE){
      dgid <- rhdf5::H5Dopen(idUcrt01, x)
      rhdf5::h5writeAttribute(attributes(inpList$dp01AgrSub$ucrt[[Dp01]][[x]])$unit, h5obj = dgid, name = "unit")
    }})
}
}
#Close HDF5 connections
rhdf5::h5closeAll()
}
