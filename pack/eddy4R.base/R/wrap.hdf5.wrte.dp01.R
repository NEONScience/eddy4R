##############################################################################################
#' @title Wrapper function: Write NEON Level 1 data, qfqm, and uncertainty to output HDF5

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Wrapper function. To write NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points), quality flags and quality metrics, and uncertainty quantification to an output HDF5 file. 

#' @param inpList A list of including dp01 data, quality flags and quality metrics, and uncertainty calculations to package and write to an output HDF5 file
#' @param FileInp The file name for the input dp0p HDF5 file to grab metadata
#' @param FileOut The file name for the output HDF5 file
#' @param SiteLoca Character: Site location.
#' @param LvlTowr The tower level that the sensor data is being collected in NEON data product convention (HOR_VER)
#' @param MethUcrt Logical: Determines if uncertainty information is available for output.
#' @param MethDp04 logical indicating if ECTE dp04 HDF5 data should be included.
#' @param MethSubAgr Logical: Determines if 1-minute data is available for output.
#' @param Meta A list of parameters and metadata for updating output HDF5 metadata

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
#   David Durden (2017-06-06)
#     Adding uncertaitny HDF5 packaging calls
#   David Durden (2017-06-29)
#     Adding switches for writing 1-min and ucrt output
#   David Durden (2017-09-10)
#     Adding switches for writing dp04 output
#   David Durden (2018-01-10)
#     Altering the dp04 output to allow footprint output
#   David Durden (2017-02-12)
#     Adding unit conversion for output
#   Natchaya P-Durden (2018-03-30)
#     applied term name convention; replace LevlTowr by LvlTowr
#   Natchaya P-Durden (2018-04-12)
#    applied eddy4R term name convention; replaced fid by idFile
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced FileIn by FileInp
#   Natchaya P-Durden (2018-05-11)
#     rename function from def.hdf5.dp01.pack() to def.hdf5.pack.dp01()
#   Natchaya P-Durden (2018-05-22)
#     rename function from def.para.hdf5.dp01() to def.hdf5.copy.para()
#   David Durden (2018-12-09)
#     changing to use a the updated def.hdf5.pack() function
#   Natchaya P-Durden (2018-11-28)
#     adding irgaTurb validation data
#   Natchaya P-Durden (2019-01-29)
#     deleted dp04 of fluxCor and fluxRaw out when writing the basic file
#   David Durden (2019-05-30)
#     Adding dp04 low resolution output on top of validation code
#   Natchaya P-Durden (2019-09-30)
#     only write qfFinl out in basic file
#   Natchaya P-Durden (2020-01-17)
#     adding rtioMoleDryH2o during validation 
#   Natchaya P-Durden (2020-01-22)
#     adding timeBgn and timeEnd attributes
#   David Durden (2020-05-01)
#     adding Pfit coefficient output metadata
#   David Durden (2021-08-17)
#     Fixing attribute issues related to rhdf5 not being able to write lists
#   David Durden (2021-08-17)
#     Adding difference to UTC to output
##############################################################################################


# start function wrap.hdf5.wrte.dp01()

wrap.hdf5.wrte.dp01 <- function(
  inpList,
  FileInp,
  FileOut,
  SiteLoca,
  LvlTowr,
  MethUcrt = TRUE,
  MethDp04 = FALSE,
  MethSubAgr = TRUE,
  Meta
){

#Determine if the output file should be expanded or basic by creating a logical determined from the filename
MethExpd <- grepl(pattern = "expanded", x = FileOut)  
  


#Initializing output list
outList <- list()
rpt <- list()

#Packaging 30-min dp01 data output for writing to HDF5 file
outList$data <- sapply(names(inpList$data), function(x) eddy4R.base::def.hdf5.pack(inpList = inpList$data, time = inpList$time, Dp = x))

#Unit conversion for dp01 30 min data
outList$data <- eddy4R.base::wrap.unit.conv.out.ec(inpList = outList$data, MethType = "data") 

#Adding time to output dataframe
rpt <- lapply(rpt, cbind, timeBgn = time[[Dp01]]$timeBgn, timeEnd = time[[Dp01]]$timeEnd, stringsAsFactors = FALSE)

#Packaging 30-min dp01 qfqm output for writing to HDF5 file
outList$qfqm <- sapply(names(inpList$qfqm), function(x) eddy4R.base::def.hdf5.pack(inpList = inpList$qfqm, time = inpList$time, Dp = x))

#Applying units to each output in dp01 30 min qfqm
outList$qfqm <- eddy4R.base::wrap.unit.conv.out.ec(inpList = outList$qfqm, MethType = "qfqm")

if(MethUcrt == TRUE){
#Packaging 30-min dp01 ucrt output for writing to HDF5 file
outList$ucrt <- sapply(names(inpList$ucrt), function(x) eddy4R.base::def.hdf5.pack(inpList = inpList$ucrt, time = inpList$time, Dp = x))

#Unit conversion for dp01 30 min ucrt values
outList$ucrt <- eddy4R.base::wrap.unit.conv.out.ec(inpList = outList$ucrt, MethType = "ucrt") 
}

if(MethSubAgr == TRUE){
  #Packaging sub-aggregated (e.g.1-min) dp01 data for writing to HDF5 file
  outList$dp01AgrSub$data <- sapply(names(inpList$dp01AgrSub$data), function(x) eddy4R.base::def.hdf5.pack(inpList = inpList$dp01AgrSub$data, time = inpList$dp01AgrSub$time, Dp = x))

  #Unit conversion for dp01 sub-aggregated data
  outList$dp01AgrSub$data <- eddy4R.base::wrap.unit.conv.out.ec(inpList = outList$dp01AgrSub$data, MethType = "data") 
  
  #adding irgaTurb validation data
  outList$vali$data$co2Turb <- inpList$vali$data$co2Turb
  outList$vali$data$h2oTurb <- inpList$vali$data$h2oTurb
  
  #If values come in as Posix, they must first be converted to characters
  for (idxTime in c("timeBgn", "timeEnd")){
    if(!is.character(outList$vali$data$co2Turb$rtioMoleDryCo2Vali[[idxTime]])){
      outList$vali$data$co2Turb$rtioMoleDryCo2Vali[[idxTime]] <- strftime(outList$vali$data$co2Turb$rtioMoleDryCo2Vali[[idxTime]], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")} 
    if(!is.character(outList$vali$data$h2oTurb$rtioMoleDryH2oVali[[idxTime]])){
      outList$vali$data$h2oTurb$rtioMoleDryH2oVali[[idxTime]] <- strftime(outList$vali$data$h2oTurb$rtioMoleDryH2oVali[[idxTime]], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")} 
  }
  
  #Unit conversion for dp01 sub-aggregated irgaTurb validation data
  outList$vali$data <- eddy4R.base::wrap.unit.conv.out.ec(inpList = outList$vali$data, MethType = "vali") 
  
  #adding validation data to dp01AgrSub
  outList$dp01AgrSub$data$co2Turb$rtioMoleDryCo2Vali <- outList$vali$data$co2Turb$rtioMoleDryCo2Vali
  outList$dp01AgrSub$data$h2oTurb$rtioMoleDryH2oVali <- outList$vali$data$h2oTurb$rtioMoleDryH2oVali
  
  #Packaging sub-aggregated (e.g.1-min) dp01 qfqm for writing to HDF5 file
  outList$dp01AgrSub$qfqm <- sapply(names(inpList$dp01AgrSub$qfqm), function(x) eddy4R.base::def.hdf5.pack(inpList = inpList$dp01AgrSub$qfqm, time = inpList$dp01AgrSub$time, Dp = x))
  
  #Applying units to each output in dp01 sub-aggregrated qfqm
  outList$dp01AgrSub$qfqm <- eddy4R.base::wrap.unit.conv.out.ec(inpList = outList$dp01AgrSub$qfqm, MethType = "qfqm")
  
  if(MethUcrt == TRUE){
  #Packaging sub-aggregated (e.g.1-min) dp01 ucrt for writing to HDF5 file
  outList$dp01AgrSub$ucrt <- sapply(names(inpList$dp01AgrSub$ucrt), function(x) eddy4R.base::def.hdf5.pack(inpList = inpList$dp01AgrSub$ucrt, time = inpList$dp01AgrSub$time, Dp = x))
  
  #Unit conversion for dp01 sub-aggregated ucrt values
  outList$dp01AgrSub$ucrt <- eddy4R.base::wrap.unit.conv.out.ec(inpList = outList$dp01AgrSub$ucrt, MethType = "ucrt")
  }
}

######################################################################
#End of packing output into proper structure format; begin writing output
######################################################################

#Applying the HDF5 write output function across all DPs
lapply(names(outList$data), function(x) eddy4R.base::def.hdf5.wrte.dp01(inpList = outList, FileOut = FileOut, SiteLoca = SiteLoca, LvlTowr = LvlTowr, Dp01 = x, MethUcrt = MethUcrt, MethSubAgr = MethSubAgr))

######################################################################
#dp04 formatting and output
######################################################################
if(MethDp04 == TRUE){
  
  #Create HDF5 connection to the output file  
  idFile <- rhdf5::H5Fopen(FileOut)

  for(idxDp04 in names(inpList$dp04$data)){
    #idxDp04 <- names(inpList$dp04$data)[1]
    if(idxDp04 == "foot") {
      #Adding time to output dataframe
      rptDp04 <-  cbind(timeBgn = outList$data$soni$veloXaxsErth$timeBgn, timeEnd = outList$data$soni$veloXaxsErth$timeEnd, inpList$dp04$data[[idxDp04]]$stat, stringsAsFactors = FALSE)
      
      #Writing unit attributes to each variable to the dataframe level
      attributes(rptDp04)$unit <- attributes(inpList$dp04$data[[idxDp04]]$stat)$unit
      #Adding timeBgn and timeEnd attributes
      attributes(rptDp04)$unit <- c("NA", "NA", attributes(rptDp04)$unit)

      #Open connection to dp04 data level
      idDataDp04 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp04/data/",idxDp04))
      
      #Output the attributes
      rhdf5::h5writeAttribute("North,East,Down (NED)", h5obj = idDataDp04, name = "sysCordRefe")
      
      #Writing flux data to output HDF5 file
      rhdf5::h5writeDataset.data.frame(obj = rptDp04, h5loc = idDataDp04, name = "stat", DataFrameAsCompound = TRUE)
      
      #Connection to dataset
      idDataDp04Df <- rhdf5::H5Dopen(idDataDp04, "stat")
      #Output the attributes
      rhdf5::h5writeAttribute(attributes(rptDp04)$unit, h5obj = idDataDp04Df, name = "unit")
      
      if(MethExpd == TRUE) {
        # Create group level for the turbulence footprint grids
        idDataDp04Expd <- rhdf5::H5Gcreate(idDataDp04, "grid")
        #Create group for output
        idDataDp04ExpdGrid <- rhdf5::H5Gcreate(idDataDp04Expd, "turb")
        
        #Write the gridded matrixes to output
        lapply(base::names(inpList$dp04$data[[idxDp04]]$grid$turb), function(x) {rhdf5::h5writeDataset.matrix(obj = inpList$dp04$data[[idxDp04]]$grid$turb[[x]], h5loc= idDataDp04ExpdGrid, name = x)})
        
      }
      ########################################################################################################################
      #Write dp04 QFQM output
      ########################################################################################################################
      
      #Adding time to output dataframe
      rptDp04Qfqm <-  cbind(timeBgn = outList$data$soni$veloXaxsErth$timeBgn, timeEnd = outList$data$soni$veloXaxsErth$timeEnd, inpList$dp04$qfqm[[idxDp04]]$turb, stringsAsFactors = FALSE)
      
      
      #Writing unit attributes to each variable to the dataframe level
      attributes(rptDp04Qfqm)$unit <- sapply(names(inpList$dp04$qfqm[[idxDp04]]$turb), function(x) attributes(inpList$dp04$qfqm[[idxDp04]]$turb[[x]])$unit)
      #Adding timeBgn and timeEnd attributes
      tmpAttr <- c()
      attributes(tmpAttr)$unit[["timeBgn"]] <- "NA"
      attributes(tmpAttr)$unit[["timeEnd"]] <- "NA"
      attributes(rptDp04Qfqm)$unit <- base::as.character(c(attributes(tmpAttr)$unit, attributes(rptDp04Qfqm)$unit))
      
      #Open connection to dp04 data level
      idQfqmDp04 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp04/qfqm/",idxDp04))
      
      #Writing flux data to output HDF5 file
      rhdf5::h5writeDataset.data.frame(obj = rptDp04Qfqm, h5loc = idQfqmDp04, name = "turb", DataFrameAsCompound = TRUE)
      
      #Connection to dataset
      idQfqmDp04Df <- rhdf5::H5Dopen(idQfqmDp04, "turb")
      #Output the attributes
      rhdf5::h5writeAttribute(attributes(rptDp04Qfqm)$unit, h5obj = idQfqmDp04Df, name = "unit")
      
      
    } 
    else {
    #output only flux for fluxCo2 in basic file
      if (idxDp04 %in% c("fluxCo2", "fluxH2o") & MethExpd == FALSE){
        inpList$dp04$data[[idxDp04]]$turb$fluxCor <- NULL
        inpList$dp04$data[[idxDp04]]$turb$fluxRaw <- NULL
      }
    #Adding time to output dataframe
    rptDp04 <-  cbind(timeBgn = outList$data$soni$veloXaxsErth$timeBgn, timeEnd = outList$data$soni$veloXaxsErth$timeEnd, inpList$dp04$data[[idxDp04]]$turb, stringsAsFactors = FALSE)
  
  
  #Writing unit attributes to each variable to the dataframe level
  attributes(rptDp04)$unit <- sapply(names(inpList$dp04$data[[idxDp04]]$turb), function(x) attributes(inpList$dp04$data[[idxDp04]]$turb[[x]])$unit)

  #Adding timeBgn and timeEnd attributes
  tmpAttr <- c()
  attributes(tmpAttr)$unit[["timeBgn"]] <- "NA"
  attributes(tmpAttr)$unit[["timeEnd"]] <- "NA"
  attributes(rptDp04)$unit <- base::as.character(c(attributes(tmpAttr)$unit, attributes(rptDp04)$unit))
  
  #Open connection to dp04 data level
  idDataDp04 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp04/data/",idxDp04))
  
  #Writing flux data to output HDF5 file
  rhdf5::h5writeDataset.data.frame(obj = rptDp04, h5loc = idDataDp04, name = "turb", DataFrameAsCompound = TRUE)
  
  #Connection to dataset
  idDataDp04Df <- rhdf5::H5Dopen(idDataDp04, "turb")
  #Output the attributes
  rhdf5::h5writeAttribute(attributes(rptDp04)$unit, h5obj = idDataDp04Df, name = "unit")
 
  ########################################################################################################################
  #Write dp04 QFQM output
  ########################################################################################################################
  
  #write out only qfFinl in basic file
  if (MethExpd == FALSE){
    lapply(names(inpList$dp04$qfqm[[idxDp04]]$turb), function(x){
      if (!(x %in% "qfFinl")) {
        inpList$dp04$qfqm[[idxDp04]]$turb[[x]] <<- NULL
      }
    })
  }
  #Adding time to output dataframe
  rptDp04Qfqm <-  cbind(timeBgn = outList$data$soni$veloXaxsErth$timeBgn, timeEnd = outList$data$soni$veloXaxsErth$timeEnd, inpList$dp04$qfqm[[idxDp04]]$turb, stringsAsFactors = FALSE)
  
  
  #Writing unit attributes to each variable to the dataframe level
  attributes(rptDp04Qfqm)$unit <- sapply(names(inpList$dp04$qfqm[[idxDp04]]$turb), function(x) attributes(inpList$dp04$qfqm[[idxDp04]]$turb[[x]])$unit)
  #Adding timeBgn and timeEnd attributes
  tmpAttr <- c()
  attributes(tmpAttr)$unit[["timeBgn"]] <- "NA"
  attributes(tmpAttr)$unit[["timeEnd"]] <- "NA"
  attributes(rptDp04Qfqm)$unit <- base::as.character(c(attributes(tmpAttr)$unit, attributes(rptDp04Qfqm)$unit))
  
  #Open connection to dp04 data level
  idQfqmDp04 <- rhdf5::H5Gopen(idFile,paste0("/", SiteLoca, "/dp04/qfqm/",idxDp04))
  
  #Writing flux data to output HDF5 file
  rhdf5::h5writeDataset.data.frame(obj = rptDp04Qfqm, h5loc = idQfqmDp04, name = "turb", DataFrameAsCompound = TRUE)
  
  #Connection to dataset
  idQfqmDp04Df <- rhdf5::H5Dopen(idQfqmDp04, "turb")
  #Output the attributes
  rhdf5::h5writeAttribute(attributes(rptDp04Qfqm)$unit, h5obj = idQfqmDp04Df, name = "unit")
    }                                
  } 
  #Close HDF5 connections
  rhdf5::h5closeAll() 
}

######################################################################
#Writing metadata from input dp0p file to output dp01 file
######################################################################
eddy4R.base::def.hdf5.copy.para(FileInp = FileInp, FileOut = FileOut)

#Create HDF5 connection to the output file  
idFile <- rhdf5::H5Fopen(FileOut)
#Open connection to dp04 data level
idSite <- rhdf5::H5Gopen(idFile, paste0("/", SiteLoca))
  
#Write updated Pfit coefficients and UTC time diff to output file
#Output the attributes
rhdf5::h5writeAttribute(round(Meta$Sci$`Pf$AngEnuXaxs`, digits = 6), h5obj = idSite, name = "Pf$AngEnuXaxs")
rhdf5::h5writeAttribute(round(Meta$Sci$`Pf$AngEnuYaxs`, digits = 6), h5obj = idSite, name = "Pf$AngEnuYaxs")
rhdf5::h5writeAttribute(round(Meta$Sci$`Pf$Ofst`, digits = 6), h5obj = idSite, name = "Pf$Ofst")
rhdf5::h5writeAttribute(Meta$Site$TimeDiffUtcLt, h5obj = idSite, name = "TimeDiffUtcLt")

#Close HDF5 connections
rhdf5::h5closeAll() 
}
