##############################################################################################
#' @title Wrapper function: Write NEON Level 1 data, qfqm, and uncertainty to output HDF5

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Wrapper function. To write NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points), quality flags and quality metrics, and uncertainty quantification to an output HDF5 file. 

#' @param inpList A list of including dp01 data, quality flags and quality metrics, and uncertainty calculations to package and write to an output HDF5 file
#' @param FileIn The file name for the input dp0p HDF5 file to grab metadata
#' @param FileOut The file name for the output HDF5 file
#' @param SiteLoca Character: Site location.
#' @param LevlTowr The tower level that the sensor data is being collected in NEON data product convention (HOR_VER)
#' @param MethUcrt Logical: Determines if uncertainty information is available for output.
#' @param MethDp04 logical indicating if ECTE dp04 HDF5 data should be included.
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
#   David Durden (2017-06-06)
#     Adding uncertaitny HDF5 packaging calls
#   David Durden (2017-06-29)
#     Adding switches for writing 1-min and ucrt output
#   David Durden (2017-09-10)
#     Adding switches for writing dp04 output
##############################################################################################


# start function wrap.hdf5.wrte.dp01()

wrap.hdf5.wrte.dp01 <- function(
  inpList,
  FileIn,
  FileOut,
  SiteLoca,
  LevlTowr,
  MethUcrt = TRUE,
  MethDp04 = FALSE,
  MethSubAgr = TRUE
){

#Initializing output list
outList <- list()


#Packaging 30-min dp01 data output for writing to HDF5 file
outList$data <- sapply(names(inpList$data), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$data, time = inpList$time, Dp01 = x))

#Packaging 30-min dp01 qfqm output for writing to HDF5 file
outList$qfqm <- sapply(names(inpList$qfqm), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$qfqm, time = inpList$time, Dp01 = x))

if(MethUcrt == TRUE){
#Packaging 30-min dp01 ucrt output for writing to HDF5 file
outList$ucrt <- sapply(names(inpList$ucrt), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$ucrt, time = inpList$time, Dp01 = x))
}

if(MethSubAgr == TRUE){
  #Packaging sub-aggregated (e.g.1-min) dp01 data for writing to HDF5 file
  outList$dp01AgrSub$data <- sapply(names(inpList$dp01AgrSub$data), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$dp01AgrSub$data, time = inpList$dp01AgrSub$time, Dp01 = x))
  
  #Packaging sub-aggregated (e.g.1-min) dp01 qfqm for writing to HDF5 file
  outList$dp01AgrSub$qfqm <- sapply(names(inpList$dp01AgrSub$qfqm), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$dp01AgrSub$qfqm, time = inpList$dp01AgrSub$time, Dp01 = x))
  
  if(MethUcrt == TRUE){
  #Packaging sub-aggregated (e.g.1-min) dp01 ucrt for writing to HDF5 file
  outList$dp01AgrSub$ucrt <- sapply(names(inpList$dp01AgrSub$ucrt), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$dp01AgrSub$ucrt, time = inpList$dp01AgrSub$time, Dp01 = x))
  }
}

outList$data$soni$angZaxsErth[,which(names(outList$data$soni$angZaxsErth) %in% c("mean","min","max","vari"))] <- def.unit.conv(outList$data$soni$angZaxsErth[,which(names(outList$data$soni$angZaxsErth) %in% c("mean","min","max","vari"))], unitFrom = "rad", unitTo = "deg")
attr(x = outList$data$soni$angZaxsErth, which = "unit") <- "deg"

if(MethSubAgr == TRUE){
  outList$dp01AgrSub$data$soni$angZaxsErth[,which(names(outList$dp01AgrSub$data$soni$angZaxsErth) %in% c("mean","min","max","vari"))] <- def.unit.conv(outList$dp01AgrSub$data$soni$angZaxsErth[,which(names(outList$dp01AgrSub$data$soni$angZaxsErth) %in% c("mean","min","max","vari"))], unitFrom = "rad", unitTo = "deg")
  attr(x = outList$dp01AgrSub$data$soni$angZaxsErth, which = "unit") <- "deg"
}
  if(MethUcrt == TRUE){
  outList$ucrt$soni$angZaxsErth[,which(names(outList$ucrt$soni$angZaxsErth) %in% c("mean","vari","se"))] <- def.unit.conv(outList$ucrt$soni$angZaxsErth[,which(names(outList$ucrt$soni$angZaxsErth) %in% c("mean","vari","se"))], unitFrom = "rad", unitTo = "deg")
  attr(x = outList$ucrt$soni$angZaxsErth, which = "unit") <- "deg"

  if(MethSubAgr == TRUE){
  outList$dp01AgrSub$ucrt$soni$angZaxsErth[,which(names(outList$dp01AgrSub$ucrt$soni$angZaxsErth) %in% c("mean","vari","se"))] <- def.unit.conv(outList$dp01AgrSub$ucrt$soni$angZaxsErth[,which(names(outList$dp01AgrSub$ucrt$soni$angZaxsErth) %in% c("mean","vari","se"))], unitFrom = "rad", unitTo = "deg")
  attr(x = outList$dp01AgrSub$ucrt$soni$angZaxsErth, which = "unit") <- "deg"
  }
}
#Applying the HDF5 write output function across all DPs
lapply(names(outList$data), function(x) eddy4R.base::def.hdf5.wrte.dp01(inpList = outList, FileOut = FileOut, SiteLoca = SiteLoca, LevlTowr = LevlTowr, Dp01 = x, MethUcrt = MethUcrt, MethSubAgr = MethSubAgr))

if(MethDp04 == TRUE){
  
  #Create HDF5 connection to the output file  
  fid <- rhdf5::H5Fopen(FileOut)

  for(idxDp04 in names(inpList$dp04$data)){
    #Adding time to output dataframe
    rptDp04 <-  cbind(timeBgn = outList$data$soni$veloXaxsErth$timeBgn, timeEnd = outList$data$soni$veloXaxsErth$timeEnd, inpList$dp04$data[[idxDp04]]$turb, stringsAsFactors = FALSE)
  }
  
  #Writing unit attributes to each variable to the dataframe level
  attributes(rptDp04)$unit <- sapply(names(inpList$dp04$data[[idxDp04]]$turb), function(x) attributes(inpList$dp04$data[[idxDp04]]$turb)$unit)

  #Open connection to dp04 data level
  idDataDp04 <- rhdf5::H5Gopen(fid,paste0("/", SiteLoca, "/dp04/data/",idxDp04))
  
  #Writing flux data to output HDF5 file
  rhdf5::h5writeDataset.data.frame(obj = rptDp04, h5loc = idDataDp04, name = "turb", DataFrameAsCompound = TRUE))
  
  #Output the attributes
  rhdf5::h5writeAttribute(attributes(inpList$dp04$data[[idxDp04]]$turb)$unit, h5obj = idDataDp04, name = "unit")                                        
 
  rhdf5::H5close()                                           
}                                          
#Writing metadata from input dp0p file to output dp01 file
eddy4R.base::def.para.hdf5.dp01(FileIn = FileIn, FileOut = FileOut)

}