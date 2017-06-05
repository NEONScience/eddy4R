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
#' 
#' @return An HDF5 file with dp01 data, qfqm, and uncertainty written
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords output, HDF5

#' @examples


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2017-06-04)
#     original creation
##############################################################################################


# start function wrap.hdf5.wrte.dp01()

wrap.hdf5.wrte.dp01 <- function(
  inpList,
  FileIn,
  FileOut,
  SiteLoca,
  LevlTowr
){


#for(idxDp01 in names(OUT$data)[which(names(OUT$data) != "time")]) {

outList <- list()


#Packaging 30-min dp01 data output for writing to HDF5 file
outList$data <- sapply(names(inpList$data), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$data, time = inpList$time, Dp01 = x))

#Packaging 30-min dp01 qfqm output for writing to HDF5 file
outList$qfqm <- sapply(names(inpList$qfqm), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$qfqm, time = inpList$time, Dp01 = x))

#Packaging sub-aggregated (e.g.1-min) dp01 data for writing to HDF5 file
outList$dp01AgrSub$data <- sapply(names(inpList$qfqm), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$dp01AgrSub$data, time = inpList$dp01AgrSub$time, Dp01 = x))

#Packaging sub-aggregated (e.g.1-min) dp01 qfqm for writing to HDF5 file
outList$dp01AgrSub$qfqm <- sapply(names(inpList$qfqm), function(x) eddy4R.base::def.hdf5.dp01.pack(inpList = inpList$dp01AgrSub$qfqm, time = inpList$dp01AgrSub$time, Dp01 = x))

lapply(names(outList$data), function(x) def.hdf5.wrte.dp01(inpList = outList, FileOut = FileOut, SiteLoca = SiteLoca, LevlTowr = LevlTowr, Dp01 = x))

eddy4R.base::def.para.hdf5.dp01(FileIn = FileIn, FileOut = FileOut)

}