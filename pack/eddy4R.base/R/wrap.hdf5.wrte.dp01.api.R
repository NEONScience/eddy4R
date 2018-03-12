##############################################################################################
#' @title Wrapper function: Gather/Write reingested NEON Level 1 data, qfqm, and uncertainty to output HDF5

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function. To write NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points), quality flags and quality metrics, and uncertainty values gathered from via the API to an output HDF5 file. 

#' @param date Character: The date for the data to be gathered in ISO format ("YYYYmmdd").
#' @param FileOut Character: The file name for the output HDF5 file
#' @param SiteLoca Character: Site location.
#' @param Dp01 Character: A vector of data product names for the data to be gathered.
#' @param LevlTowr Character: The tower level that the sensor data is being collected in NEON data product convention (HOR_VER).
#' @param TimeAgr Integer: The time aggregation index in minutes (i.e. 30)
#' 
#' @return An updated dp0p HDF5 file with dp01 data, qfqm, and uncertainty written
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords output, HDF5

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2018-02-28)
#     original creation
##############################################################################################

# date <- "20170901"
# 
# SiteLoca <- "CPER"
# 
# DpName <- "tempAirLvl" #"DP1.00002.001" #SAAT
# #DpName <- "tempAirTop" "DP1.00003.001" #TRAAT
# FileOut <- "/home/ddurden/eddy/data/dev_tests/dp01/ECSE_dp0p_CPER_2017-09-01.h5"
# 
# LevlTowr <- "000_040"  
# 
# TimeAgr <- c(1,30)

wrap.hdf5.wrte.dp01.api <- function(
  date,
  FileOut,
  SiteLoca,
  DpName = c("tempAirLvl", "tempAirTop"),
  LevlTowr,
  TimeAgr = c(1,30)
){
 
  #Initialize the reporting data list
 rpt <- list() 
 
 #Loop around data products
 for(idxDp in DpName){
 #Call the definition function for all the data product
 rpt[[idxDp]] <- lapply(TimeAgr, function(x) {
   #Use the definition function to grab reingest data
   def.hdf5.wrte.dp01.api(date = date, SiteLoca = SiteLoca, FileOut = FileOut, DpName = idxDp, LevlTowr = LevlTowr, TimeAgr = x)
 })# End of lapply function
 } #End of for loop around dp01 data products
  
} #End of wrap.hdf5.wrte.dp01.api function
