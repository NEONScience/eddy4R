##############################################################################################
#' @title Definition function: to package dp01 outputs to be written to HDF5 files

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org} \cr

#' @description 
#' Definition function to produce a list of dataframes corresponding times for aggregation periods for dp01, and are packaged to be written to the HDF5 file.
#'
#' @param inpList a list of dp01 computed output statistics or dp01 quality flags and quality metrics over multiple aggregations periods that need to be combined and formatted for output to HDF5.
#' @param time a dataframe including the timeBgn and timeEnd for the aggregated periods should be included in the data to be combined.
#' @param Dp01 which data product is being packaged to be written to the HDF5 file.
 
#' @return A list of dataframes of for aggregation periods.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, dp01, averaging intervals, HDF5

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-05-07)
#     original creation
#   Dave Durden (2017-05-22)
#     Updating time formatting for output
#   Natchaya Pingintha-Durden (2017-08-24)
#     Added MethMeas
#    Natchaya Pingintha-Durden (2017-09-07)
#     removed MethMeas and generated new function for ECSE
##############################################################################################



def.hdf5.dp01.pack <- function(
  inpList,
  time,
  Dp01
){

  #Initializing lists
rpt <- list()
tmp <- list()

#Looping around variables
for(idxVar in names(inpList[[Dp01]][[1]])) {
  #print(idxVar) #For testing
  #Reformatting data to have data subproducts at a higher hierarchical level than descriptive stats for HDF5 output
  lapply(names(inpList[[Dp01]]), function(x) {
    tmp[[idxVar]][[x]] <<- inpList[[Dp01]][[x]][,idxVar]
  }) 
  
};rm(idxVar)

# Combining list of data into datafram
rpt <- lapply(names(tmp), function(x) data.frame(do.call("cbind", tmp[[x]])))

#Copying names from input
names(rpt) <- names(tmp)

#If values come in as Posix, they must first be converted to characters
if(!is.character(time[[Dp01]]$timeBgn)){time[[Dp01]] <- lapply(time[[Dp01]], strftime, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")} 

#Adding time to output dataframe
rpt <- lapply(rpt, cbind, timeBgn = time[[Dp01]]$timeBgn, timeEnd = time[[Dp01]]$timeEnd, stringsAsFactors = FALSE)

#Looping around varaibles
for(idxVar in base::names(inpList[[Dp01]][[1]])) {
 #Writing unit attributes to each variable 
  base::attr(x = rpt[[idxVar]], which = "unit") <-
    base::attr(x = inpList[[Dp01]][[1]][,idxVar], which = "unit")
  
}; rm(idxVar)

  
return(rpt)
}
