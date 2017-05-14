##############################################################################################
#' @title Definition function: to package dp01 dp01 outputs to be written to HDF5 files

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org} \cr

#' @description 
#' Definition function to produce a list of dataframes corresponding times for aggregation periods for dp01, and are packaged to be written to the HDF5 file.
#'
#' @param inpList a list of dp01 computed output statistics or dp01 quality flags and quality metrics over multiple aggregations periods that need to be combined and formatted for output to HDF5.
#'@param time a dataframe including the timeBgn and timeEnd for the aggregated periods should be included in the data to be combined.
#'@param Dp01 which data product is being packaged to be written to the HDF5 file
#'  
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
##############################################################################################



def.hdf5.dp01.pack <- function(
  inpList,
  time,
  Dp01
){

rpt <- list()
tmp <- list()

options(digits.secs = 4)

for(idxVar in names(inpList[[Dp01]][[1]])) {
  print(idxVar)  
  lapply(names(inpList[[Dp01]]), function(x) {
    tmp[[idxVar]][[x]] <<- inpList[[Dp01]][[x]][,idxVar]
  }) 
  
};rm(idxVar)


rpt <- lapply(names(tmp), function(x) data.frame(do.call("cbind", tmp[[x]])))

names(rpt) <- names(tmp)

#If values come in as characters, they must first be converted to Posix
if(is.character(time[[Dp01]]$timeBgn)){time[[Dp01]] <- lapply(time[[Dp01]], as.POSIXlt, format="%Y-%m-%dT%H:%M:%OSZ") 
#Add fraction of a second to prevent rounding
time[[Dp01]]$secs <- time[[Dp01]]$secs + 0.0001}

rpt <- lapply(rpt, cbind, timeBgn = strftime(time[[Dp01]]$timeBgn, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), timeEnd = strftime(time[[Dp01]]$timeEnd, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), stringsAsFactors = FALSE)


for(idxVar in base::names(inpList[[Dp01]][[1]])) {
  
  base::attr(x = rpt[[idxVar]], which = "unit") <-
    base::attr(x = inpList[[Dp01]][[1]][,idxVar], which = "unit")
  
}; rm(idxVar)

return(rpt)
}
