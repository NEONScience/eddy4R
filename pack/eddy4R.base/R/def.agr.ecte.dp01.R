##############################################################################################
#' @title Definition function: aggregation of ecte dp01 outputs

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function to produce a dataframe of indices and corresponding times for aggregation periods.
#'
#' @param inpList a list of dp01 computed output statistics and dp01 quality flags and quality metrics over multiple aggregations periods that need to be combined.
#' 
#' @return A dataframe of indices and corresponding times for aggregation periods.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, aggregation, averaging intervals

#' @examples 


#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-04-30)
#     original creation
##############################################################################################

def.agr.ecte.dp01 <- function(
inpList 
  
){
  
  # concatenate results
  rpt <- list()
  # timestamps
  rpt$time <- data.frame(
    timeBgn = inpList$whrData20hz$timeBgn,
    timeEnd = inpList$whrData20hz$timeEnd
  )
  
  
  
  # loop around data products
  for(idxDp01 in names(inpList$dp01[[1]])) {
    
    # combine data for different time periods
    # http://stackoverflow.com/questions/10832288/coerce-a-specific-sublist-of-a-list-to-a-matrix-data-frame-with-base-r
    # https://ryouready.wordpress.com/2009/01/23/r-combining-vectors-or-data-frames-of-unequal-length-into-one-data-frame/
    # http://stackoverflow.com/questions/15673550/why-is-rbindlist-better-than-rbind
    # http://stackoverflow.com/questions/26843861/replace-rbind-in-for-loop-with-lapply-2nd-circle-of-hell
    rpt$data[[idxDp01]] <- 
      
      # first call to lapply, targeting the result data.frames to be created (data sub-products: mean, min, max, vari", numSamp)
      lapply(names(inpList$dp01[[1]][[idxDp01]]), function(y)
        
        # second call to lapply, targeting the observations to be combined into the result data.frames
        do.call(rbind, lapply(1:length(inpList$dp01), function(x) inpList$dp01[[x]][[idxDp01]][[y]] ))
        
      )
    
    # assign names to data.frames      
    names(rpt$data[[idxDp01]]) <- names(inpList$dp01[[1]][[idxDp01]])
    
    #Put together output list for qfqm 
    rpt$qfqm[[idxDp01]] <- 
      
      # first call to lapply, targeting the result data.frames to be created (data sub-products: mean, min, max, vari", numSamp)
      lapply(names(inpList$qfqmOut[[1]][[idxDp01]]), function(y)
        
        # second call to lapply, targeting the observations to be combined into the result data.frames
        do.call(rbind, lapply(1:length(inpList$qfqmOut), function(x) inpList$qfqmOut[[x]][[idxDp01]][[y]] ))
        
      )
    
    # assign names to data.frames      
    names(rpt$qfqm[[idxDp01]]) <- names(inpList$qfqmOut[[1]][[idxDp01]])
    
    
  }
  
  
  #Return output
  return(rpt)
  
}
