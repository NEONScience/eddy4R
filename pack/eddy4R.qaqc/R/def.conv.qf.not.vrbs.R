##############################################################################################
#' @title Definition function: Convert from verbose to non-verbose output of quality tests

#' @author
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description 
#' Function definition. Convert from the verbose output of quality test algorithms (actual quality flag values) to the non-verbose option (vector positions of failed and na test results).  

#' @param qf A list of variables, each containing a data frame of quality flags for that variable. 

#' @return A named list of variables matching those in \code{qf}, each itself a named list of flags 
#' (standard or user-defined names), with nested lists of $fail and $na vector positions of failed and na 
#' quality test results for that variable and flag (eg. setQf$X$setQfStep$fail and setQf$Y$setQfStep$na). 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords quality flag

#' @examples 
#' qf <- list(var01=data.frame(qfRng = c(0,1,-1,0,1,0,0,0,0,-1),qfStep = c(0,0,-1,-1,0,0,0,0,0,-1))) # Verbose output from quality testing
#' setQf <- def.conv.qf.not.vrbs(qf=qf) # Convert to vector positions of failed and na quality tests

#' @seealso 
#' \code{\link[eddy4R.qaqc]{def.plau}}
#' \code{\link[eddy4R.qaqc]{def.conv.qf.vrbs}}

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-11-22)
#     original creation 
#   Natchaya P-Durden (2018-04-04)
#    applied eddy4R term name convention; replaced posQf by setQf
##############################################################################################
def.conv.qf.not.vrbs <- function (
  qf
) {
  
  nameVar <- base::names(qf)
  numVar <- base::length(nameVar)
  
  setQf <- base::vector("list",numVar) # Initialize
  base::names(setQf) <- nameVar
  
  for (idxVar in nameVar) {
    
    # Intialize flags for this variable
    numQf <- base::length(qf[[idxVar]])
    setQf[[idxVar]] <- base::vector("list",numQf)
    base::names(setQf[[idxVar]]) <- base::names(qf[[idxVar]])
    
    for (idxQf in 1:numQf) {
      setQf[[idxVar]][[idxQf]] <- base::list(fail=numeric(0),na=numeric(0)) # initialize
      
      # Get positions of failed & na vector positions
      setQf[[idxVar]][[idxQf]]$fail <- base::which(qf[[idxVar]][,idxQf] == 1) # fail
      setQf[[idxVar]][[idxQf]]$na <- base::which(qf[[idxVar]][,idxQf] == -1) # na
      
    }
    
  }
  
  return(setQf)
  
}
