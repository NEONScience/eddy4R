##############################################################################################
#' @title Definition function: Convert from non-verbose to verbose output of quality tests

#' @author
#' Cove Sturtevant \email{eddy4R.info@gmail.com} \cr

#' @description 
#' Function definition. Convert from the non-verbose option (vector positions of failed and na test results) of quality test algorithms to the verbose output (actual quality flag values).  

#' @param setQf A named list of variables, each itself a named list of flags (standard or user-defined names), with nested lists of $fail and $na vector positions of failed and na quality tests for that variable and flag (eg. setQf$X$setQfStep$fail and setQf$Y$setQfStep$na).
#' @param numRow A single integer indicating the number of rows in the original data from which the quality test results in \code{setQf} were derived

#' @return A list of variables matching those in \code{setQf}, each containing a data frame of quality flags for that variable. Number of rows match that of \code{numRow} 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords quality flag

#' @examples 
#' setQf <- list(var01=list(qfRng = list(fail=c(2,5),na=c(3,10)),qfStep = list(fail=numeric(0),na=c(3,4,10)))) # non-verbose output from quality testing
#' qf <- def.conv.qf.vrbs(setQf=setQf,numRow=10) # Convert to quality flag values

#' @seealso 
#' \code{\link[eddy4R.qaqc]{def.plau}}
#' \code{\link[eddy4R.qaqc]{def.conv.qf.not.vrbs}}

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-11-22)
#     original creation 
#   Natchaya P-Durden (2018-04-04)
#    applied eddy4R term name convention; replaced posQf by setQf
#   Natchaya P-Durden (2018-04-11)
#    applied eddy4R term name convention; replaced pos by idx
##############################################################################################
def.conv.qf.vrbs <- function (
  setQf,
  numRow
) {
  
  nameVar <- base::names(setQf)
  numVar <- base::length(nameVar)
  
  qf <- base::vector("list",numVar) # Initialize
  base::names(qf) <- nameVar
  
  for (idxVar in nameVar) {
    
    # Intialize flags for this variable
    numQf <- base::length(setQf[[idxVar]])
    qf[[idxVar]] <- base::as.data.frame(base::matrix(0,ncol=numQf,nrow=numRow)) # Intialize quality flags with all 0
    nameQf <- base::names(setQf[[idxVar]])
    base::names(qf[[idxVar]]) <- nameQf
    
    for (idxQf in nameQf) {
      
      # Assign quality flags for failed & na vector positions
      idxFail <- setQf[[idxVar]][[idxQf]]$fail
      idxNa <- setQf[[idxVar]][[idxQf]]$na
      qf[[idxVar]][idxFail,idxQf] <- 1 # fail
      qf[[idxVar]][idxNa,idxQf] <- -1 # na
      
    }
    
  }
  
  return(qf)
  
}
