##############################################################################################
#' @title Convert from verbose to non-verbose output of quality tests

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org} \cr

#' @description 
#' Function definition. Convert from the non-verbose option (vector positions of failed and na test results) of quality test algorithms to the verbose output (actual quality flag values).  

#' @param posQf A named list of variables, each itself a named list of flags (standard or user-defined names), with nested lists of $fail and $na vector positions of failed and na quality tests for that variable and flag (eg. posQf$X$posQfStep$fail and posQf$Y$posQfStep$na).
#' @param numRow A single integer indicating the number of rows in the original data from which the quality test results in \code{posQf} were derived

#' @return A list of variables matching those in \code{posQf}, each containing a data frame of quality flags for that variable. Number of rows match that of \code{numRow} 

#' @references None

#' @keywords quality flag

#' @examples 
#' posQf <- list(var01=list(qfRng = list(fail=c(2,5),na=c(3,10)),qfStep = list(fail=numeric(0),na=c(3,4,10)))) # non-verbose output from quality testing
#' qf <- def.conv.qf.vrbs(posQf=posQf,numRow=10) # Convert to quality flag values

#' @seealso 
#' \code{\link[eddy4R.qaqc]{def.plau}}
#' \code{\link[eddy4R.qaqc]{def.conv.qf.not.vrbs}}

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-11-22)
#     original creation 
##############################################################################################
def.conv.qf.vrbs <- function (
  posQf,
  numRow
) {
  
  nameVar <- base::names(posQf)
  numVar <- base::length(nameVar)
  
  qf <- base::vector("list",numVar) # Initialize
  base::names(qf) <- nameVar
  
  for (idxVar in nameVar) {
    
    # Intialize flags for this variable
    numQf <- base::length(posQf[[idxVar]])
    qf[[idxVar]] <- base::as.data.frame(base::matrix(0,ncol=numQf,nrow=numRow)) # Intialize quality flags with all 0
    nameQf <- base::names(posQf[[idxVar]])
    base::names(qf[[idxVar]]) <- nameQf
    
    for (idxQf in nameQf) {
      
      # Assign quality flags for failed & na vector positions
      posFail <- posQf[[idxVar]][[idxQf]]$fail
      posNa <- posQf[[idxVar]][[idxQf]]$na
      qf[[idxVar]][posFail,idxQf] <- 1 # fail
      qf[[idxVar]][posNa,idxQf] <- -1 # na
      
    }
    
  }
  
  return(qf)
  
}
