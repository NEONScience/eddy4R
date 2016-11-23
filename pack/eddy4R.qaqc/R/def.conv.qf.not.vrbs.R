##############################################################################################
#' @title Convert from verbose to non-verbose output of quality tests

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org} \cr

#' @description 
#' Function definition. Convert from the verbose output of quality test algorithms (actual quality flag values) to the non-verbose option (vector positions of failed and na test results).  

#' @param qf A list of variables, each containing a data frame of quality flags for that variable. 

#' @return A named list of variables matching those in \code{qf}, each itself a named list of flags 
#' (standard or user-defined names), with nested lists of $fail and $na vector positions of failed and na 
#' quality test results for that variable and flag (eg. posQf$X$posQfStep$fail and posQf$Y$posQfStep$na). 

#' @references None

#' @keywords quality flag

#' @examples 
#' qf <- list(var01=data.frame(qfRng = c(0,1,-1,0,1,0,0,0,0,-1),qfStep = c(0,0,-1,-1,0,0,0,0,0,-1))) # Verbose output from quality testing
#' posQf <- def.conv.qf.not.vrbs(qf=qf) # Convert to vector positions of failed and na quality tests

#' @seealso 
#' \code{\link[eddy4R.qaqc]{def.plau}}
#' \code{\link[eddy4R.qaqc]{def.conv.qf.vrbs}}

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-11-22)
#     original creation 
##############################################################################################
def.conv.qf.not.vrbs <- function (
  qf
) {
  
  nameVar <- base::names(qf)
  numVar <- base::length(nameVar)
  
  posQf <- base::vector("list",numVar) # Initialize
  base::names(posQf) <- nameVar
  
  for (idxVar in nameVar) {
    
    # Intialize flags for this variable
    numQf <- base::length(qf[[idxVar]])
    posQf[[idxVar]] <- base::vector("list",numQf)
    base::names(posQf[[idxVar]]) <- base::names(qf[[idxVar]])
    
    for (idxQf in 1:numQf) {
      posQf[[idxVar]][[idxQf]] <- base::list(fail=numeric(0),na=numeric(0)) # initialize
      
      # Get positions of failed & na vector positions
      posQf[[idxVar]][[idxQf]]$fail <- base::which(qf[[idxVar]][,idxQf] == 1) # fail
      posQf[[idxVar]][[idxQf]]$na <- base::which(qf[[idxVar]][,idxQf] == -1) # na
      
    }
    
  }
  
  return(posQf)
  
}
