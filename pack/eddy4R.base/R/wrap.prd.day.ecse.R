##############################################################################################
#' @title Wrapper function: To perform daily ECSE processing in native resolution

#' @author
#' Natchaya Pingintha-Durden \email{eddy4R.info@gmail.com}

#' @description Wrapper function. To perform daily ECSE processing in native resolution

#' @param inpList List consisting of input data in the format provided by function \code{eddy4R.base::wrap.neon.read.hdf5.eddy()}. Of types numeric and integer.
#' 
#' @return 
#' The returned object consistes of \code{rpt} after applying the daily processing.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords ECSE, daily processing, qfqm, quality

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Natchaya Pingintha-Durden (2018-01-23)
#     original creation
##############################################################################################

wrap.prd.day.ecse <- function(
  inpList
) {
  #Create a list to hold all the output
  rpt <- list()
  
  #Removing high frequency flagged data
  #Applying the bad quality flags to the reported output data
  rpt <- eddy4R.qaqc::wrap.qf.rmv.data(inpList = inpList, Vrbs = FALSE, MethMeas = "ecse")
  
  #return output
  return(rpt)
  }