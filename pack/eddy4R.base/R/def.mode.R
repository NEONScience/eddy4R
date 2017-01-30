##############################################################################################
#' @title Definition function: Calculate mode based on a continuous distribution

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Calculate mode based on a continuous distribution.

#' @param \code{x} Either a vector or an object of class numeric of the data from which the estimate is to be computed.

#' @return 
#' The estimated arithmetic mode of the values in \code{x}. \cr

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007 \cr
#' http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode \cr

#' @keywords descriptive statistics, mode

#' @examples 
#' def.mode(x = rnorm(100))

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-11-21)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-02-22)
#     Initail naming convention for eddy4R
##############################################################################################
#calculate mode based on a continuous distribution
#http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode

def.mode <- function(x) {
    
    dens <- stats:::density(x)
    rpt <- dens$x[which.max(dens$y)]
    
#return reported object    
return(rpt)

# end function def.mode()   
}
