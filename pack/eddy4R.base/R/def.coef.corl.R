##############################################################################################
#' @title Coriolis coefficient

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description Function definition. Determine coriolis coefficient.

#' @param \code{lat} Required. Latitude [degrees North]

#' @return Coriolis coefficient for \code{lat} [rad s-1].

#' @references Currently none

#' @keywords natural constants

#' @examples Currently none

#' @seealso Currently none

#' @export
#' 
# changelog and author contributions / copyrights
#   Stefan Metzger (2013-08-26)
#     original creation of a file with global constants that is called via source()
#   Cove Sturtevant (2016-02-09)
#     conformed code to EC TES coding convention (previously within conNat.r, now as own function)
#          changed function name from coriolis to def.coef.corl
#   Cove Sturtevant (2016-03-10)
#     adjusted used of natural constants used within function to call internal package data
##############################################################################################

def.coef.corl <- function(lat) {
  
  # Compute coriolis coefficient
  coefCorl <- 2 * eddy4R.base::Natu$AvelErth * base::sin(def.aply.conv.poly(data=lat,coefPoly=eddy4R.base::Conv$DegRad)) # [rad s-1]
  
  return(coefCorl)
  
}
  
