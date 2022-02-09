##############################################################################################
#' @title Definition function: 63\% frequency constant after Aubinet (2012) Eq. 4.22

# type (one of function definition, function wrapper, workflow, demo): function definition

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

#' @description 63\% frequency constant after Aubinet (2012) Eq. 4.22.

#' @param inpPrd numeric, time period in seconds

#' @return A numeric vector \code{freqOut} of equivalent frequencies to the input time periods

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

def.spec.freq.cnst <- function(inpPrd) {
  
  #Convert time/period constant values to frequency
  freqOut <- 1 / (2 * pi * inpPrd)
  
  #Return output
  return(freqOut)
  
}#End function
