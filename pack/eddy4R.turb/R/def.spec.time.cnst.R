##############################################################################################
#' @title Definition function: 63\% time constant after Aubinet (2012) Eq. 4.22

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

#' @description 63\% time constant after Aubinet (2012) Eq. 4.22.

#' @param inpFreq numeric, frequency in Hertz (Hz)

#' @return A numeric vector \code{prdOut} of equivalent time periods to the input frequencies in seconds (sec) 

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' Aubinet (2012)

#' @keywords Fast Fourier Transform, FFT, spectral

#' @examples 
#' 
#' inpFreq <- seq(1, 0.02, by = -0.02)
#' def.spec.time.cnst(inpFreq = inpFreq)

#' @seealso Currently none

#' @export
##############################################################################################

def.spec.time.cnst <- function(inpFreq) {
 
  #Calculate period from frequency
  prdOut <- 1 / (2 * pi * inpFreq)
  
 #Return calculated period
 return(prdOut)
 
} #End of function
