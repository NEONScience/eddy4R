##############################################################################################
#' @title Definition function: 63\% frequency constant after Aubinet (2012) Eq. 4.22

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

#' @description 63\% frequency constant after Aubinet (2012) Eq. 4.22.

#' @param inpPrd numeric, time period in seconds

#' @return A numeric vector \code{freqOut} of equivalent frequencies to the input time constant

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' Foken T., Leuning R., Oncley S.R., Mauder M., Aubinet M. (2012) Corrections and Data Quality Control. In: Aubinet M., Vesala T., Papale D. (eds) Eddy Covariance. Springer Atmospheric Sciences. Springer, Dordrecht. https://doi.org/10.1007/978-94-007-2351-1_4
#' Foken, Thomas. (2017). Micrometeorology. 10.1007/978-3-642-25440-6. 

#' @keywords Fast Fourier Transform, FFT, spectral

#' @examples 
#' 
#' inpTime <- seq(1, 3600, by = 10)
#' def.spec.freq.cnst(inpTime = inpTime)

#' @seealso Currently none

#' @export
##############################################################################################

def.spec.freq.cnst <- function(inpTime) {
  
  #Convert time/period constant values to frequency
  freqOut <- 1 / (2 * pi * inpTime)
  
  #Return output
  return(freqOut)
  
}#End function
