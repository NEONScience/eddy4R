##############################################################################################
#' @title Definition function: Sigmoidal transfer function (Lorentzian)

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

#' @description Sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21.

#' @param FreqCut half-power / cut-off frequency
#' @param Freq frequency

#' @return A Sigmoidal transfer function for the defined frequencies and cut-off frequency.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' Foken T., Leuning R., Oncley S.R., Mauder M., Aubinet M. (2012) Corrections and Data Quality Control. In: Aubinet M., Vesala T., Papale D. (eds) Eddy Covariance. Springer Atmospheric Sciences. Springer, Dordrecht. https://doi.org/10.1007/978-94-007-2351-1_4
#' Eugster, W. and W. Senn (1995). "A COSPECTRAL CORRECTION MODEL FOR MEASUREMENT OF TURBULENT NO2 FLUX." Boundary-Layer Meteorology 74(4): 321-340.
#' Foken, Thomas. (2017). Micrometeorology. 10.1007/978-3-642-25440-6. 

#' @keywords Fast Fourier Transform, FFT, spectral

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21
########################################################
def.spec.tfun.sigm <- function(
  #half-power / cut-off frequency
  FreqCut, 
  #frequency
  Freq
  ){
  #sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21
  tfunSigm <- 1 / (1 + (Freq / FreqCut)^2)
  
  #Return transfer function
  return(tfunSigm)
  
} #End of function


