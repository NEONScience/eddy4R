##############################################################################################
#' @title Definition function: Determine cutoff frequency empirically 

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

#' @description Determine cutoff frequency empirically .

#' @param FreqCut cutoff frequency
#' @param idep independent variable, preferabley f, but n is possible
#' @param depe dependent variable, spectra or cospectra
#' @param CoefCorRefe reference correction factor

#' @return Optimality criterion from the difference of the reference correction and the derived correction.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' Foken T., Leuning R., Oncley S.R., Mauder M., Aubinet M. (2012) Corrections and Data Quality Control. In: Aubinet M., Vesala T., Papale D. (eds) Eddy Covariance. Springer Atmospheric Sciences. Springer, Dordrecht. https://doi.org/10.1007/978-94-007-2351-1_4
#' Eugster, W. and W. Senn (1995). "A COSPECTRAL CORRECTION MODEL FOR MEASUREMENT OF TURBULENT NO2 FLUX." Boundary-Layer Meteorology 74(4): 321-340.
#' Foken, Thomas. (2017). Micrometeorology. 10.1007/978-3-642-25440-6. 


#' @keywords Fast Fourier Transform, FFT, spectral, cut-off frequency, transfer function

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#determine cutoff frequency empirically 
########################################################
def.spec.freq.cut.emp <- function(
  #cutoff frequency
  FreqCut,
  #independent variable, preferabley f, but n is possible
  idep,
  #dependent variable, spectra or cospectra
  depe,
  #reference correction factor
  CoefCorRefe
) {
  
  library(eddy4R.base)
  rlog = Logger.Singleton$new() #class defined in eddy4R.base
  rlog$debug("in function wrap.def.spec.freq.cut.emp.R")
  
  #sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21
  tfunSigm <- eddy4R.turb::def.spec.tfun.sigm(FreqCut=FreqCut, Freq=idep)
  
  #plotting
  #       plot(fun_tsig ~ ide)
  
  #calculate resulting correction factor over all frequencies
  CoefCor <- base::sum(depe / tfunSigm, na.rm=TRUE) / base::sum(depe, na.rm=TRUE)
  
  #calculate optimality criterion
  critOptm <- base::abs(CoefCorRefe - CoefCor)
  
  #return results
  return(critOptm)
  
  ########################################################  
}
########################################################

