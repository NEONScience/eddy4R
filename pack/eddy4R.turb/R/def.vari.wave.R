##############################################################################################
#' @title Definition function: funtion to determine the temporally resolved variance/covariance from continuous wavelet transform

#' @author
#' David Durden \email{ddurden@battelleecology.org}
#' Stefan Metzger

#' @description 
#' Wrapper function. funtion to determine the temporally resolved variance/covariance from continuous wavelet transform including high-frequency spectral correction and selectable low-frequency cutoff The frequency response correction using Wavelet techniques described in Norbo and Katul, 2012 (NK12)

#' @param dfInp data.frame, consisting of the input data to perform the wavelet transformation


#' 
#' @return A vector constaining temporally resolved variance/covariance from the continuous wavelet transform.
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords Wavelet, spectrum, cospectrum, NK12, frequency response correction

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2017-10-07)
#     original creation
##############################################################################################


# start function def.vari.wave()


#funtion to determine the temporally resolved variance/covariance from CWT
#including high-frequency spectral correction and selectable low-frequency cutoff
def.vari.wave <- function(
  #complex Wavelet coefficients variable 1
  spec1 = mycwt[["w_met"]]@spectrum,
  #complex Wavelet coefficients variable 2
  spec2 = NULL,
  #        spec2 = mycwt[[FREQ_0_map[[vari]][2]]]@spectrum,
  #width of the wavelet at each scale [s]
  scal = mycwt[[vari]]@scale,
  #approximate corresponding Fourier period [s]
  peri = mycwt[[vari]]@period,
  #half-power frequencies for individual variables [Hz]
  freq_0 = NA,
  #which wavelengths/spatial scales to consider
  whr_peri = NULL,
  #        whr_peri = whr_peri_20,
  #normalization factor specific to the choice of Wavelet parameters
  fac_norm = rpt$coefNorm
) {
  
  if(is.null(spec2)) {
    #un-weighted wavelet scalogram
    #two approaches identical, see Mauder et al. (2008) and Stull (1988, Sect. 8.6.2 and 8.8.2)
    cwt_vc1 <- abs(spec1)^2
    #cwt_vc1 <- Re(spec1 * Conj(spec1))
    
  } else {
    
    #un-weighted wavelet cross-scalogram
    #two approaches identical, see Mauder et al. (2008) and Stull (1988, Sect. 8.6.2 and 8.8.2)
    cwt_vc1 <- Re(spec1 * Conj(spec2))
    #cwt_vc1 <- Re(spec1) * Re(spec2) + Im(spec1) * Im(spec2)
    
  }
  
  #weighted wavelet scalogram
  cwt_vc2 <- t(sapply(1:nrow(cwt_vc1), function(x) cwt_vc1[x,] / scal ))
  
  #spectral correction using sigmoidal transfer function
  #http://paos.colorado.edu/research/wavelets/faq.html#scale
  # The scale refers to the width of the wavelet.
  # The period (or inverse frequency) is the approximate Fourier period that corresponds to the oscillations within the wavelet.
  # mycwt[["w_met"]]@scale / mycwt[["w_met"]]@period
  
  #perform only if half-power frequency is defined for variable
  if(!is.na(freq_0)) {
    
    #transfer function
    fun_tsig <- fun_TSIG(freq_0 = freq_0, freq = 1/peri)
    cwt_vc3 <- t(sapply(1:nrow(cwt_vc2), function(x) cwt_vc2[x,] / fun_tsig ))
    
  } else {
    
    cwt_vc3 <- cwt_vc2
    
  }
  
  #time/space series of variance at native resolution
  if(is.null(whr_peri)) whr_peri <- 1:ncol(cwt_vc2)
  myvc2 <- fac_norm * base:::rowSums(cwt_vc2[,whr_peri])
  myvc3 <- fac_norm * base:::rowSums(cwt_vc3[,whr_peri])
  
  #conversion from variance fraction to total local variance
  myvc2 <- myvc2 * length(myvc2)
  myvc3 <- myvc3 * length(myvc3)
  
  #plot change in variance
  #between 0% and 10% along flight line for H2O
  #between 0% and 1% along flight line for T
  #plot(I(myvc2 / myvc3), log="y")
  
  #return result
  return(myvc3)
  
}