##############################################################################################
#' @title Definition function: Function to define nonlinear model to fit to veloZaxsHor spectra to determine frequency peak

#' @author
#' Adam Young \email{younga1@battelleecology.org}

#' @description 
#' Function to define nonlinear model to fit to veloZaxsHor spectra and determine frequency peak. The output from this model is then used with 'optim' function to the frequency at which vertical wind speak spectral power reaches it's peak value.

#' @param specPowrWght Spectra to fit model to
#' @param para Parameters to be fit model via optimization.
#' @param freq frequency vector as independent variable.

#' 
#' @return List of spectra peak freq values and indices, as welll as best fit parameters and interpolated results.
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords Currently none.

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   Adam Young (2024-05-17)
#     original creation
#   Adam Young (2024-05-21)
#     Modified to include rounding in comparison of frequency values to find peak, needed
#     for comparison of significant figures
##############################################################################################


# start function def.spec.peak.modl()

def.spec.peak <- function(specPowrWght, para, freq) {
  
  # Get parameter estimates from model of spectra peak. Found using RMSE
  paraEst <- optim(
    par = para, 
    fn = function(para, freq, spec) mean((eddy4R.turb::def.spec.peak.modl(para, freq) - specPowrWght)^2), 
    freq = freq,
    method = "Nelder-Mead"
  )
  
  # Interpolated 100-point function of freq-weighted vertical wind speed power spectra
  freqItpl <- exp(seq(log(min(freq)), log(max(freq)), length.out = 100))
  specItpl <- eddy4R.turb::def.spec.peak.modl(para = paraEst$par, freq = freqItpl)
  
  idxFreqPeakItpl <- which.max(specItpl) # Index of peak frequency
  freqPeak <- freqItpl[idxFreqPeakItpl] # Value of peak frequency
  
  # Don't start extrapolating inertial sub range at peak, move one full scale higher ...
  # First, pick only frequency indices that are greater than the peak frequency
  idxFreqPeak <- which(round(freq, 6) >= round(freqPeak, 6)) # Rounding needed for numerical comparison of significant figs
  # Second, keep only the one closest to peak frequency
  idxFreqPeak <- idxFreqPeak[abs(freq[idxFreqPeak] - freqPeak) == min(abs(freq[idxFreqPeak] - freqPeak))] 
  
  return(list(
    freqPeak = freqPeak, 
    idxFreqPeak = idxFreqPeak, 
    para = paraEst$par, 
    rptItpl = data.frame(freqItpl = freqItpl, specItpl = specItpl)
  ))
  
}
