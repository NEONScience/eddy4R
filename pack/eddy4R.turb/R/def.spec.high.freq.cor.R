##############################################################################################
#' @title Definition function: function to determine the temporally resolved variance/covariance from continuous wavelet transform

#' @author
#' David Durden \email{ddurden@battelleecology.org}
#' Stefan Metzger

#' @description 
#' Definition function. Function to determine the temporally resolved variance/covariance from continuous wavelet transform including high-frequency spectral correction and selectable low-frequency cutoff. The frequency response correction using Wavelet techniques described in Norbo and Katul, 2012 (NK12)

#' @param spec01 Waves package output object spectrum, continuous wavelet transform output object complex spectrum for the first variable (typically w' ==> "veloZaxsHor")denoted as \code{object1@spectrum}.
#' @param spec02 Waves package output object spectrum, continuous wavelet transform output object complex spectrum for the second variable for cospectra denoted as \code{object2@spectrum}.
#' @param scal Waves package output object scale, width of the wavelet at each scale [s] denoted as \code{object1@scale}
#' @param prd Waves package output object period, approximate corresponding Fourier period [s] denoted as \code{object1@period}
#' @param FreqSamp numeric, that determines the time series objects points
#' @param FreqCut vector, half-power frequencies for individual variables [Hz] for determining transfer function to correct frequency response
#' @param SetPrd numeric, which wavelengths/spatial scales to consider if you want to only consider high frequency values
#' @param CoefNorm numeric, normalization factor specific to the choice of Wavelet parameters.
#' @param qfWave Wavelet flag: process (0) or not (1)
#' @param paraStbl stability parameter (numeric)
#' @param MethSpec spectrum or cospectrum  c("spec", "cosp")
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
#   Stefan Metzger (2017-10-14)
#     complete initial Wavelet correction
#   Stefan Metzger (2017-10-15)
#     MVP candidate incl. efficiency improvements
#   David Durden (2020-09-24)
#     Adding Nordbo adn Katul, 2013 high frequency wavelet spectra correction coefficients and flags to output
#   David Durden (2023-03-12)
#     updating terms, fixing ts period issue
#   David Durden (2023-05-12)
#     Adding cospectra to the output, updating term name in function
##############################################################################################


# start function def.vari.wave()


#function to determine the temporally resolved variance/covariance from CWT
#including high-frequency spectral correction and selectable low-frequency cutoff
def.spec.high.freq.cor <- function(
  # Wavelet coefficients variable
  varDwt,
  # Wavelet coefficients for vertical wind speed
  veloZaxsDwt,
  # Scales of discrete wavelet transform
  scal = sapply(varDwt@W, function(x) log(length(x), base = 2)),
  # Sampling Frequency in Hz
  FreqSamp = 20,
  # Initial parameterization for fitting model to find peak freq
  init = c(1, 1), 
  # Quality flag for missing data (>10%)
  qfWave
) {
  
  
# only process if < 10% NAs
if(qfWave == 0) {

  freq <- 2^scal * FreqSamp / 2^length(scal); names(freq) <- NULL
    
  varSpecPowr <- sapply(varDwt@W, function(x) mean(x^2) / log(2))
  veloZaxsSpecPowr <- sapply(veloZaxsDwt@W, function(x) mean(x^2) / log(2))
  
  names(varSpecPowr) <- NULL; names(veloZaxsSpecPowr) <- NULL
  
  # Find peak frequency of vertical wind speed using freq-weighted power spectra
  veloZaxsPowrWght <- freq * veloZaxsSpecPowr / FreqSamp / var(as.numeric(veloZaxsDwt@series))
  # varPowrWght <- freq * varSpecPowr / FreqSamp / var(as.numeric(varDwt@series))

  paraEst <- optim(
    par = init, 
    fn = function(para, freq, spec) mean((eddy4R.turb::def.spec.peak.modl(para, freq) - spec)^2), 
    freq = freq, spec = veloZaxsPowrWght, 
    method = "Nelder-Mead"
  )
  
  freqItpl <- 10^seq(log10(min(freq)), log10(max(freq)), length.out = 100)
  specItpl <- def.spec.peak.modl(para = paraEst$par, freq = freqItpl)
  
  idxFreqPeakItpl <- which.max(specItpl) # Index of peak frequency
  freqPeak <- freqItpl[idxFreqPeakItpl]
  
  # don't start evaluating Inertial Sub Range at peak, move one full scale higher
  idxFreqLim01 <- which(freq >= freqPeak) # First pick only frequency indices that are greater than the peak frequency
  idxFreqLim01 <- idxFreqLim01[abs(freq[idxFreqLim01] - freqPeak) == min(abs(freq[idxFreqLim01] - freqPeak))] # Then keep only the one closest to peak frequency
  idxFreqLim01 <- ifelse(idxFreqLim01 > ceiling(length(scal) / 2) - 1, ceiling(length(scal) / 2) - 1, idxFreqLim01) # If it is too low set it to default value of index 7 (of 15 scales)
  
  idxFreqLim02 <- idxFreqLim01 + 2 # Upper limit (i.e. lower frequencies) is two scales below idxFreqLim01

  # Only do processing is there are at least the 3 highest scales left to adjust
  if(freqPeak <= 1.25) {
    
    modlLin <- lm(log(varSpecPowr[seq(idxFreqLim01, idxFreqLim02)]) ~ log(freq[seq(idxFreqLim01, idxFreqLim02)]))
    
    # if the regression slope (power law coefficient) exceeds the bounds -1.8 ... -1.3, the conventional -5/3 slope is used as alternative
    modlLin$coefficients[1] <- log(varSpecPowr[idxFreqLim01]) - (modlLin$coefficients[2] * log(freq[idxFreqLim01]))
    qfWaveSlp <- 0
    
    if(!(modlLin$coefficients[2] > -1.8 & modlLin$coefficients[2] < -1.3)) {
      modlLin$coefficients[1] <- log(varSpecPowr[idxFreqLim01]) - (-5/3 * log(freq[idxFreqLim01]))
      modlLin$coefficients[2] <- -5/3
      qfWaveSlp <- 1
    }
    
    # calculate the reference spectral coefficients following the power slope
    specRefe <- exp(modlLin$coefficients[1] + modlLin$coefficients[2] * log(freq))
    
    waveSpecAmpl <- sqrt(varSpecPowr)
    
    # Get ratio of amplitudes for adjusted and original power spectra for 6 smallest scales, cannot be < 1
    rtioAmpl <- rep(1, length(freq))
    rtioAmpl[seq(idxFreqLim01 - 1, 1)] <- sqrt(specRefe[seq(idxFreqLim01 - 1, 1)]) / waveSpecAmpl[seq(idxFreqLim01 - 1, 1)]
    rtioAmpl[rtioAmpl < 1] <- 1 # No dampening following NK12
    names(rtioAmpl) <- names(varDwt@W)
    
    # Get lorenz curve results
    rptLorenz <- eddy4R.turb::def.wave.lorenz.calc(lapply(veloZaxsDwt@W, function(x) x^2))
    
    # Adjust only the most energetic Wavelet coefficients base on results from Lorenz curve (those that have 90% of the energy)
    waveScalAdjList <- lapply(
      names(varDwt@W),
      function(x) {
        
        idxEngy <- rptLorenz$index[[x]][rptLorenz$lorenz[[x]] > 0.1]
        idxNull <- rptLorenz$index[[x]][rptLorenz$lorenz[[x]] <= 0.1]
        
        waveScalAll <- mean(varDwt@W[[x]]^2) * length(rptLorenz$index[[x]])
        waveScalEngy <- mean(varDwt@W[[x]][idxEngy]^2) * length(idxEngy)
        waveScalNull <- mean(varDwt@W[[x]][idxNull]^2) * length(idxNull)
        
        engyScal <- sqrt((rtioAmpl[[x]]^2 * waveScalAll - waveScalNull) / waveScalEngy)
        
        waveScalAdj <- varDwt@W[[x]][idxEngy] * engyScal
        
        return(list(index = idxEngy, waveScalAdj = waveScalAdj))
        
      }
    )
    
    waveScalAdj <- varDwt
    for (i in seq(idxFreqLim01 - 1, 1)) {
      waveScalAdj@W[[i]][waveScalAdjList[[i]]$index] <- waveScalAdjList[[i]]$waveScalAdj
    }
    
    invDwt01 <- wavelets::idwt(waveScalAdj) # Time series reconstruction
    
    covOrig <- cov(veloZaxsDwt@series, varDwt@series)
    covAdj <- cov(veloZaxsDwt@series, invDwt01)
    
    fluxMiss <- covOrig / covAdj # Flux attenuation for half-hour period as a percentage
    
    coefCor <- 1 / fluxMiss # Correction coefficient is inverse of flux attenuation
    # coefCor <- ifelse(coefCor < 1, 1, coefCor) # Correction coefficient can't be < 1.0 (i.e., cant make attenuation worse)
    
    # Calculate and report cospectra, can be converted to frequency-weighted cospectra with other output variables
    cosp <- sapply(names(varDwt@W), 
                   function(x) { 
                     mean(varDwt@W[[x]] * veloZaxsDwt@W[[x]]) / log(2) 
                   })
    
    
    # prepare outputs
    rpt <- base::list()
    
    # peak frequency
    rpt$freqPeak <- freqPeak
    
    #Output the cospectra
    rpt$cosp <- cosp
    
    # uncorrected covariance
    rpt$mean <- covOrig
    
    # corrected covariance
    rpt$cor <- covAdj
    
    # correction coefficient (i.e., rpt$cor / rpt$mean)
    rpt$coefCor <- coefCor
    
    # flag
    rpt$qfWave <- qfWave
    
    #Slope of the wavelet high frequency correction
    rpt$slpRegWave <- modlLin$coefficients[2]
    
    #Slope flag for high frequency correction if outside 1.3 - 1.8 bounds
    rpt$qfWaveSlp <- qfWaveSlp
    
    # in case peak frequency > 1.25 Hz
  } else {
    
    # prepare outputs
    rpt <- base::list(
      freqPeak = freqPeak,
      cosp = NA,
      mean = covOrig,
      cor = NA,
      coefCor = 1,
      qfWave = qfWave,
      slpRegWave = NA,
      qfWaveSlp = -1)
    
  }
  
  # no Wavelet processing if > 10% NAs
} else {
  
  # prepare outputs
  rpt <- base::list(
    freqPeak = NA,
    cosp = NA,
    mean = NA,
    cor = NA,
    coefCor = 1,
    qfWave = qfWave,
    slpRegWave <- NA,
    qfWaveSlp <- -1
  )
  
}
  
  # return results
  return(rpt)

  
  # # some testing
  # rng <- range(c(sqrt(waveVari), sqrt(dfInp$veloZaxsHor^2)))
  # plot(sqrt(waveVari) ~ sqrt(dfInp$w_met^2), xlim = rng, ylim = rng, asp=1)
  # lines(sqrt(waveVari), col=2)

  #plot change in variance
  #between 0% and 10% along flight line for H2O
  #between 0% and 1% along flight line for T
  #plot(I((waveVari / myvc3), log="y")

}