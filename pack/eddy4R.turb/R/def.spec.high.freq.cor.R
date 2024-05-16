##############################################################################################
#' @title Definition function: function to determine the temporally resolved covariance from discrete wavelet transform and frequency response correction

#' @author
#' David Durden \email{ddurden@battelleecology.org}
#' Stefan Metzger

#' @description 
#' Definition function. Function to determine the temporally resolved variance/covariance from continuous wavelet transform including high-frequency spectral correction and selectable low-frequency cutoff. The frequency response correction using Wavelet techniques described in Norbo and Katul, 2012 (NK12)

#' @param sclrDwt wavelets object output, discrete wavelet transform output object for a given scalar/variable (e.g., rtioMoleDryH2o)
#' @param veloZaxsDwt wavelets object output, discrete wavelet transform output object for a vertical wind speed (i.e., w')
#' @param sclrCnst scaling constant for scalar and vertical wind speed, needed to account for rounding in 'wavelets' package
#' @param scal vector of scales for wavelets objects
#' @param FreqSamp numeric, sampling frequency defaults to 20Hz
#' @param init initialization parameters for fitting two-parameter nonlinear model to frequency-weighted vertical wind speed data. Used to determine spectral peak.
#' @param idxData index of time series data to use and modify. Only needed if zero-padding is being done. Currently just defaults to every observation in dataset.
#' @param qfWave Wavelet flag: process (0) or not (1) depending on if number of data points missing >10%
#' 
#' @return A list containing output from NK12 high frequency correction, including a correction coefficient (coefCor) providing a proportion of how much to adjust 30-minute flux values.
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
#   Adam Young (2024-03-17)
#     Initial commit of renaming def.wave.vari.R -> def.spec.high.freq.cor.R
#   Adam Young (2024-05-07)
#     Finalizing script for broader testing prior to data reprocessing
##############################################################################################


# start function def.spec.high.freq.cor()


#function to determine the temporally resolved variance/covariance from discrete wavelet transform
#including high-frequency spectral correction 
def.spec.high.freq.cor <- function(
  # Discrete wavelet coefficients for specific environmental variable (e.g., temp, rtioMoleDryCo2)
  sclrDwt,
  # Discrete wavelet coefficients for vertical wind speed
  veloZaxsDwt,
  # Scaling constant for scalar and vertical wind speed, needed to account for rounding in 'wavelets' package
  sclrCnst = c(1, 1),
  # Scales of discrete wavelet transform
  scal = sapply(sclrDwt@W, function(x) log(length(x), base = 2)),
  # Sampling Frequency in Hz
  FreqSamp = 20,
  # Initial parameterization for fitting model to find peak freq
  init = c(3, 5), 
  # Data index for original time series, assumes no zero padding
  idxData = seq(1, length(sclrDwt@series)),
  # Quality flag for missing data (>10%)
  qfWave
) {
  
  
# only process if < 10% NAs
if(qfWave == 0) {
  
  freq <- 2^scal * FreqSamp / 2^length(scal); names(freq) <- NULL # Eq. 15 in NK12
    
  # Unweighted spectra power (modification of Eq. 7 in NK12)
  sclrSpecPowr <- sapply(sclrDwt@W, function(x) mean(x^2) / log(2))
  veloZaxsSpecPowr <- sapply(veloZaxsDwt@W, function(x) mean(x^2) / log(2))
  names(varSpecPowr) <- NULL; names(veloZaxsSpecPowr) <- NULL
  
  # Find peak frequency of vertical wind speed using freq-weighted power spectra (Eq. 7 in NK12)
  veloZaxsPowrWght <- freq * veloZaxsSpecPowr / FreqSamp / var(as.numeric(veloZaxsDwt@series))
  
  paraEst <- optim(
    par = init, 
    fn = function(para, freq, spec) mean((eddy4R.turb::def.spec.peak.modl(para, freq) - spec)^2), 
    freq = freq, spec = veloZaxsPowrWght, 
    method = "Nelder-Mead"
  )
  
  # Interpolated 100-point function of freq-weighted vertical wind speed power spectra
  freqItpl <- exp(seq(log(min(freq)), log(max(freq)), length.out = 100))
  specItpl <- def.spec.peak.modl(para = paraEst$par, freq = freqItpl)
  
  idxFreqPeakItpl <- which.max(specItpl) # Index of peak frequency
  freqPeak <- freqItpl[idxFreqPeakItpl] # Value of peak frequency
  
  # don't start evaluating Inertial Sub Range at peak, move one full scale higher
  idxFreqLim01 <- which(freq >= freqPeak) # First pick only frequency indices that are greater than the peak frequency
  idxFreqLim01 <- idxFreqLim01[abs(freq[idxFreqLim01] - freqPeak) == min(abs(freq[idxFreqLim01] - freqPeak))] # Then keep only the one closest to peak frequency
  idxFreqLim01 <- ifelse(idxFreqLim01 > ceiling(length(scal) / 2) - 1, ceiling(length(scal) / 2) - 1, idxFreqLim01) # If it is too low set it to default value of index 7 (of 15 scales)
  
  idxFreqLim02 <- idxFreqLim01 + 2 # Upper limit (i.e. lower frequencies) is two scales below idxFreqLim01

  # Only do processing is there are at least the 3 highest scales left to adjust. Also do not do any processing if frequency peak is too low (<0.05 Hz). 
  # So only 3-7 highest scales can be adjusted.
  if(freqPeak >= 0.05 & freqPeak <= 1.25) {
    
    # Regression on spectra power in inertial subrange
    modlLin <- lm(log(sclrSpecPowr[seq(idxFreqLim01, idxFreqLim02)]) ~ log(freq[seq(idxFreqLim01, idxFreqLim02)]))
    slpRegWave <- modlLin$coefficients[2] # Record original slope prior to evaluating range tolerance. For reporting purposes.
    
    # if the regression slope (power law coefficient) exceeds the bounds -1.8 ... -1.3, the conventional -5/3 slope is used as alternative
    modlLin$coefficients[1] <- log(varSpecPowr[idxFreqLim01]) - (modlLin$coefficients[2] * log(freq[idxFreqLim01]))
    qfWaveSlp <- 0
    
    if(!(modlLin$coefficients[2] > -1.8 & modlLin$coefficients[2] < -1.3)) {
      modlLin$coefficients[1] <- log(sclrSpecPowr[idxFreqLim01]) - (-5/3 * log(freq[idxFreqLim01]))
      modlLin$coefficients[2] <- -5/3
      qfWaveSlp <- 1
    }
    
    # calculate the reference spectral coefficients following the power slope
    specRefe <- exp(modlLin$coefficients[1] + modlLin$coefficients[2] * log(freq))
    
    waveSpecAmpl <- sqrt(sclrSpecPowr)
    
    # Get ratio of amplitudes for adjusted and original power spectra for 6 smallest scales, cannot be < 1
    rtioAmpl <- rep(1, length(freq))
    rtioAmpl[seq(idxFreqLim01 - 1, 1)] <- sqrt(specRefe[seq(idxFreqLim01 - 1, 1)]) / waveSpecAmpl[seq(idxFreqLim01 - 1, 1)]
    rtioAmpl[rtioAmpl < 1] <- 1 # No dampening following NK12
    names(rtioAmpl) <- names(sclrDwt@W)
    
    # Get lorenz curve results
    rptLorenzWaveCoef <- eddy4R.turb::def.wave.lorenz.calc(lapply(veloZaxsDwt@W, function(x) x^2))

    # Adjust only the most energetic Wavelet coefficients base on results from Lorenz curve (those that have 90% of the energy)
    waveCoefAdjList <- lapply(
      names(sclrDwt@W),
      function(x) {
        
        idxEngy <- rptLorenzWaveCoef$index[[x]][rptLorenzWaveCoef$lorenz[[x]] > 0.1]
        idxNull <- rptLorenzWaveCoef$index[[x]][rptLorenzWaveCoef$lorenz[[x]] <= 0.1]
        
        waveScalAll <- mean(sclrDwt@W[[x]]^2) * length(rptLorenzWaveCoef$index[[x]])
        waveScalEngy <- mean(sclrDwt@W[[x]][idxEngy]^2) * length(idxEngy)
        waveScalNull <- mean(sclrDwt@W[[x]][idxNull]^2) * length(idxNull)
        
        engyScal <- sqrt((rtioAmpl[[x]]^2 * waveScalAll - waveScalNull) / waveScalEngy)
        
        waveCoefAdj <- sclrDwt@W[[x]][idxEngy] * engyScal
        
        return(list(index = idxEngy, waveAdj = waveCoefAdj))
        
      }
    )

    # Adjust only wavelet coefficients with highest energy    
    sclrDwtAdj <- sclrDwt
    for (idxScal in seq(idxFreqLim01 - 1, 1)) {
      sclrDwtAdj@W[[idxScal]][waveCoefAdjList[[idxScal]]$index] <- waveCoefAdjList[[idxScal]]$waveAdj
    }
    
    # Apply scaling constant to wavelet coefficients before time series reconstruction, wavelets::idwt() is where the troublesome rounding occurs
    for (idxScal in seq(1, length(sclrDwtAdj@W))) {
      sclrDwtAdj@W[[idxScal]] <- sclrCnst * sclrDwtAdj@W[[idxScal]]
    }
    
    # Do time series reconstruction with 
    invSclrDwt01 <- wavelets::idwt(sclrDwtAdj) # Time series reconstruction
    invSclrDwt01 <- invSclrDwt01 / sclrCnst # Re-scale back to original
    
    covOrig <- cov(veloZaxsDwt@series[idxData], sclrDwt@series[idxData])
    covAdj <- cov(veloZaxsDwt@series[idxData], invSclrDwt01[idxData])
    
    fluxMiss <- covOrig / covAdj # Flux attenuation for half-hour period as a proportion
    
    coefCor <- 1 / fluxMiss # Correction coefficient is inverse of flux attenuation
    # coefCor <- ifelse(coefCor < 1, 1, coefCor) # Correction coefficient can't be < 1.0 (i.e., cant make attenuation worse)
    
    cospPowrWght    <- freq * sapply(names(sclrDwt@W), 
                                     function(x) { 
                                       mean(sclrDwt@W[[x]] * veloZaxsDwt@W[[x]]) / log(2) / FreqSamp / covOrig
                                     })
    
    cospPowrWghtCor <- freq * sapply(names(sclrDwtAdj@W), 
                                     function(x) { 
                                       mean(sclrDwtAdj@W[[x]] * veloZaxsDwt@W[[x]]) / log(2) / FreqSamp / covAdj 
                                     })
    
    # prepare outputs
    rpt <- base::list()
    
    # frequency vector 
    rpt$freq <- freq
    
    # peak frequency of vertical wind speed
    rpt$freqPeak <- freqPeak
    
    #Output the attenuated cospectra
    rpt$cosp <- cospPowrWght
    
    #Output the corrected cospectra
    rpt$cospCor <- cospPowrWghtCor
    
    # uncorrected covariance
    rpt$mean <- covOrig
    
    # corrected covariance
    rpt$cor <- covAdj
    
    # correction coefficient, inverse of flux attenuation
    rpt$coefCor <- coefCor
    
    # flag
    rpt$qfWave <- qfWave
    
    rpt$modlLinScal <- seq(idxFreqLim01, idxFreqLim02)
    
    #Slope of the original unmodified wavelet high frequency correction
    rpt$slpRegWave <- slpRegWave
    
    #Slope flag for high frequency correction if outside 1.3 - 1.8 bounds
    rpt$qfWaveSlp <- qfWaveSlp
    
    # in case peak frequency is not in range of [0.05Hz, 1.25 Hz]
  } else {
    
    # prepare outputs
    rpt <- base::list(
      freq = NA, 
      freqPeak = freqPeak,
      cosp = NA,
      cospCor = NA,
      mean = cov(veloZaxsDwt@series, sclrDwt@series),
      cor = NA,
      coefCor = 1,
      qfWave = qfWave,
      modlLinScal = NA, 
      slpRegWave = NA,
      qfWaveSlp = -1)
    
  }
  
  # no Wavelet processing if > 10% NAs
} else {
  
  # prepare outputs
  rpt <- base::list(
    freq = NA, 
    freqPeak = NA,
    cosp = NA,
    cospCor = NA,
    mean = NA,
    cor = NA,
    coefCor = 1,
    qfWave = qfWave,
    modlLinScal = NA,
    slpRegWave = NA,
    qfWaveSlp = -1
  )
  
}
  
  # return results
  return(rpt)

}
