##############################################################################################
#' @title Definition function: function to determine the temporally resolved covariance from discrete wavelet transform and frequency response correction

#' @author
#' David Durden \email{ddurden@battelleecology.org}
#' Stefan Metzger

#' @description 
#' Definition function. Function to determine the temporally resolved variance/covariance from discrete wavelet transform and frequency response correction described in Nordbo and Katul (2012)

#' @param sclrDwt wavelets object output, discrete wavelet transform output object for a given scalar/variable (e.g., rtioMoleDryH2o)
#' @param veloZaxsDwt wavelets object output, discrete wavelet transform output object for a vertical wind speed (i.e., w')
#' @param sclrSpecPowr vector of spectral power estimates calculated for sclrDwt, derived from wavelet coefficients from dwt.R output following Eq. 7 in NK12
#' @param FreqSamp Sampling frequency, defaults to 20 Hz
#' @param freq vector of frequencies for wavelet spectral results
#' @param freqPeak vector (length = 2) for peak frequency values of scalar variable and vertical wind speed variable, respectively
#' @param idxFreqPeak vector (length = 2) for index from 'freq' vector for peak frequency values of both scalar variable and vertical wind speed variable, respectively
#' @param sclrCnst scaling constant in scientific notation (e.g., 1e06, 1e-04), needed to account for rounding in idwt.R in 'wavelets' package
#' @param idxData index of time series data to use and modify. Only needed if zero-padding is being done. Currently just defaults to every observation in dataset.
#' @param qfWave Wavelet flag: process (0) or not (1) depending on if number of data points missing >10%
#' 
#' @return A list containing output from NK12 high frequency correction, including a correction coefficients for use in flux data (coefCor)
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
#   Adam Young (2024-05-21) 
#     Moved peak finding to its own function, set up to output cospectra results and reconstructed time series.
##############################################################################################


# start function def.spec.high.freq.cor()


#function to determine the temporally resolved variance/covariance from discrete wavelet transform
#including high-frequency spectral correction 
def.spec.high.freq.cor <- function(
  # Discrete wavelet coefficients for specific environmental variable (e.g., temp, rtioMoleDryCo2)
  sclrDwt,
  # Discrete wavelet coefficients for vertical wind speed
  veloZaxsDwt,
  # vector of spectra power estimates for a specific variable
  sclrSpecPowr,  
  # Sampling frequency
  FreqSamp = veloZaxsDwt@attr.X$tsp[3], # Sampling frequency in time series object stored in wavelets output
  # Frequency vector of discrete wavelet transform
  freq = 2^(seq(veloZaxsDwt@level - 1, 0)) * FreqSamp / 2^veloZaxsDwt@level, # Can calculate frequency from wavelets output, 'level' is the number of scales
  # Two-element vector of peak frequency values for both scalar variable (element 1) and vertical wind speed (element 2)
  freqPeak = rep(10, 2),
  # Two-element vector of indices for peak frequency for both scalar variable (element 1) and vertical wind speed (element 2)
  idxFreqPeak = rep(ceiling(length(freq) / 2), 2),
  # Scaling constant for scalar and vertical wind speed, needed to account for rounding in 'wavelets' package
  sclrCnst = 1.0,
  # Data index for original time series, assumes no zero padding
  idxData = seq(1, length(sclrDwt@series)),
  # Quality flag for missing data (>10%)
  qfWave
) {
  
  
# only process if < 10% NAs
if(qfWave == 0) {
  
  # Covariance for the original "unadjusted" time series between the scalar 
  # and vertical wind speed
  covRaw <- cov(sclrDwt@series[idxData], veloZaxsDwt@series[idxData])
  
  # Cospectra power calculations for unadjusted/original time series data
  # using output from 'wavelets' package. Eq. 11 in NK12
  cospPowr <- sapply(names(sclrDwt@W), 
                     function(x) { 
                       mean(sclrDwt@W[[x]] * veloZaxsDwt@W[[x]]) / log(2)
                     })
  
  cospPowrNorm <- freq * cospPowr / FreqSamp / covRaw # Normalize so integrates to 1.0
  
  
  # Index and peak frequency values derived from eddy4R.turb::def.spec.peak.R
  idxSclrFreqPeak <- idxFreqPeak[1] # scalar variable
  idxVeloZaxsHorFreqPeak <- idxFreqPeak[2] # vertical wind speed
  freqPeak <- freqPeak[2] # actual frequency where peak vertical wind speed spectra occurs at
  
  # 'freq' vector is ordered reverse based on results from wavelets package, so 
  # high index values indicate lower frequency values. If index is too high
  # that means peak frequency was found under lower frequencies likely outside 
  # inertial subrange. Limit this to half of max frequency on log scale.
  idxFreqLim01 <- idxVeloZaxsHorFreqPeak
  idxFreqLim01 <- ifelse(idxFreqLim01 > ceiling(length(freq) / 2), ceiling(length(freq) / 2), idxFreqLim01) # Leaves highest 7 scales available for adjustment 
  
  # If peak frequency from veloZaxsHor is at lowest scales then just make sure 
  # there is vector to pass on a a failsafe, will be excluded from high-freq 
  # correction algorithm because freqPeak not in the right range
  idxFreqLim02 <- ifelse(idxFreqLim01 < length(freq) - 1, idxFreqLim01 + 2, idxFreqLim01) # Only use 3 scales for regression anlaysis based on NK12 methods
  idxFreqLims <- seq(idxFreqLim01, idxFreqLim02)
  
  # The following conditional statement is implementing approach
  # from MATLAB code that accompanies NK12: inertial subrange of scalar variable
  # cannot contain peak frequency of that scalar.
  idxFreqLims <- idxFreqLims[freq[idxFreqLims] > freq[idxSclrFreqPeak]]
  
  # Only do processing is there are at least the 3 highest scales left to adjust. 
  # Also do not do any processing if frequency peak is too low (<0.05 Hz). So 
  # only 3-7 highest scales can be adjusted. If it is outside this range do not
  # do high frequency correction.
  if(freqPeak >= 0.05 & freqPeak <= 1.25) {
    
    if (length(idxFreqLims) == 3) {
      
      # Regression on spectra power in inertial subrange
      modlLin <- lm(log(sclrSpecPowr[idxFreqLims]) ~ log(freq[idxFreqLims]))
      slpRegWave <- modlLin$coefficients[2] # Record original slope prior to evaluating range tolerance. For reporting purposes.
      
      # if the regression slope (power law coefficient) exceeds the bounds -1.8 ... -1.3, the conventional -5/3 slope is used as alternative
      modlLin$coefficients[1] <- log(sclrSpecPowr[idxFreqLim01]) - (modlLin$coefficients[2] * log(freq[idxFreqLim01]))
      qfWaveSlp <- 0
      
      if(!(modlLin$coefficients[2] > -1.8 & modlLin$coefficients[2] < -1.3)) {
        
        modlLin$coefficients[1] <- log(sclrSpecPowr[idxFreqLim01]) - (-5/3 * log(freq[idxFreqLim01]))
        modlLin$coefficients[2] <- -5/3
        qfWaveSlp <- 1
        
      }      
      
    } else if (length(idxFreqLims) < 3) {
      
      modlLin <- list()
      slpRegWave <- NA
      
      modlLin$coefficients[1] <- log(sclrSpecPowr[idxFreqLim01]) - (-5/3 * log(freq[idxFreqLim01]))
      modlLin$coefficients[2] <- -5/3
      qfWaveSlp <- 1
      
    }
    
    # calculate the reference spectral coefficients following the power slope
    specRefe <- exp(modlLin$coefficients[1] + modlLin$coefficients[2] * log(freq))
    
    waveSpecAmpl <- sqrt(sclrSpecPowr) # Amplitude of spectra
    
    # Get ratio of amplitudes for adjusted and original power spectra for 6 
    # smallest scales, cannot be < 1
    rtioAmpl <- rep(1, length(freq))
    rtioAmpl[seq(idxFreqLim01 - 1, 1)] <- sqrt(specRefe[seq(idxFreqLim01 - 1, 1)]) / waveSpecAmpl[seq(idxFreqLim01 - 1, 1)]
    rtioAmpl[rtioAmpl < 1] <- 1 # No dampening following NK12
    names(rtioAmpl) <- names(sclrDwt@W)
    
    # Get lorenz curve results
    rptLorenzWaveCoef <- eddy4R.turb::def.wave.lorenz.calc(lapply(veloZaxsDwt@W, function(x) x^2))

    # Adjust only the most energetic Wavelet coefficients base on results from 
    # Lorenz curve (those that have 90% of the energy)
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
    
    # Apply scaling constant to wavelet coefficients before time series 
    # reconstruction, wavelets::idwt() is where the troublesome rounding occurs
    for (idxScal in seq(1, length(sclrDwtAdj@W))) {
      sclrDwtAdj@W[[idxScal]] <- sclrCnst * sclrDwtAdj@W[[idxScal]]
    }
    
    # Do time series reconstruction with wavelets::idwt.R
    invSclrDwt01 <- wavelets::idwt(sclrDwtAdj) # Time series reconstruction
    
    # Re-scale everything back to original now that time series reconstruction is 
    # finished
    invSclrDwt01 <- invSclrDwt01 / sclrCnst 
    
    # Rescale wavelet coefficients back to original as well, needed for
    # calculating corrected cospectra for output
    for (idxScal in seq(1, length(sclrDwtAdj@W))) {
      sclrDwtAdj@W[[idxScal]] <- sclrDwtAdj@W[[idxScal]] / sclrCnst
    }
    
    # Calculate covariances between scalar and vertical wind speed for adjusted wavelet coefficients
    covCor <- cov(invSclrDwt01[idxData], veloZaxsDwt@series[idxData])
    
    fluxMiss <- covRaw / covCor # Flux attenuation for half-hour period as a proportion
    
    coefCor <- 1 / fluxMiss # Correction coefficient is inverse of flux attenuation
    # coefCor <- ifelse(coefCor < 1, 1, coefCor) # Correction coefficient can't be < 1.0 (i.e., cant make attenuation worse)
    
    # Calculate cospectra power values for corrected/adjusted time series (Eq. 7 in NK12)
    cospPowrCor <- sapply(names(sclrDwtAdj@W), 
                       function(x) { 
                         mean(sclrDwtAdj@W[[x]] * veloZaxsDwt@W[[x]]) / log(2)
                       })
    
    # Normalized corrected cospectra
    cospPowrNormCor <- freq * cospPowrCor / FreqSamp / covCor # (Eq. 11 in NK12)
    
    
    # prepare outputs
    rpt <- base::list()
    
    # Corrected time series, could be used to recalculate corrected wavelet coefficients and spectra
    rpt$dataCor <- ts(invSclrDwt01, start = 0, frequency = FreqSamp)
    
    # uncorrected covariance
    rpt$covRaw <- covRaw
    
    # corrected covariance
    rpt$covCor <- covCor
    
    #Output the attenuated cospectra
    rpt$cospPowr <- cospPowr
    
    # Output the attenuated cospectra (normalized)
    rpt$cospPowrNorm <- cospPowrNorm
    
    #Output the corrected cospectra
    rpt$cospPowrCor <- cospPowrCor
    
    #Output the corrected cospectra (normalized)
    rpt$cospPowrNormCor <- cospPowrNormCor
    
    # correction coefficient (covCor / covRaw), inverse of flux attenuation
    rpt$coefCor <- coefCor
    
    # Indices that identify the inertial sub-range used in regression.
    # NA's indicate scales where scalar peak frequency overlaps with inertial 
    # sub-range identified from vertical wind peak frequency
    rpt$modlLinScal <- c(rep(NA, 3 - length(idxFreqLims)), idxFreqLims)
    
    # Slope of the original unmodified wavelet high frequency correction. 
    # NA if < 3 points available for fit
    rpt$slpRegWave <- slpRegWave
    
    #Slope flag for high frequency correction if outside 1.3 - 1.8 bounds
    rpt$qfWaveSlp <- qfWaveSlp
    
  } else { # in case peak frequency is not in range of [0.05Hz, 1.25 Hz]
    
    # prepare outputs
    rpt <- base::list(
      dataCor = NA,
      covRaw = covRaw,
      covCor = NA,
      cospPowr = cospPowr,
      cospPowrNorm = cospPowrNorm,
      cospPowrCor = NA,
      cospPowrNormCor = NA,
      coefCor = 1,
      modlLinScal = NA, 
      slpRegWave = NA,
      qfWaveSlp = -1)
    
  }
  
} else { # no Wavelet processing if > 10% NAs
  
  # prepare outputs
  rpt <- base::list(
    dataCor = NA,
    covRaw = NA,
    covCor = NA,
    cospPowr = NA,
    cospPowrNorm = NA,
    cospPowrCor = NA,
    cospPowrNormCor = NA,
    coefCor = 1,
    modlLinScal = NA, 
    slpRegWave = NA,
    qfWaveSlp = -1)
  
}
  
  # return results
  return(rpt)

}
