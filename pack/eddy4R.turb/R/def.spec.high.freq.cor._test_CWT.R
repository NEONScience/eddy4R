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
def.spec.high.freq.cor._test_CWT <- function(
  # Wavelet coefficients variable
  spec01,
  # Wavelet coefficients for vertical wind speed
  spec02,
  # Scales of discrete wavelet transform
  prd,
  dj = 1/8,
  # scal = sapply(varDwt@W, function(x) log(length(x), base = 2)),
  # Sampling Frequency in Hz
  FreqSamp = 20,
  # Initial parameterization for fitting model to find peak freq
  init = c(1, 1), 
  # Quality flag for missing data (>10%)
  qfWave,
  #        spec02 = mycwt[[FREQ_0_map[[vari]][2]]]@spectrum,

  #half-power frequencies for individual variables [Hz]
  # FreqCut = NA,
  #which wavelengths/spatial scales to consider
  # SetPrd = NULL,
  #        SetPrd = whr_peri_20,
  #normalization coefficient specific to the choice of Wavelet parameters
  # CoefNorm = rpt$coefNorm,
  # Wavelet flag: process (0) or not (1)
  #stability parameter
  paraStbl,
  #spectrum or cospectrum?
  MethSpec = c("spec", "cosp")[2]
) {
  
  
# only process if < 10% NAs
if(qfWave == 0) {
  
  if(base::is.null(spec02)) {
    #un-weighted wavelet scalogram
    #two approaches identical, see Mauder et al. (2008) and Stull (1988, Sect. 8.6.2 and 8.8.2)
    # waveScal<- base::abs(spec01)^2
    #waveScal<- Re(spec01 * Conj(spec01))

  } else {

    #un-weighted wavelet cross-scalogram
    #two approaches identical, see Mauder et al. (2008) and Stull (1988, Sect. 8.6.2 and 8.8.2)
    waveScal <- abs(spec01@spectrum)^2 #base::Re(spec01 * base::Conj(spec02))
    #waveScal<- Re(spec01) * Re(spec02) + Im(spec01) * Im(spec02)

    # Also output spectral power for vertical wind speed (veloZaxsHor)
    veloZaxsScal <- abs(spec02@spectrum)^2

  }

  # frequency [Hz]
  freq <- 1 / prd

  # Normalized power spectra (both unweighted and weighted) for vertical wind speed (veloZaxs) and scalar
  veloZaxsSpec <- colSums(veloZaxsScal) / sum(veloZaxsScal)

  # Frequency-weighted spectra for vert wind speed needed to find peak
  veloZaxsSpecWght <- do.call(rbind, lapply(1:nrow(veloZaxsScal), function(x) freq * veloZaxsScal[x,]))
  veloZaxsSpecWght <- colSums(veloZaxsSpecWght) / sum(veloZaxsSpecWght)

  # Corrections evaluated on un-weighted spectra data for given variable
  waveSpec <- colSums(waveScal) / sum(waveScal)
  waveSpecAmpl <- sqrt(waveSpec) # Amplitude, see NK12 paper

  # Find frequency that peak veloZaxsSpec occurs through optimization
  paraEst <- optim(
    par = init,
    fn = function(para, freq, spec) mean((def.spec.peak.modl(para, freq) - spec)^2), #eddy4R.turb::
    freq = freq, spec = veloZaxsSpecWght,
    method = "Nelder-Mead"
  )

  # Get spec model results for interpolated frequency values to generate smooth curve
  freqItpl <- exp(seq(log(min(freq)), log(max(freq)), length.out = 200))
  specItpl <- def.spec.peak.modl(para = paraEst$par, freq = freqItpl)

  idxFreqPeakItpl <- which.max(specItpl) # Index of peak frequency for interpolated data
  idxFreqPeak <- which.min(abs(freqItpl[idxFreqPeakItpl] - freq)) # Index for peak frequency
  freqPeak <- freq[idxFreqPeak] # Peak frequency

  idxFreqLim01 <- idxFreqPeak - 1 / dj # don't start evaluating Inertial Sub Range at peak, move one full scale higher (minus sign is due to order or freq values)
  # Evaluation of attenuation in inertial sub range max freq limits are set to 1 Hz (could adjust lower to 0.5)
  idxFreqLim02 <- which.max(freq <= 1) # Inertial subrange can't occur at frequencies higher than 1 Hz

    # only continue if peak frequency < 1 Hz and there are at least 1/dj points to do regression on
    if(freqPeak < 1 & length(seq(idxFreqLim01, idxFreqLim02)) >= 1/dj) {

      # for overview page 18 of http://use-r-carlvogt.github.io/PDFs/2017Avril_Cantoni_Rlunch.pdf
      modlLin <- lm(log(veloZaxsSpec[seq(idxFreqLim01, idxFreqLim02)]) ~ log(freq[seq(idxFreqLim01, idxFreqLim02)]))
  
      # if the regression slope (power law coefficient) exceeds the bounds -1.8 ... -1.3, the conventional -5/3 slope is used as alternative
      slpRegWave <- modlLin$coefficients[2]
      qfWaveSlp <- 0
      if(!(modlLin$coefficients[2] > -1.8 & modlLin$coefficients[2] < -1.3)) {
        modlLin$coefficients[1] <- base::mean(log(waveSpec[seq(idxFreqLim01, idxFreqLim02)]) - (-5/3 * log(freq[seq(idxFreqLim01, idxFreqLim02)])), na.rm = TRUE)
        modlLin$coefficients[2] <- -5/3
        qfWaveSlp <- 1
      }
  
      # calculate the reference spectral coefficients following the power slope
      specRefe <- exp(modlLin$coefficients[1] + modlLin$coefficients[2] * log(freq))
      
      waveSpecAmpl <- sqrt(veloZaxsSpec)
      
      # Get ratio of amplitudes for adjusted and original power spectra for 6 smallest scales, cannot be < 1
      rtioAmpl <- rep(1, length(freq))
      rtioAmpl[seq(idxFreqLim02, 1)] <- sqrt(specRefe[seq(idxFreqLim02, 1)]) / waveSpecAmpl[seq(idxFreqLim02, 1)]
      rtioAmpl[rtioAmpl < 1] <- 1 # No dampening following NK12
      # names(rtioAmpl) <- names(varDwt@W)
      
      # Get lorenz curve results
      rptLorenz <- def.wave.lorenz.calc._test_CWT(Re(spec02@spectrum)^2) # eddy4R.turb::
      
      # Adjust only the most energetic Wavelet coefficients base on results from Lorenz curve (those that have 90% of the energy)
      waveScalAdjList <- lapply(
        seq(1, ncol(spec01@spectrum)),
        function(x) {
          
          idxEngy <- rptLorenz$index[rptLorenz$lorenz[, x] > 0.1, x]
          idxNull <- rptLorenz$index[rptLorenz$lorenz[, x] <= 0.1, x]
          
          waveScalAll <- mean(spec01@spectrum[, x]^2) * length(rptLorenz$index[, x])
          waveScalEngy <- mean(spec01@spectrum[idxEngy, x]^2) * length(idxEngy)
          waveScalNull <- mean(spec01@spectrum[idxNull, x]^2) * length(idxNull)
          
          engyScal <- sqrt(rtioAmpl[x]^2 * (waveScalAll - waveScalNull) / waveScalEngy)
      
          waveScalAdj <- spec01@spectrum[idxEngy, x] * engyScal
          
          return(list(index = idxEngy, waveScalAdj = waveScalAdj))
          
        }
      )
      
      waveScalAdj <- spec01
      for (i in seq(idxFreqLim02 - 1, 1)) {
        waveScalAdj@spectrum[waveScalAdjList[[i]]$index] <- waveScalAdjList[[i]]$waveScalAdj
      }
      
      invCwt01 <- Waves::recon.cwt(waveScalAdj) # Time series reconstruction
      
      covOrig <- cov(spec02@series, spec01@series)
      covAdj <- cov(spec02@series, invCwt01)
      
      fluxMiss <- covOrig / covAdj # Flux attenuation for half-hour period as a percentage
      
      coefCor <- 1 / fluxMiss # Correction coefficient is inverse of flux attenuation
      coefCor <- ifelse(coefCor < 1, 1, coefCor) # Correction coefficient can't be < 1.0 (i.e., cant make attenuation worse)
      
      # Calculate and report cospectra, can be converted to frequency-weighted cospectra with other output variables
      cosp <- Re(spec01@spectrum * Conj(spec02@spectrum))
      cosp <- colSums(cosp) / sum(cosp)
                     
    
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
      rpt$slpRegWave <- slpRegWave
      
      #Slope flag for high frequency correction if outside 1.3 - 1.8 bounds
      rpt$qfWaveSlp <- qfWaveSlp

  # in case peak frequency > 1 Hz
  } else {
    
    # prepare outputs
    rpt <- base::list(
      freqPeak = freqPeak,
      cosp = NA,
      mean = covOrig,
      cor = NA,
      coefCor = 1,
      qfWave = 1,
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
    qfWaveSlp <- -1,
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
