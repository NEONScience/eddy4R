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
#   David Durden (2023-03-12)
#     updating terms, fixing ts period issue
##############################################################################################


# start function def.vari.wave()


#function to determine the temporally resolved variance/covariance from CWT
#including high-frequency spectral correction and selectable low-frequency cutoff
def.wave.vari <- function(
  #complex Wavelet coefficients variable 1
  spec01 = rpt$wave[["veloZaxsHor"]]@spectrum,
  #complex Wavelet coefficients variable 2
  spec02 = NULL,
  #        spec02 = mycwt[[FREQ_0_map[[vari]][2]]]@spectrum,
  #width of the wavelet at each scale [s]
  scal = rpt$wave[[var]]@scale,
  #approximate corresponding Fourier period [s]
  prd = rpt$wave[[var]]@period,
  #Sampling frequency (Defaults to 20 Hz)
  FreqSamp = 20,
  #half-power frequencies for individual variables [Hz]
  FreqCut = NA,
  #which wavelengths/spatial scales to consider
  SetPrd = NULL,
  #        SetPrd = whr_peri_20,
  #normalization coefficient specific to the choice of Wavelet parameters
  CoefNorm = rpt$coefNorm,
  # Wavelet flag: process (0) or not (1)
  qfWave,
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
    waveScal<- base::abs(spec01)^2
    #waveScal<- Re(spec01 * Conj(spec01))
    
  } else {
    
    #un-weighted wavelet cross-scalogram
    #two approaches identical, see Mauder et al. (2008) and Stull (1988, Sect. 8.6.2 and 8.8.2)
    waveScal<- base::Re(spec01 * base::Conj(spec02))
    #waveScal<- Re(spec01) * Re(spec02) + Im(spec01) * Im(spec02)
    
  }
  
  
  
  
    # some testing
    # why is the spectral peak for unweighted coefficients too high?
      # it is not too high, instead the model SPEmod() internally works with the peak of the frequency-weighted spectrum
      # hence, the resulting peak when using SPEmod() in optim() is the peak of the frequency-weighted spectrum
    # why is the wavelet scalogram being weighted when calculating the covariance; this appears different from summing cospectra?
      # part of variance / covariance estimate, see Torrenco and Compo (1998) Eq (14) or Metzger et al. (2013) Eq. (7)
  
    # rows from first obs to last obs
    # columns from high-frequency to low-frequency
    # str(waveScal)
  
    # variance contribution of each scale [unit^2]
    # use absolute value for determining power-law decay and transfer function only
      # important to take the absolute value of the Wavelet coefficients (not scale integrated "Fourier" coefficients)
      # the "Fourier" coefficients are already attenuated through summing over positive and negative Wavelet coefficients, such don't express the total variance on that scale anymore
    # the transfer function then still needs to be applied over the cross-scalogram with positive and negative Wavelet coefficients
    # sum results in total variance for dataset, e.g. 30 min
    # then normalize to sum of unity
    spec <- base::colSums(base::abs(waveScal))
    spec <- spec / base::sum(spec, na.rm=TRUE)
    
    # frequency [Hz]
    freq <- 1/prd #??? Should this be divided by sampling frequency

    # #determine spectral peak empirically
    # fx_out <- optim(
    #   par = 0.1,
    #   fn = eddy4R.turb::find_FX_og,
    #   #independent variable, preferabley f, but n is possible
    #   IDE = rev(freq),
    #   #dependent variable, spectra or cospectra
    #   # DEP = rev(tst * scal),
    #   DEP = rev(spec),
    #   #spectrum or cospectrum?
    #   MethSpec = MethSpec,
    #   #stability parameter
    #   paraStbl = paraStbl,
    #   #use frequency-weighted (co)spectrum?
    #   WEIGHT = FALSE,
    #   #frequency range for determining optimiality criterion
    #   WHR_CRIT = c(0.01, 1),
    #   #cumulative flux contribution for which measured (co)-spectrum is scaled to model (co)-spectrum
    #   crit_cum = 0.6,
    #   #generate plot?
    #   plot_path = NULL,
    #   #determine peak frequency or output spectral correction factor?
    #   meth = c("peak", "corfac")[1],
    #   method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")[4],
    #   lower = 0.001, upper = 1
    # )
    
    # peak index and frequency
    # idxPeak <- GenKern::nearest(x = freq, xval = fx_out$value)
    idxPeak <- GenKern::nearest(x = freq, xval = 0.1)
    # freq[idxPeak]
    
    # maximum frequency to consider for fitting power law decay
    # these are up to the next 20 higher-frequency increments after the spectral peak, but never higher than 1 Hz
    idx1Hz <- GenKern::nearest(x = freq, xval = 0.5)
    idxFreqMax <- idx1Hz
    # idxFreqMax <- idxPeak - 20
    # if(idxFreqMax < idx1Hz) idxFreqMax <- idx1Hz

    # only continue if peak frequency < 1 Hz
    if(idxPeak > idxFreqMax) {

      # linear model to determine regression slope between peak frequency and 1 Hz
      # robust::lmRob() is doing a similar MM-estimation as robustbase::lmrob(), but appears to be less error-prone
      # for overview page 18 of http://use-r-carlvogt.github.io/PDFs/2017Avril_Cantoni_Rlunch.pdf
      modlLin <-
        # robustbase::lmrob(log10(spec[idxFreqMax:idxPeak]) ~ log10(freq[idxFreqMax:idxPeak]))
        robust::lmRob(log10(spec[idxFreqMax:idxPeak]) ~ log10(freq[idxFreqMax:idxPeak]))
      
      # plot(log10(tst[idxFreqMax:idxPeak]) ~ log10(freq[idxFreqMax:idxPeak]))
      # points(modlLin$fitted.values ~ log10(freq[idxFreqMax:idxPeak]), col = 2)
  
      # if the regression slope (power law coefficient) exceeds the bounds -1.8 ... -1.3, the conventional -5/3 slope is used as alternative
      if(!(modlLin$coefficients[2] > -1.8 & modlLin$coefficients[2] < -1.3)) {
        modlLin$coefficients[1] <- base::mean(base::log10(spec[idxFreqMax:idxPeak]) - (-5/3 * base::log10(freq[idxFreqMax:idxPeak])), na.rm = TRUE)
        modlLin$coefficients[2] <- -5/3
      }
  
      # calculate the reference spectral coefficients following the power slope
      specRefe <- 10^(modlLin$coefficients[1] + modlLin$coefficients[2] * base::log10(freq))
  
      # calculate transfer function
      # apply only to frequencies > 1 Hz
      funcTfm <- spec / specRefe
      funcTfm[base::which(freq < 0.5)] <- 1
      # funcTfm[idxPeak:length(funcTfm)] <- 1
      # plot(funcTfm ~ freq, log = "x")
      
      # # plotting
      # 
      #   #generate spectral model for range of frequencies
      #   spemod <- SPEmod(
      #     #independent variable, preferabley f, but n is possible
      #     ide = freq,
      #     #spectrum or cospectrum?
      #     MethSpec = MethSpec,
      #     #stability parameter
      #     paraStbl = wrk$reyn$mn$sigma,
      #     #frequency f at which fCO(f) reaches its maximum value
      #     fx=freq[idxPeak],
      #     #output frequency-weighted (co)spectrum?
      #     # weight=TRUE
      #     weight=FALSE
      #   )
      # 
      #   # actual plotting
      #   plot(spec ~ freq, log="xy")
      #   lines(spemod ~ freq)
      #   points(spec[idxFreqMax:idxPeak] ~ freq[idxFreqMax:idxPeak], pch=21, col=4, bg=4)
      #   points(spec[idxPeak] ~ freq[idxPeak], pch=21, col=2, bg=2)
      #   lines(specRefe[1:idxPeak] ~ freq[1:idxPeak], col=2)
         
      # apply transfer function
      waveScalCor <- 
          base::sapply(1:base::ncol(waveScal), function(x) waveScal[,x] / funcTfm[x] )
          # t(sapply(1:nrow(waveScal), function(x) waveScal[x,] / funcTfm ))
      
    #weighted wavelet scalogram
      
      # uncorrected
      waveScalWght <-
        base::sapply(1:base::ncol(waveScal), function(x) waveScal[,x] / scal[x] )
        # t(sapply(1:nrow(waveScal), function(x) waveScal[x,] / scal ))
      
      # corrected
      waveScalCorWght <-
        base::sapply(1:base::ncol(waveScalCor), function(x) waveScalCor[,x] / scal[x] )
        # t(sapply(1:nrow(waveScalCor), function(x) waveScalCor[x,] / scal ))
      
      
    # #spectral correction using sigmoidal transfer function
    # #http://paos.colorado.edu/research/wavelets/faq.html#scale
    # # The scale refers to the width of the wavelet.
    # # The period (or inverse frequency) is the approximate Fourier period that corresponds to the oscillations within the wavelet.
    # # mycwt[["w_met"]]@scale / mycwt[["w_met"]]@period
    #   
    # #perform only if half-power frequency is defined for variable
    # if(!is.na(FreqCut)) {
    #   
    #   #transfer function
    #   fun_tsig <- fun_TSIG(FreqCut = FreqCut, freq = 1/prd)
    #   cwt_vc3 <- t(sapply(1:nrow(waveScalWght), function(x) waveScalWght[x,] / fun_tsig ))
    #   
    # } else {
    #   
    #   cwt_vc3 <- waveScalWght
    #   
    # }
    
    #time/space series of variance at native resolution
    if(base::is.null(SetPrd)) SetPrd <- 1:base::ncol(waveScalWght)
    waveVari <- CoefNorm * base:::rowSums(waveScalWght[,SetPrd])
    waveVariCor <- CoefNorm * base:::rowSums(waveScalCorWght[,SetPrd])
    
    #conversion from variance fraction to total local variance
    waveVari <- waveVari * base::length(waveVari)
    waveVariCor <- waveVariCor * base::length(waveVariCor)
  
    #
    
    
    # prepare outputs
    rpt <- base::list()
    
      # peak frequency
      rpt$freqPeak <- freq[idxPeak]
      
      # uncorrected
      rpt$mean <- base::mean(waveVari, na.rm = TRUE)
      
      # corrected
      rpt$cor <- base::mean(waveVariCor, na.rm = TRUE)
      
      # ratio
      rpt$coefCor <- rpt$cor / rpt$mean
      
      # flag
      rpt$qfWave <- qfWave
      
      # Total local variance/covariance
      rpt$waveVariCor <- waveVariCor 

  # in case peak frequency > 1 Hz
  } else {
    
    # prepare outputs
    rpt <- base::list(
      freqPeak = freq[idxPeak],
      mean = base::mean(waveVari, na.rm = TRUE),
      cor = NA,
      coefCor = 1,
      qfWave = 1,
      waveVariCor = NA
    )
      
  }
    
# no Wavelet processing if > 10% NAs
} else {
  
  # prepare outputs
  rpt <- base::list(
    freqPeak = NA,
    mean = NA,
    cor = NA,
    coefCor = 1,
    qfWave = qfWave,
    waveVariCor = NA
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