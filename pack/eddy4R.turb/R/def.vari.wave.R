##############################################################################################
#' @title Definition function: funtion to determine the temporally resolved variance/covariance from continuous wavelet transform

#' @author
#' David Durden \email{ddurden@battelleecology.org}
#' Stefan Metzger

#' @description 
#' Wrapper function. funtion to determine the temporally resolved variance/covariance from continuous wavelet transform including high-frequency spectral correction and selectable low-frequency cutoff The frequency response correction using Wavelet techniques described in Norbo and Katul, 2012 (NK12)

#' @param dfInp data.frame, consisting of the input data to perform the wavelet transformation
#' @param spec1 Waves package output object spectrum, continuous wavelet transform output object complex spectrum for the first variable (typically w')denoted as \code{object1@spectrum}.
#' @param spec2 Waves package output object spectrum, continuous wavelet transform output object complex spectrum for the second variable for cospectra denoted as \code{object2@spectrum}.
#' @param scal Waves package output object scale, width of the wavelet at each scale [s] denoted as \code{object1@scale}
#' @param peri Waves package output object period, approximate corresponding Fourier period [s] denoted as \code{object1@period}
#' @param freq_0 vector, half-power frequencies for individual variables [Hz] for determining transfer function to correct frequency response
#' @param whr_peri numeric, which wavelengths/spatial scales to consider if you want to only consider high frequency values
#' @param fac_norm numeric, normalization factor specific to the choice of Wavelet parameters.
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
  fac_norm = rpt$coefNorm,
  # Wavelet flag: process (0) or not
  flag,
  #stability parameter
  SI,
  #spectrum or cospectrum?
  SC
) {
  
  
# only process if < 10% NAs
if(flag == 0) {

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
  
  
  
  
    # some testing
    # why is the spectral peak for unweighted coefficients too high?
      # it is not too high, instead the model SPEmod() internally works with the peak of the frequency-weighted spectrum
      # hence, the resulting peak when using SPEmod() in optim() is the peak of the frequency-weighted spectrum
    # why is the wavelet scalogram being weighted when calculating the covariance; this appears different from summing cospectra?
      # part of variance / covariance estimate, see Torrenco and Compo (1998) Eq (14) or Metzger et al. (2013) Eq. (7)
  
    # rows from first obs to last obs
    # columns from high-frequency to low-frequency
    # str(cwt_vc1)
  
    # variance contribution of each scale [unit^2]
    # use absolute value for determining power-law decay and transfer function only
      # important to take the absolute value of the Wavelet coefficients (not scale integrated "Fourier" coefficients)
      # the "Fourier" coefficients are already attenuated through summing over positive and negative Wavelet coefficients, such don't express teh total variance on that scale anymore
    # the transfer function then still needs to be applied over the cross-scalogram with positive an negative Wavelet coefficients
    # sum results in total variance for dataset, e.g. 30 min
    # then normalize to sum of unity
    spec <- colSums(abs(cwt_vc1))
    spec <- spec / sum(spec, na.rm=TRUE)
    
    # frequency [Hz]
    freq <- 1/(peri/20)

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
    #   SC = SC,
    #   #stability parameter
    #   SI = SI,
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
      LM <- robustbase::lmrob(log10(spec[idxFreqMax:idxPeak]) ~ log10(freq[idxFreqMax:idxPeak]))
      # plot(log10(tst[idxFreqMax:idxPeak]) ~ log10(freq[idxFreqMax:idxPeak]))
      # points(LM$fitted.values ~ log10(freq[idxFreqMax:idxPeak]), col = 2)
  
      # if the regression slope (power law coefficient) exceeds the bounds −1.8 ... −1.3, the conventional −5/3 slope is used as alternative
      if(!(LM$coefficients[2] > -1.8 & LM$coefficients[2] < -1.3)) {
        LM$coefficients[1] <- mean(log10(spec[idxFreqMax:idxPeak]) - (-5/3 * log10(freq[idxFreqMax:idxPeak])), na.rm = TRUE)
        LM$coefficients[2] <- -5/3
      }
  
      # calculate the reference spectral coefficients following the power slope
      specRefe <- 10^(LM$coefficients[1] + LM$coefficients[2] * log10(freq))
  
      # calculate transfer function
      # apply only to frequencies > 1 Hz
      tf <- spec / specRefe
      tf[which(freq < 0.5)] <- 1
      # tf[idxPeak:length(tf)] <- 1
      # plot(tf ~ freq, log = "x")
      
      # plotting

        #generate spectral model for range of frequencies
        spemod <- SPEmod(
          #independent variable, preferabley f, but n is possible
          ide = freq,
          #spectrum or cospectrum?
          sc = SC,
          #stability parameter
          si = wrk$reyn$mn$sigma,
          #frequency f at which fCO(f) reaches its maximum value
          fx=freq[idxPeak],
          #output frequency-weighted (co)spectrum?
          # weight=TRUE
          weight=FALSE
        )

        # actual plotting
        plot(spec ~ freq, log="xy")
        lines(spemod ~ freq)
        points(spec[idxFreqMax:idxPeak] ~ freq[idxFreqMax:idxPeak], pch=21, col=4, bg=4)
        points(spec[idxPeak] ~ freq[idxPeak], pch=21, col=2, bg=2)
        lines(specRefe[1:idxPeak] ~ freq[1:idxPeak], col=2)
      
      # apply transfer function
      cwt_vc1t <- t(sapply(1:nrow(cwt_vc1), function(x) cwt_vc1[x,] / tf ))
  
          
    #weighted wavelet scalogram
      
      # uncorrected
      cwt_vc2 <- t(sapply(1:nrow(cwt_vc1), function(x) cwt_vc1[x,] / scal ))
      
      # corrected
      cwt_vc2t <- t(sapply(1:nrow(cwt_vc1t), function(x) cwt_vc1t[x,] / scal ))
      
      
    # #spectral correction using sigmoidal transfer function
    # #http://paos.colorado.edu/research/wavelets/faq.html#scale
    # # The scale refers to the width of the wavelet.
    # # The period (or inverse frequency) is the approximate Fourier period that corresponds to the oscillations within the wavelet.
    # # mycwt[["w_met"]]@scale / mycwt[["w_met"]]@period
    #   
    # #perform only if half-power frequency is defined for variable
    # if(!is.na(freq_0)) {
    #   
    #   #transfer function
    #   fun_tsig <- fun_TSIG(freq_0 = freq_0, freq = 1/peri)
    #   cwt_vc3 <- t(sapply(1:nrow(cwt_vc2), function(x) cwt_vc2[x,] / fun_tsig ))
    #   
    # } else {
    #   
    #   cwt_vc3 <- cwt_vc2
    #   
    # }
    
    #time/space series of variance at native resolution
    if(is.null(whr_peri)) whr_peri <- 1:ncol(cwt_vc2)
    myvc2 <- fac_norm * base:::rowSums(cwt_vc2[,whr_peri])
    myvc2t <- fac_norm * base:::rowSums(cwt_vc2t[,whr_peri])
    
    #conversion from variance fraction to total local variance
    myvc2 <- myvc2 * length(myvc2)
    myvc2t <- myvc2t * length(myvc2t)
  
    #
    
    
    # prepare outputs
    rpt <- list()
    
      # peak frequency
      rpt$freqPeak <- freq[idxPeak]
      
      # uncorrected
      rpt$mean <- ifelse(SC == "spe", sqrt(mean(myvc2, na.rm = TRUE)), mean(myvc2, na.rm = TRUE))
      
      # corrected
      rpt$corr <- ifelse(SC == "spe", sqrt(mean(myvc2t, na.rm = TRUE)), mean(myvc2t, na.rm = TRUE))
      
      # ratio
      rpt$fac <- rpt$corr / rpt$mean
      
      # flag
      rpt$flag <- flag

  # in case peak frequency > 1 Hz
  } else {
    
    # prepare outputs
    rpt <- list(
      freqPeak = freq[idxPeak],
      mean = mean(myvc2, na.rm = TRUE),
      corr = NA,
      fac = 1,
      flag = 1
    )
      
  }
    
# no Wavelet processing if > 10% NAs
} else {
  
  # prepare outputs
  rpt <- list(
    freqPeak = NA,
    mean = NA,
    corr = NA,
    fac = 1,
    flag = flag
  )
  
}
  
  # return results
  return(rpt)

  
  # # some testing
  # rng <- range(c(sqrt(myvc2), sqrt(dfInp$w_met^2)))
  # plot(sqrt(myvc2) ~ sqrt(dfInp$w_met^2), xlim = rng, ylim = rng, asp=1)
  # lines(sqrt(myvc2), col=2)

  #plot change in variance
  #between 0% and 10% along flight line for H2O
  #between 0% and 1% along flight line for T
  #plot(I(myvc2 / myvc3), log="y")

}