##############################################################################################
#' @title Definition function: Fast Fourier transform

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Fast Fourier transform.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

##the transform can be inverted (use for sensor fusion?)
#  spec.inv <- Re(mvfft(spec.fft, inverse = T))

########################################################
#FAST FOURIER TRANSFORM
########################################################
SPEC.fwd <- function(
  tstamp=POST$UT,
  data,
  relmot=POST$veloTAmagn,
  height=POST$lengMCzLS,
  fre=freq,
  demean=F,
  detrend=T,
  taper=0.05)
{
  #in:
  #tstamp: continuous timestamp in any float format; only for gapfilling
  #data: continuous matrix of variables to be transformed with same length as tstamp; NAs are interpolated
  #relmot: relative motion between observation platform and atmosphere with same length as tstamp; NAs are interpolated. E.g. |wind vector| for tower observation [m s-1]
  #height: measurement height, either single number or vector of same length as tstamp
  #fre: observation frequency: single integer [Hz]
  #demean: demean the data?
  #detrend: detrend the data?
  #taper: tapter the data for a fraction (<=0.5) at both ends, or F
  #out:
  #transformation of variables in frequency space and calculation of various independent variables
  #sorted from lowest to highest frequency
  
  #combine variables
  datadum <- cbind(data, height, relmot)
  
  #gap filling
  for(i in 1:ncol(datadum)) {
    dummy <- approx(tstamp, datadum[,i], xout = tstamp)[[2]]	#linearly interpolate gaps
    #    dummy <- dummy[2:(length(dummy)-1)]				#cut out potentially remaining NAs at the end points
    if (i == 1) {
      spec.in <- as.ts(dummy, frequency = fre)
    } else {
      spec.in <- ts.union(spec.in, as.ts(dummy, frequency = fre))
    }
  }
  
  #cut out potentially remaining NAs at the end points
  spec.in <- ts.union(as.ts(tstamp, frequency = fre), spec.in)
  spec.in <- na.omit(spec.in)
  tstamp <- spec.in[,1]
  spec.in <- spec.in[,-1]
  
  #separate variables
  height <- spec.in[,(ncol(spec.in)-1)]
  relmot <- spec.in[,ncol(spec.in)]
  spec.in <- as.matrix(spec.in[,1:(ncol(spec.in)-2)])
  dimnames(spec.in)[[2]] <- dimnames(data)[[2]]
  
  #demeaning
  if(demean == T & detrend == F) {
    for(i in 1:ncol(spec.in)) {
      spec.in[,i] <- spec.in[,i] - mean(spec.in[,i], na.rm=T)
    }
  }
  
  #detrending
  #http://www.stat.rutgers.edu/home/rebecka/Stat565/lab42007.pdf: Before you estimate a power spectrum, trends should be removed. Rapid falloff in the spectrum makes the estimates very unreliable (bias is a problem). The spectrum() command removes linear trends by default if you omit detrend=F.
  if(detrend == T) {
    for(i in 1:ncol(spec.in)) {
      spec.in[,i] <- spec.in[,i] - lm(spec.in[,i]~tstamp)$fitted.values
      #plot(spec.in[,1]~I(tstamp[2:(length(tstamp)-1)]), type="l")
      #abline(h=0, col=4, lwd=3)
      #lines(test$fitted.values~I(tstamp[2:(length(tstamp)-1)]), col=2, lwd=3)
    }
  }
  
  #create time series
  spec.in <- as.ts(spec.in, frequency = fre)
  
  #tapering (omit for EC)
  #http://www.stat.rutgers.edu/home/rebecka/Stat565/lab42007.pdf: Tapering reduces bias and makes peaks look sharper. By tapering you avoid having far away frequency peaks leak into other frequencies. The taper number has to be between 0 and 0.5. This is the fraction of data points that get down-weighted in the spectrum computation. Tapering reduces bias, increases variance, decreases the correlation between variables.
  if(is.numeric(taper)) {
    spec.in <- spec.taper(x=spec.in, p=taper)
  }
  
  #independent variables                                                                                                                    
  #define relative frequency according to Stull
  fr <- (as.ts(1:attributes(spec.in)$dim[1], frequency = fre) - 1) / attributes(spec.in)$dim[1]
  #only 0 < frequency < Nyqvist
  fr_whr <- which(fr > 0 & fr <= 0.5)
  #corresponding 'observation' frequency
  fo <- fr * fre
  #corresponding wavelength according to taylor's hypothesis (mean relative motion, Foken, 2008 Eq. 2.106)
  la <- mean(relmot, na.rm=T) / fo
  #corresponding normalized frequency
  fn <- mean(height, na.rm=T) / la
  #corresponding wavenumber
  ka <- (2 * pi) / la
  
  #dependent variables
  #fft returns unnormalized univariate Fourier transform of the sequence of values in z
  #therefore divide by length of time series
  #Note that for frequencies greater than .5 the Fourier transform is just the complex conjugate of the frequencies less than .5
  spec.fft <- mvfft(spec.in) / attributes(spec.in)$dim[1]
  
  #unfolded spectral energy
  G <- Re(spec.fft * Conj(spec.fft))
  
  #combine results
  export <- list(
    fr_whr,	#valid subset (0 < frequency < Nyqvist)
    fr,		#relative frequency (0...1) [-]
    fo,		#observation frequency (0 < frequency < fmax) [s-1]
    la,		#wavelength [m]
    fn,		#normalized frequency (fo * height / U) [-]
    ka,		#wavenumber (2 * pi * fo / U) [m-1]
    spec.in,	#continuous input data in time space (no NAs)
    spec.fft,	#variables in frequency space; complex numbers [input units]
    G		#unfolded spectral energy in frequency space[(input units)^2]
  )
  attributes(export)$names <- c("fr_whr", "fr_rel", "fr_obs", "wl_obs", "fr_nor", "wn_obs", "TScont", "FScomplex", "FSunfold")
  
  #export results
  return(export)
  
  ########################################################
}
########################################################