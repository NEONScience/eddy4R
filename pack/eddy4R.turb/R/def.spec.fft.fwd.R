##############################################################################################
#' @title Definition function: Fast Fourier transform

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-16)
#     terms update

#' @description Fast Fourier transform.

#' @param time continuous timestamp in any float format; only for gapfilling
#' @param data continuous matrix of variables to be transformed with same length as tstamp; NAs are interpolated
#' @param veloRltv relative motion between observation platform and atmosphere with same length as tstamp; NAs are interpolated. E.g. |wind vector| for tower observation [m s-1]
#' @param distZaxsMeas measurement height, either single number or vector of same length as tstamp
#' @param FreqSamp observation frequency: single integer [Hz]
#' @param MethMeanRmv logical to determine if the data should be demeaned
#' @param MethTrndRmv logical to determine if the data should be detrended
#' @param WghtTape taper the data for a fraction (<=0.5) at both ends, or FALSE

#' @return  A list (\code{rpt}) of transformed data variables in frequency space and calculation of various independent variables sorted from lowest to highest frequency

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Fast Fourier Transform, FFT, spectral

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

##the transform can be inverted (use for sensor fusion?)
#  spec.inv <- Re(mvfft(specFft, inverse = T))

########################################################
#FAST FOURIER TRANSFORM
########################################################
def.spec.fft.fwd <- function(
  time, #time: continuous timestamp in any float format; only for gapfilling
  data, #data: continuous matrix of variables to be transformed with same length as tstamp; NAs are interpolated
  veloRltv, #veloRltv: relative motion between observation platform and atmosphere with same length as tstamp; NAs are interpolated. E.g. |wind vector| for tower observation [m s-1]
  distZaxsMeas, #distZaxsMeas: measurement height, either single number or vector of same length as tstamp
  FreqSamp, #FreqSamp: observation frequency: single integer [Hz]
  MethMeanRmv=FALSE, #MethMeanRmv: logical to determine if the data should be demeaned
  MethTrndRmv=TRUE, #MethTrndRmv: logical to determine if the data should be detrended
  WghtTape=0.05 #WghtTape: taper the data for a fraction (<=0.5) at both ends, or FALSE
){

  #combine variables
  tmpData <- base::cbind(data, distZaxsMeas, veloRltv)
  
  #gap filling
  for(idx in 1:base::ncol(tmpData)) {
    tmpDataGf <- stats::approx(time, tmpData[,idx], xout = time)[[2]]	#linearly interpolate gaps
    #    tmp <- tmp[2:(length(tmp)-1)]				#cut out potentially remaining NAs at the end points
    if (idx == 1) {
      dataSpecInp <- stats::as.ts(tmpDataGf, frequency = FreqSamp)
    } else {
      dataSpecInp <- stats::ts.union(dataSpecInp, stats::as.ts(tmpDataGf, frequency = FreqSamp))
    }
  }
  
  #cut out potentially remaining NAs at the end points
  dataSpecInp <- stats::ts.union(stats::as.ts(time, frequency = FreqSamp), dataSpecInp)
  dataSpecInp <- stats::na.omit(dataSpecInp)
  time <- dataSpecInp[,1]
  dataSpecInp <- dataSpecInp[,-1]
  
  #separate variables
  distZaxsMeas <- dataSpecInp[,(base::ncol(dataSpecInp)-1)]
  veloRltv <- dataSpecInp[,base::ncol(dataSpecInp)]
  dataSpecInp <- base::as.matrix(dataSpecInp[,1:(ncol(dataSpecInp)-2)])
  base::dimnames(dataSpecInp)[[2]] <- base::dimnames(data)[[2]]
  
  #demeaning
  if(MethMeanRmv == TRUE & MethTrndRmv == FALSE) {
    for(idx in 1:base::ncol(dataSpecInp)) {
      dataSpecInp[,idx] <- dataSpecInp[,idx] - base::mean(dataSpecInp[,idx], na.rm=T)
    }
  }
  
  #detrending
  #http://www.stat.rutgers.edu/home/rebecka/Stat565/lab42007.pdf: Before you estimate a power spectrum, trends should be removed. Rapid falloff in the spectrum makes the estimates very unreliable (bias is a problem). The spectrum() command removes linear trends by default if you omit detrend=F.
  if(MethTrndRmv == TRUE) {
    for(idx in 1:base::ncol(dataSpecInp)) {
      dataSpecInp[,idx] <- dataSpecInp[,idx] - stats::lm(dataSpecInp[,idx]~time)$fitted.values
      #plot(dataSpecInp[,1]~I(tstamp[2:(length(tstamp)-1)]), type="l")
      #abline(h=0, col=4, lwd=3)
      #lines(test$fitted.values~I(tstamp[2:(length(tstamp)-1)]), col=2, lwd=3)
    }
  }
  
  #create time series
  dataSpecInp <- stats::as.ts(dataSpecInp, frequency = FreqSamp)
  
  #tapering (omit for EC)
  #http://www.stat.rutgers.edu/home/rebecka/Stat565/lab42007.pdf: Tapering reduces bias and makes peaks look sharper. By tapering you avoid having far away frequency peaks leak into other frequencies. The taper number has to be between 0 and 0.5. This is the fraction of data points that get down-weighted in the spectrum computation. Tapering reduces bias, increases variance, decreases the correlation between variables.
  if(base::is.numeric(WghtTape)) {
    dataSpecInp <- stats::spec.taper(x=dataSpecInp, p=WghtTape)
  }
  
  #independent variables                                                                                                                    
  #define relative frequency according to Stull
  freqRltv <- (stats::as.ts(1:base::attributes(dataSpecInp)$dim[1], frequency = FreqSamp) - 1) / base::attributes(dataSpecInp)$dim[1]
  #only 0 < frequency < Nyqvist
  setFreqRng <- base::which(freqRltv > 0 & freqRltv <= 0.5)
  #corresponding 'observation' frequency
  freqMeas <- freqRltv * FreqSamp
  #corresponding wavelength according to taylor's hypothesis (mean relative motion, Foken, 2008 Eq. 2.106)
  distWave <- base::mean(veloRltv, na.rm=TRUE) / freqMeas
  #corresponding normalized frequency
  freqNorm <- base::mean(distZaxsMeas, na.rm=TRUE) / distWave
  #corresponding wavenumber
  numWave <- (2 * pi) / distWave
  
  #dependent variables
  #fft returns unnormalized univariate Fourier transform of the sequence of values in z
  #therefore divide by length of time series
  #Note that for frequencies greater than .5 the Fourier transform is just the complex conjugate of the frequencies less than .5
  specFft <- stats::mvfft(dataSpecInp) / base::attributes(dataSpecInp)$dim[1]
  
  #unfolded spectral energy
  specFftEngy <- base::Re(specFft * base::Conj(specFft))
  
  #combine results
  rpt <- base::list(
    setFreqRng,	#valid subset (0 < frequency < Nyqvist)
    freqRltv,		#relative frequency (0...1) [-]
    freqMeas,		#observation frequency (0 < frequency < fmax) [s-1]
    distWave,		#wavelength [m]
    freqNorm,		#normalized frequency (freqMeas * distZaxsMeas / U) [-]
    numWave,		#wavenumber (2 * pi * freqMeas / U) [m-1]
    dataSpecInp,	#continuous input data in time space (no NAs)
    specFft,	#variables in frequency space; complex numbers [input units]
    specFftEngy		#unfolded spectral energy in frequency space[(input units)^2]
  )
  base::attributes(rpt)$names <- c("setFreqRng", "freqRltv", "freqMeas", "distWave", "freqNorm", "numWave", "dataSpecInp", "specFft", "specFftEngy")
  
  #export results
  return(rpt)
  
  ########################################################
} #End of function
########################################################