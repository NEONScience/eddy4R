# from https://gist.github.com/FHedin/05d4d6d74e67922dfad88038b04f621c
# Fast autocorrelation for R, using fftw from fftwtools. Based on the Wiener--Khintchine theorem, adapted from http://www.tibonihoo.net/literate_musing/autocorrelations.html#wikispecd

# fast autocorrelation using fftw from fftwtools (R's fft and ifft are too slow)
# first calculates autocovariance, then auto correlation at the end
acf.fft <- function(datavector)
{
  # see http://www.tibonihoo.net/literate_musing/autocorrelations.html#wikispecd
  #  and  https://lingpipe-blog.com/2012/06/08/autocorrelation-fft-kiss-eigen
  
  # get a centred version of the signal
  datavector <- datavector - mean(datavector)
  
  #  need to pad with zeroes first ; pad to a power of 2 will give faster FFT
  len.dat <- length(datavector)
  len.opt <- 2^(ceiling(log2(len.dat))) - len.dat
  fftdat <- c(datavector,rep.int(0,len.opt))
  
  # fft using fast fftw library as backend
  fftdat <- fftwtools::fftw(fftdat)
  
  # take the inverse transform of the power spectral density
  fftdat <- (abs(fftdat))^2
  fftdat <- fftwtools::fftw(fftdat,inverse=1)
  
  # We repeat the same process (except for centering) on a ‘mask’ signal,
  # in order to estimate the error made on the previous computation.
  mask <- rep.int(1,len.dat)
  mask <- c(mask,rep.int(0,len.opt))
  mask <- fftwtools::fftw(mask)
  mask <- fftwtools::fftw((abs(mask))^2,inverse=1)
  
  # The “error” made can now be easily corrected by an element-wise division
  fftdat <- fftdat / mask
  
  # keep real parts only (although there should be not imaginary part or really small ones) and remove padding data
  fftdat <- Re(fftdat[1:len.dat])
  
  # now what we have is autocovariance, for getting autocorrelation
  # The normalization is made by the variance of the signal,
  # which corresponds to the very first value of the autocovariance.
  
  fftdat <- fftdat/fftdat[1]
  
  return(fftdat)
}