##############################################################################################
#' @title Wrapper function: Calculate Wavelet cospectra and frequency response correction

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description
#' Wrapper function. Calculate Wavelet spectrum/cospectrum using the Waves package. The frequency response correction using Wavelet techniques described in Norbo and Katul, 2012 (NK12)

#' @param dfInp data.frame, consisting of the input data to perform the wavelet transformation
#' @param FuncWave numeric, wavelet function to be used, defaults to orthogonal 'haar'
#' @param FreqSamp numeric, sampling frequency, defaults to 20Hz for NEON data
#' @param zeroPad logical, should zero padding of time series be applied. If set to TRUE then detrending of time series is performed as well.
#' @param ThshMiss numeric, dimensionless fraction of missing values in each column of data allowed before the quality flag is tripped. Defaults to 0.1 or 10 percent.
#' @param init numeric, initial values to perform optimization of fitting nonlinear model to frequency-weighted vertical wind speed. Passed on to def.spec.high.freq.cor.R
#' @param paraStbl stability parameter (numeric). Not currently implemented.

#'
#' @return An list containing wavelet cospectra, quality flags if data was available to perform correction, and correction coefficients (coefCor) for each variable.
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
#   David Durden (2018-01-12)
#     Added failsafe for ts object creation in case all dfInp values are NaN's
#   Stefan Metzger (2018-02-24)
#     Added failsafe in case the vertical wind is not available
#   David Durden (2023-03-12)
#     Updating terms, fixing ts bug, and adding cutoff frequencies
#   Adam Young (2024-03-19)
#     Initial commit for major changes that now use methods that more closely align with NK12
#   Adam Young (2024-05-07)
#     Clean and finalize code for broader testing.
##############################################################################################

wrap.wave <- function(
dfInp,
FuncWave = "haar", # Default from Nordbo and Katul 2012, orthogonal wavelet basis is ideal
FreqSamp = 20, #Defaults to 20Hz
zeroPad = FALSE,
ThshMiss = 0.1,
init = c(3, 5), # Parameter initialization for optimized curve fitting of weighted spectra
paraStbl = NULL # Stability not currently considered
) {

#Create output list
rpt <- list()

####Check quality of data###########################################
#Create a logical flag if more data is missing than the threshold
rpt$qfMiss <- as.list(base::colMeans(base::is.na(dfInp), na.rm = TRUE) > ThshMiss)
#Turn the flags into integers
rpt$qfMiss <- lapply(rpt$qfMiss, base::as.integer)
# in case the vertical wind is not available, no cospectral correction can be performed
# then flag all scalars
if(rpt$qfMiss$veloZaxsHor == 1) invisible(lapply(names(rpt$qfMiss), function(x) rpt$qfMiss[x] <<- 1))
####################################################################

# fill missing values through linear interpolation
# NAs at start and end are removed by setting na.rm = TRUE
tmp <- zoo::na.approx(dfInp, na.rm = TRUE, rule = 2) #Rule 2 allows extrapolating end values using the nearest value or explicitly using zoo::na.locf, could be replaced with zeros using na.fill with any value: see https://stackoverflow.com/questions/7317607/interpolate-na-values-in-a-data-frame-with-na-approx
# tmpNorm <- as.data.frame(scale(tmp, center = TRUE, scale = FALSE)) # Recenter time series after filling in NAs

#Failsafe in case all values are NaN
if(nrow(tmp) > 1) dfInp <- tmp

# Determine number of scales based on whether zero padding of time series will be done AND current number of observations in time series
if (zeroPad) {
  
  numScal <- ceiling(log(nrow(dfInp), base = 2))
  
  numZero <- 2^numScal - nrow(dfInp)
  dfInpPad <- as.data.frame(matrix(0, nrow = nrow(dfInp) + numZero, ncol = ncol(dfInp)))
  colnames(dfInpPad) <- colnames(dfInp)
  
  idxData <- seq(numZero / 2 + 1, numZero / 2 + nrow(dfInp))
  
  dfInpPad[idxData, ] <- dfInp
  dfInp <- dfInpPad
  rm(dfInpPad, numZero)
  
} else {
  
  # Calculate number of scales based on length of time series dataset
  numScal <- floor(log(nrow(dfInp), base = 2))
  idxData <- seq(1, 2^numScal) # Index for data in this case is the first 2^J observations
  dfInp <- dfInp[idxData, ] # only used first 2^J observations
  
}

# Re-center data after excluding observations after 1:2^numScal and filling in NAs
dfInp <- as.data.frame(scale(dfInp, center = TRUE, scale = FALSE))

# *** Not sure if following code is needed anymore ***
# dfInp <- as.data.frame(stats::ts(
#   dfInp,
#   start = 0,
#   frequency = FreqSamp
# ))

rpt$wave <- list()

for (idxCol in colnames(dfInp)) {
  # idxCol <- colnames(dfInp)[6]
  
  #Creating ts vector
  vectTmp <- stats::ts(
    dfInp[,idxCol],
    start = 0,
    frequency = FreqSamp
  )
  
    # 'wavelets' (ver 0.3-0.2) package rounds to the nearest 5th decimal point (e.g., round(x, 5))
    # For variables like rtioMoleDryCo2 this causes weird behavior as the time 
    # series is interpreted mostly as zeros. Using the standard deviation of the 
    # time series to estimate scale, the following lines of code multiply 
    # variables that approach this rounding limit by a constant to ensure this 
    # rounding does not affect results. Since the key value returned is based on a ratio
    # of the covariances then multiplying by a constant should not impact results as 
    # the constants will cancel out.
    # Cov(aX,Y) = aCov(X,Y)
    # https://en.wikipedia.org/wiki/Covariance#Covariance_of_linear_combinations
    
    sdVectTmp <- sd(vectTmp)
    numZeroDecimal <- floor(log10(sdVectTmp)) 
    
    if (numZeroDecimal < -1) vectTmp <- 10^abs(numZeroDecimal) * vectTmp 
  
  
  if (rpt$qfMiss[[idxCol]] == 1) {
    
    rpt$wave[[idxCol]] <- NA
    
    if (idxCol == "veloZaxsHor") {
      
      rpt$scal <- seq(numScal - 1, 0)
      
    }
    
  } else {
    
    # compute discrete wavelet transform
    rpt$wave[[idxCol]] <- wavelets::dwt(vectTmp, 
                                        filter = FuncWave, 
                                        n.levels = numScal) #, boundary = "reflection")
    msg <- paste(idxCol, "... done.")
    tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
    
    if (idxCol == "veloZaxsHor") { # Only retrieve scale values one time
      
      rpt$scal <- sapply(rpt$wave[["veloZaxsHor"]]@W, function(x) log(length(x), base = 2)); names(rpt$scal) <- NULL
      
    }
    
  }
    
}

# covariance for all wavelengths
# not currently implemented for friction velocity as approach to negative
rpt$cov <- lapply(names(rpt$wave)[-which(names(rpt$wave) == "veloZaxsHor")], function(var)
  eddy4R.turb::def.spec.high.freq.cor(
  # def.vari.wave(
    # Wavelet coefficients variable 1
    varDwt = rpt$wave[[var]],
    # Wavelet coefficients vertical wind speed
    veloZaxsDwt = rpt$wave[["veloZaxsHor"]],
    # Scale vector
    scal = rpt$scal,
    # Initial parameters for optimzation routine to find peak frequency
    init = init,
    # Data index for original time series (i.e., no zero padding)
    idxData = idxData,
    # Wavelet flag: process (0) or not
    qfWave=rpt$qfMiss[[var]]
  )
)

names(rpt$cov) <- names(rpt$wave)[-which(names(rpt$wave) == "veloZaxsHor")]

#return all output from the wave function
return(rpt)

}

