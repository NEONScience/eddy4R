##############################################################################################
#' @title Wrapper function: Calculate Wavelet spectrum/cospectrum and frequency response correction

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description
#' Wrapper function. Calculate Wavelet spectrum/cospectrum using the Waves package. The frequency response correction using Wavelet techniques described in Norbo and Katul, 2012 (NK12)

#' @param dfInp data.frame, consisting of the input data to perform the wavelet transformation
#' @param DiffScal numeric, determining the step difference in scales for the wavelet transformation
#' @param FuncWave Waves package function, denoting the type of mother wavelet function to be used in the transformation
#' @param FreqSamp numeric, that determines the time series objects points
#' @param SetPrd numeric, which wavelengths/spatial scales to consider if you want to only consider high frequency values
#' @param ThshMiss numeric, dimensionless fraction of missing values in each column of data allowed before the quality flag is tripped. Defaults to 0.1 or 10 percent.
#' @param paraStbl stability parameter (numeric)

#'
#' @return An list constaining wavelet spectra, quality flags if data was available to perform correction, and frequency reponse correction parameters if activated.
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
##############################################################################################

wrap.wave <- function(
dfInp,
FuncWave = "haar", # From Nordbo and Katul 2012, orthogonal wavelet basis is ideal
FreqSamp = 20, #Defaults to 20Hz
zeroPad = FALSE,
ThshMiss = 0.1,
init = c(1, 1), # Parameter initialization for optimized curve fitting of weighted spectra
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
tmpNorm <- as.data.frame(scale(tmp, center = TRUE, scale = FALSE)) # Recenter time series after filling in NAs

#Failsafe in case all values are NaN
if(nrow(tmpNorm) > 1) dfInp <- tmpNorm

# Determine number of scales based on whether zero padding of time series will be done AND current number of observations in time series
if (zeroPad) {
  
  numScal <- ceiling(log(nrow(dfInp), base = 2))
  
  numZero <- 2^numScal - nrow(dfInp)
  dfInpPad <- as.data.frame(matrix(0, nrow = nrow(dfInp) + numZero, ncol = ncol(dfInp)))
  colnames(dfInpPad) <- colnames(dfInp)
  
  dfInpPad[seq(numZero / 2 + 1, numZero / 2 + nrow(dfInp)), ] <- dfInp
  dfInp <- dfInpPad
  rm(dfInpPad)
  
} else {
  
  numScal <- floor(log(nrow(dfInp), base = 2))
  dfInp <- dfInp[seq(1, 2^numScal), ] # only used first 2^J observations
  
}

dfInp <- as.data.frame(stats::ts(
  dfInp,
  start = 0,
  frequency = FreqSamp
))

rpt$wave <- list()

for (idxCol in colnames(dfInp)) {
  
  #Creating ts vector
  vectTmp <- stats::ts(
    dfInp[,idxCol],
    start = 0,
    frequency = FreqSamp
  )
  
  if (rpt$qfMis[[idxCol]] == 1) {
    
    rpt$wave[[idxCol]] <- NA
    
    if (idxCol == "veloZaxsHor") {
      
      rpt$scal <- seq(numScal - 1, 0)
      
    }
    
  } else {
    
    # compute discrete wavelet transform
    rpt$wave[[idxCol]] <- wavelets::dwt(vectTmp, filter = FuncWave, n.levels = numScal)
    msg <- paste(idxCol, "... done.")
    tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
    
    if (idxCol == "veloZaxsHor") {
      
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
    init = c(1,1),
    # Wavelet flag: process (0) or not
    qfWave=rpt$qfMiss[[var]]
  )
)

names(rpt$cov) <- names(rpt$wave)[-which(names(rpt$wave) == "veloZaxsHor")]

#return all output from the wave function
return(rpt)

}

