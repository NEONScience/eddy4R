##############################################################################################
#' @title Wrapper function: Calculate Wavelet cospectra and frequency response correction

#' @author
#' David Durden \email{ddurden@battelleecology.org}
#' Adam Young \email{younga1@battelleecology.org}

#' @description
#' Wrapper function. Calculate Wavelet spectrum/cospectrum using the Waves package. The frequency response correction using Wavelet techniques described in Norbo and Katul, 2012 (NK12)

#' @param dfInp data.frame, consisting of the input data to perform the wavelet transformation
#' @param FuncWave numeric, wavelet function to be used within 'wavelets' package, defaults to orthogonal 'haar'
#' @param FreqSamp numeric, sampling frequency, defaults to 20Hz for NEON data
#' @param zeroPad logical, should zero padding of time series be applied? Not currently implemented, for potential future use.
#' @param ThshMiss numeric, dimensionless fraction of missing values in each column of data allowed before the quality flag is tripped. Defaults to 0.1 or 10 percent.
#' @param init numeric, initial parameter values to perform optimization of fitting nonlinear model to frequency-weighted vertical wind speed and find frequency peak occurrs at. Passed on to eddy4R.turb::def.spec.peak.R
#' @param paraStbl stability parameter (numeric). Not currently implemented.

#'
#' @return An list containing wavelet cospectra, quality flags if data was available to perform correction, and correction coefficients (coefCor) for each variable.
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
#' Nordbo, A., Katul, G. A Wavelet-Based Correction Method for Eddy-Covariance High-Frequency Losses in Scalar Concentration Measurements. Boundary-Layer Meteorol 146, 81–102 (2013). https://doi.org/10.1007/s10546-012-9759-9

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
#   Adam Young (2024-05-21)
#     Updated to account for rounding in 'wavelets' package and ensure inertial 
#     sub-range estimates don't overlap with spectral peak for each data variable.
#     Also updated output to include all the spectra/cospectra results, both unnormalized and normalized.
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

# Currently only output to make it easier to test in the future, can remove to save space
rpt$input <- dfInp

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

#Failsafe in case all values are NaN
if(nrow(tmp) > 1) dfInp <- tmp

# Determine number of scales based on whether zero padding of time series will 
# be done AND current number of observations in time series
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

# idwt.R function in 'wavelets' (ver 0.3-0.2) package rounds to the nearest 5th 
# decimal point (e.g., round(x, 5)). For variables like rtioMoleDryCo2 this causes 
# weird behavior as the time series is interpreted mostly as zeros. Using the 
# standard deviation of the time series to estimate scale, the following lines of 
# code multiply variables that approach this rounding limit by a scaling constant 
# (scalCnst) to ensure this rounding does not affect results.
sclrCnst <- sapply(dfInp, 
                   function(x) {
                     xSd <- sd(x) # Standard deviation to estimate scale (e.g., 1e-02, 1e-03)
                     varScal <- 10^floor(log10(xSd)) # Scale magnitude (e.g., 1e-01, 1e-02, etc.)
                     sclrCnst <- ifelse(varScal >= 1, 1, 1 / varScal) # Get inverse of variable scale if < 1
                     return(sclrCnst)
                     })

rpt$wave <- list()

for (idxCol in colnames(dfInp)) {
  # idxCol <- colnames(dfInp)[6]
  
  #Creating ts vector
  vectTmp <- stats::ts(
    dfInp[,idxCol],
    start = 0,
    frequency = FreqSamp
  )
  
  if (rpt$qfMiss[[idxCol]] == 1) {
    
    rpt$wave[[idxCol]] <- NA
    
  } else {
    
    # compute discrete wavelet transform
    rpt$wave[[idxCol]] <- wavelets::dwt(vectTmp, filter = FuncWave, n.levels = numScal)
    msg <- paste(idxCol, "... done.")
    tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
    
  }
    
}

# The following code used to be internal to def.spec.high.freq.cor.R but it only needs to be done
# once per half hour since it is only for vertical wind speed. If >10% veloZaxsHor data missing
# then set to NA as processing of wavelet correction will not be done anyway.
rpt$FreqSamp <- FreqSamp
if (rpt$qfMiss$veloZaxsHor == 0) {
  
  rpt$scal <- as.numeric(sapply(rpt$wave$veloZaxsHor@W, function(x) floor(log(length(x), base = 2))))
  rpt$freq <- 2^rpt$scal * FreqSamp / 2^length(rpt$scal) # Eq. 15 in NK12  
  
} else {
  
  rpt$scal <- NA
  rpt$freq <- NA
  
}

# Time series variance estimate for each data variable, needed to normalize spectral and cospectra power
variVect <- diag(cov(dfInp))

for (idxCol in names(rpt$wave)) {
  # idxCol <- names(rpt$wave)[1]
  
  if (rpt$qfMiss[[idxCol]] == 0) {
    
    rpt$spec[[idxCol]]$vari <- variVect[idxCol] # Variance
    rpt$spec[[idxCol]]$specPowr <- sapply(rpt$wave[[idxCol]]@W, function(x) mean(x^2) / log(2))
    
    # This is the frequency weighted spectra curve. Can calculate for spectra or cospectra
    rpt$spec[[idxCol]]$specPowrNorm <- rpt$freq * rpt$spec[[idxCol]]$specPowr / rpt$FreqSamp / rpt$spec[[idxCol]]$vari # Normalized wavelet power spectrum (Eq. 7 in NK12)
    
    # Find and save frequency where peak of speectra occur
    tmpFreqPeak <- eddy4R.turb::def.spec.peak(specPowrWght = rpt$spec[[idxCol]]$specPowrNorm, para = init, freq = rpt$freq)
    rpt$spec[[idxCol]]$freqPeak <- tmpFreqPeak$freqPeak
    rpt$spec[[idxCol]]$idxFreqPeak <- tmpFreqPeak$idxFreqPeak
    rpt$spec[[idxCol]]$specModlPara <- tmpFreqPeak$para
    rpt$spec[[idxCol]]$specModlItpl <- tmpFreqPeak$rptItpl
    
    rm(tmpFreqPeak)
    
  } else {
   
    rpt$spec[[idxCol]]$vari <- NA
    rpt$spec[[idxCol]]$specPowr <- NA
    rpt$spec[[idxCol]]$specPowrNorm <- NA
    rpt$spec[[idxCol]]$freqPeak <- NA
    rpt$spec[[idxCol]]$idxFreqPeak <- NA
    rpt$spec[[idxCol]]$specModlPara <- NA
    rpt$spec[[idxCol]]$specModlItpl <- NA
     
  }
  
}

# covariance for all wavelengths
# not currently implemented for friction velocity as approach to negative
rpt$cov <- lapply(names(rpt$wave)[-which(names(rpt$wave) == "veloZaxsHor")], function(var)
  eddy4R.turb::def.spec.high.freq.cor(
    # Wavelet coefficients variable 1
    sclrDwt = rpt$wave[[var]],
    # Wavelet coefficients vertical wind speed
    veloZaxsDwt = rpt$wave[["veloZaxsHor"]],
    # Spectral power for scalar variable of interest
    sclrSpecPowr = rpt$spec[[var]]$specPowr,
    # Sampling frequency 
    FreqSamp = rpt$FreqSamp,
    # Frequency list of sampling frequency and vector for wavelet analysis
    freq = rpt$freq,
    # Peak frequency values for veloZaxsHor and scalar variable, from interpolated/modeled spectra results
    freqPeak = as.numeric(sapply(rpt$spec[c(var, "veloZaxsHor")], function(x) x$freqPeak)),
    # Index of frequency vector for veloZaxsHor frequency peak of spectra 
    idxFreqPeak = as.numeric(sapply(rpt$spec[c(var, "veloZaxsHor")], function(x) x$idxFreqPeak)),
    # Scaling constant for scalar and vertical wind speed, needed to account for rounding in 'wavelets' package
    sclrCnst = as.numeric(sclrCnst[var]),
    # Data index for original time series (i.e., no zero padding)
    idxData = idxData,
    # Wavelet flag: process (0) or not
    qfWave = rpt$qfMiss[[var]]
  )
)

names(rpt$cov) <- names(rpt$wave)[-which(names(rpt$wave) == "veloZaxsHor")]

#return all output from the wave function
return(rpt)

}

