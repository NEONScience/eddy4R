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


# start function wrap.hdf5.wrte.dp01()

wrap.wave <- function(
dfInp,
DiffScal = 1/8,
FuncWave = Waves::morlet(),
FreqSamp = 20, #Defaults to 20Hz
SetPrd = NULL,
ThshMiss = .1,
paraStbl
){

#Create output list
rpt <- base::list()

####Check quality of data###########################################
#Create a logical flag if more data is missing than the threshold
rpt$qfMiss <- base::as.list(base::colMeans(base::is.na(dfInp)) > ThshMiss)
#Turn the flags into integers
rpt$qfMiss <- lapply(rpt$qfMiss, base::as.integer)
# in case the vertical wind is not available, no cospectral correction can be performed
# then flag all scalars
if(rpt$qfMiss$veloZaxsHor == 1) base::invisible(lapply(base::names(rpt$qfMiss), function(x) rpt$qfMiss[x] <<- 1))
####################################################################

# fill missing values through linear interpolation
# NAs at start and end are removed by setting na.rm = TRUE
tmp <- zoo::na.approx(dfInp, na.rm = TRUE, rule = 2) #Rule 2 allows extropolating end values using the nearest value or explicitly using zoo::na.locf, could be replaced with zeros using na.fill with any value: see https://stackoverflow.com/questions/7317607/interpolate-na-values-in-a-data-frame-with-na-approx

#Failsafe in case all values are NaN
if(base::nrow(tmp) > 1) dfInp <- tmp

#time series
dfInp <- base::as.data.frame(stats::ts(
  dfInp,                   #discard rows with bogus w
  start = 0,      		        #compensate for missing first row
  frequency = FreqSamp			#time unit is 20 Hz
))


# perform CWT
# in the future, can consider package "wmtsa" could enable transition to R 3.x (http://cran.at.r-project.org/web/packages/wmtsa/wmtsa.pdf)
rpt$wave <- base::list()
for (idxCol in base::colnames(dfInp)) {
  #Creating ts vector
  vectTmp <- stats::ts(
    dfInp[,idxCol],                   #discard rows with bogus w
    start = 0,      		        #compensate for missing first row
    frequency = FreqSamp			#time unit is 20 Hz
  )
    rpt$wave[[idxCol]] <- Waves::cwt(vectTmp, wavelet = FuncWave, dj = DiffScal)
    msg <- paste(idxCol, "... done.")
    tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
    
    #Calculate global wavelet spec
    waveScal<- base::abs(rpt$wave[[idxCol]]@spectrum)^2
    rpt$spec[[idxCol]] <- base::colSums(base::abs(waveScal))
  }

#normalization factor specific to the choice of Wavelet parameters
rpt$coefNorm <- rpt$wave[["veloZaxsHor"]]@dj * rpt$wave[["veloZaxsHor"]]@dt / rpt$wave[["veloZaxsHor"]]@wavelet@cdelta / base::length(rpt$wave[["veloZaxsHor"]]@series)




# # variance for all wavelengths
# # not currently used; commented out to conserve computation time
# # var <- names(rpt$wave)[3]
# rpt$var <- lapply(names(rpt$wave), function(var)
#   eddy4R.turb::def.wave.vari(
#   # def.wave.vari(
#     #complex Wavelet coefficients variable 1
#     spec01 = rpt$wave[[var]]@spectrum,
#     #complex Wavelet coefficients variable 2
#     # spec02 = rpt$wave[[var]]@spectrum,
#     #width of the wavelet [s]
#     scal = rpt$wave[[var]]@scale,
#     #approximate Fourier period [d]
#     prd = rpt$wave[[var]]@period,
#     #half-power frequencies for individual variables [Hz]
#     FreqCut  = NA,
#     #which wavelengths/spatial scales to consider
#     SetPrd = NULL,
#     #normalization factor specific to the choice of Wavelet parameters
#     CoefNorm = rpt$coefNorm,
#     # Wavelet flag: process (0) or not
#     qfWave = rpt$qfWave[[var]],
#     #stability parameter
#     paraStbl = paraStbl,
#     #spectrum or cospectrum?
#     metSpec = c("spec", "cosp")[1]
#   )
# ); names(rpt$var) <- names(rpt$wave)


# covariance for all wavelengths
# not currently implemented for friction velocity as approach to negative
rpt$cov <- lapply(names(rpt$wave)[-which(names(rpt$wave) == "veloZaxsHor")], function(var)
  eddy4R.turb::def.wave.vari(
  # def.vari.wave(
    #complex Wavelet coefficients variable 1
    spec01 = rpt$wave[[var]]@spectrum,
    #complex Wavelet coefficients variable 2
    spec02 = rpt$wave[["veloZaxsHor"]]@spectrum,
    #width of the wavelet [s]
    scal = rpt$wave[[var]]@scale,
    #approximate Fourier period [d]
    prd = rpt$wave[[var]]@period,
    
    
    #half-power frequencies for individual variables [Hz] - cutoff frequencies
    FreqCut = NA,
    #which wavelengths/spatial scales to consider
    SetPrd = NULL,
    #normalization factor/coefficient specific to the choice of Wavelet parameters
    CoefNorm = rpt$coefNorm,
    # Wavelet flag: process (0) or not
    qfWave=rpt$qfMiss[[var]],
    #stability parameter
    paraStbl = paraStbl,
    #spectrum or cospectrum?
    MethSpec = c("spec", "cosp")[2]
  )
); base::names(rpt$cov) <- base::names(rpt$wave)[-base::which(base::names(rpt$wave) == "veloZaxsHor")]

#return all output from the wave function
return(rpt)
}

