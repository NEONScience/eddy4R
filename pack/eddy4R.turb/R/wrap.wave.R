##############################################################################################
#' @title Wrapper function: Calculate Wavelet spectrum/cospectrum and frequency response correction

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Wrapper function. Calculate Wavelet spectrum/cospectrum using the Waves package. The frequency response correction using Wavelet techniques described in Norbo and Katul, 2012 (NK12)

#' @param dfInp data.frame, consisting of the input data to perform the wavelet transformation
#' @param DiffScale numeric, determining the step difference in scales for the wavelet transformation
#' @param FuncWave Waves package function, denoting the type of mother wavelet function to be used in the transformation
#' @param FreqSamp numeric, that determines the time series objects points
#' @param ThshMiss numeric, dimensionless fraction of missing values in each column of data allowed before the quality flag is tripped. Defaults to 0.1 or 10 percent.
#' 

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
##############################################################################################


# start function wrap.hdf5.wrte.dp01()

wrap.wave <- function(
dfInp,
DiffScal = 1/8,
FuncWave = Waves::morlet(),
FreqSamp = 20, #Defaults to 20Hz
ThshMiss = .1
){

#Create output list
  rpt <- list()
  
####Check quality of data###########################################
#Create a logical flag if more data is missing than the threshold   
rpt$qfMiss <- as.list(colMeans(is.na(dfInp)) > ThshMiss)
#Turn the flags into integers
rpt$qfMiss <- lapply(rpt$qfMiss, as.integer)
####################################################################

# fill missing values through linear interpolation
# NAs at start and end are removed by setting na.rm = TRUE
dfInp <- zoo::na.approx(dfInp, na.rm = TRUE, rule = 2) #Rule 2 allows extropolating end values using the nearest value or explicitly using zoo::na.locf, could be replaced with zeros using na.fill with any value: see https://stackoverflow.com/questions/7317607/interpolate-na-values-in-a-data-frame-with-na-approx


#time series
dfInp <- as.data.frame(ts(
  dfInp,                   #discard rows with bogus w
  start = 0,      		        #compensate for missing first row
  frequency = FreqSamp			#time unit is 20 Hz
))


#Perform CWT
rpt$wave <- list()
for (c in colnames(dfInp)) {
    cat(paste(c, "..."))
    rpt$wave[[c]] <- Waves::cwt(dfInp[[c]], wavelet = FuncWave, dj = DiffScal)
    cat(" done.\n")
  }


#normalization factor specific to the choice of Wavelet parameters
rpt$coefNorm <- rpt$wave[["w_met"]]@dj * rpt$wave[["w_met"]]@dt / rpt$wave[["w_met"]]@wavelet@cdelta / length(rpt$wave[["w_met"]]@series)

#covariance for all wavelengths
rpt$cov <- data.frame(sapply(names(rpt$wave), function(var)
  def.vari.wave(
    #complex Wavelet coefficients variable 1
    spec1 = rpt$wave[[var]]@spectrum,
    #complex Wavelet coefficients variable 2
    spec2 = rpt$wave[[var]]@spectrum,
    #width of the wavelet [s]
    scal = rpt$wave[["w_met"]]@scale,
    #approximate Fourier period [d]
    peri = rpt$wave[["w_met"]]@period,
    #half-power frequencies for individual variables [Hz]
    freq_0 = NA,
    #which wavelengths/spatial scales to consider
    whr_peri = NULL,
    #normalization factor specific to the choice of Wavelet parameters
    fac_norm = rpt$coefNorm
  )
))

#return all output from the wave function
return(rpt)
}

