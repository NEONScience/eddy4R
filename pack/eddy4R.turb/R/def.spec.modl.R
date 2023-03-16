#############################################################################################
#' @title Definition function: Model (co)spectrum after Massman, 2005 (in Lee, 2005)

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

#' @description Model (co)spectrum after Massman, 2005 (in Lee, 2005).

#' @param idep independent variable, preferabley f, but n is possible
#' @param MethSpec spectrum or cospectrum?
#' @param paraStbl stability parameter
#' @param FreqPeak frequency f at which fCO(f) reaches its maximum value
#' @param MethWght output frequency-weighted (co)spectrum?

#' @return Return a (co)spectrum model after Massman, 2005 (in Lee, 2005).

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' Kaimal, J.C., Wyngaard, J.C., Izumi, Y. and Cote, O.R. (1972), Spectral characteristics of surface-layer turbulence. Q.J.R. Meteorol. Soc., 98: 563-589. https://doi.org/10.1002/qj.49709841707
#' Foken, Thomas. (2017). Micrometeorology.10.1007/978-3-642-25440-6.
#' Massman W. (2004) Concerning the Measurement of Atmospheric Trace Gas Fluxes with Open- and Closed-Path Eddy Covariance System: The WPL Terms and Spectral Attenuation. In: Lee X., Massman W., Law B. (eds) Handbook of Micrometeorology. Atmospheric and Oceanographic Sciences Library, vol 29. Springer, Dordrecht. https://doi.org/10.1007/1-4020-2265-4_7

#' @keywords Fast Fourier Transform, FFT, spectral

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#function to generate model (co)spectrum after Massman, 2005 (in Lee, 2005)
#continuous approximation of the Kaimal (1972) cospectra
########################################################
def.spec.modl <- function(
  #independent variable, preferabley f, but n is possible
  idep,
  #spectrum or cospectrum?
  MethSpec = c("spec", "cosp")[2],
  #stability parameter
  paraStbl,
  #frequency f at which fCO(f) reaches its maximum value
  FreqPeak = 0.1,
  #output frequency-weighted (co)spectrum?
  MethWght = TRUE
){
  
  #(inertial subrange) slope parameter
  #3/2 for -5/3 (spectra) power law
  if(MethSpec == "spec") paraSlp <- 3/2
  #3/4 for -7/3 (cospectra), 
  if(MethSpec == "cosp") paraSlp <- 3/4
  
  #broadness parameter
  #1/2 for unstable
  if(paraStbl <= 0) paraBrd <- 1/2  
  #7/6 for stable stratification 
  if(paraStbl > 0)  paraBrd <- 7/6
  
  #calculate non-scaled, frequency-weighted model Cospectrum (fCo or nCo)
  modlSpec <- (idep / FreqPeak) / (
    ( 1 + paraSlp * (idep / FreqPeak)^(2 * paraBrd) )^( (1/(2*paraBrd)) * ((paraSlp+1)/paraSlp) )
  )
  
  #(un)weight the (co)spectrum if necessary
  if(MethWght == FALSE) modlSpec <- modlSpec / idep
  
  #normalize to sum of 1
  modlSpec <- modlSpec / base::sum(modlSpec, na.rm=TRUE)
  
  #return output
  return(modlSpec)
  
  
  ########################################################	  
} #end of function
########################################################
