##############################################################################################
#' @title Integral over the universal function

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description 
#' Function defintion. Calculation of the integral over the universal function after Businger et. al. (1971) in the form of Högström (1988).

#' @param \code{distZaxsMeas} Measurement height and of class "numeric". [m]
#' @param \code{distObkv} Monin-Obukhov length and of class "numeric". [m]
#' @param \code{RngStblObkv} An object of class "numeric" containing the range (minimum and maximum) of the stability parameter. Defaults to  RngStblObkv = c(-2, 1).[-]

#' @return The function returns a named list containing the following:\cr
#' univFunc = A list object of class "numeric" containing the integral over the universal function value. \cr
#' flagStblObkv = A list object of class "numeric" containing the flag which indicate how the universal function being calculated (0 = calculated using the stability parameter within defined in \code{RngStblObkv}, 1 = calculated using the minimum stability parameter defined in \code{RngStblObkv}, and -1 = calculated using the maximum stability parameter defined in \code{RngStblObkv}). \cr

#' @references 
#' Businger, J. A., Wyngaard, J. C., Izumi, Y., and Bradley, E. F.: Flux-proﬁle relationships in the atmospheric surface layer, Journal of the Atmospheric Sciences, 28, 181–189, 1971. \cr
#' Foken, T.: Micrometeorology, Springer, Berlin, Heidelberg, 2008. \cr
#' Högström, U.: Non-dimensional wind and temperature proﬁles in the atmospheric surface layer: a re-evaluation, Boundary-Layer Meteorology, 42, 55–78, 1988. \cr

#' @keywords eddy-covariance, turbulent flux, universal function

#' @examples 
#' def.func.univ(distZaxsMeas = 13.5,distObkv = 45 ,RngStblObkv = c(-2,1))

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Natchaya P-Durden (2016-06-20)
#     Initail naming convention for eddy4R
##############################################################################################
#CALCULATION OF THE INTEGRAL OVER THE UNIVERSAL FUNCTION
#after Businger (1971) in the form of Hogstrom (1988)
#from Foken (2008) Eq. (2.85) - (2.88)

def.func.univ <- function(
  distZaxsMeas,
  distObkv,
  RngStblObkv = c(-2, 1)
) {
  
  #calculation of local stability
  stblObkv <- distZaxsMeas / distObkv
  
  #assign a flag for universal function being calculated within defined stability range
  flagStblObkv <- 0
  
  #defined local constants
  CnstLoc <- list(Cnst01 = 19.3, Cnst02 = 1/4, Cnst03 = 0.95, Cnst04 = 11.6, Cnst05 = 1/2, Cnst06 = -6)
  
  #instable case
  if(!is.na(stblObkv) & stblObkv < 0) {
    
    #catch cases for which parametrization is not defined
    if (stblObkv < RngStblObkv[1]) { 
      stblObkv <- RngStblObkv[1]
      flagStblObkv <- -1
    }
    
    #actual calculation
    x <- (1 - CnstLoc$Cnst01 * stblObkv)^(CnstLoc$Cnst02)
    y <- CnstLoc$Cnst03 * (1 - CnstLoc$Cnst04 * stblObkv)^(CnstLoc$Cnst05)
    univFunc <- log(((1 + x^2) / 2) * ((1 + x) / 2)^2) - 2 * atan(x) + pi / 2
    rm(x, y)
    
    #stable case
  } else {
    
    #catch cases for which parametrization is not defined
    if (!is.na(stblObkv) & stblObkv > RngStblObkv[2]) {
      stblObkv <- RngStblObkv[2]
      flagStblObkv <- 1
    }
    
    #actual calculation
    univFunc <- CnstLoc$Cnst06 * stblObkv
    
  }
  
  #return result
  rpt <- list(univFunc=univFunc, flagStblObkv=flagStblObkv)
  return(rpt)
  
  #universal function for momentum exchange after Skeib (1980)
  #not integrated, only defined for RngStblObkv <- c(-2,2)
  #only defined for RngStblObkv <- c(-2,2)
  
  #any case not fulfilling RngStblObkv
  #univFunc <- NA
  #labile case
  #if(stblObkv > RngStblObkv[1] & stblObkv < -0.0625) univFunc <- (-stblObkv / 0.0625)^(-1/4)
  #neutral case
  #if(stblObkv >= -0.0625 & stblObkv <= 0.125) univFunc <- 1
  #stable case
  #if(stblObkv > 0.125 & stblObkv < RngStblObkv[2]) univFunc <- (stblObkv / 0.125)
  
}
