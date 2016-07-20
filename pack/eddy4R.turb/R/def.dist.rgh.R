##############################################################################################
#' @title Aerodynamic roughness length

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description 
#' Function defintion. Aerodynamic roughness length.

#' @param \code{distZaxsMeas} Measurement height and of class "numeric". [m]
#' @param \code{distObkv} Monin-Obukhov length and of class "numeric". [m]
#' @param \code{veloXaxs} Mean along-axis horizontal wind speed and of class "numeric". For eddy4R uses case, \code{veloXaxs} after pitch and roll rotation and azimuth rotation into the mean wind [m s-1]
#' @param \code{veloFric} Friction velocity and of class "numeric". [m s-1]
#' @param \code{RngStblObkv} An object of class "numeric" containing the range (minimum and maximum) of the stability parameter. Defaults to  RngStblObkv = c(-2, 1).[-]

#' @return Estimated aerodynamic roughness length. [m]

#' @references
#' Businger, J. A., Wyngaard, J. C., Izumi, Y., and Bradley, E. F.: Flux-proﬁle relationships in the atmospheric surface layer, Journal of the Atmospheric Sciences, 28, 181–189, 1971. \cr
#' Foken, T.: Micrometeorology, Springer, Berlin, Heidelberg, 2008. \cr
#' Högström, U.: Non-dimensional wind and temperature proﬁles in the atmospheric surface layer: a re-evaluation, Boundary-Layer Meteorology, 42, 55–78, 1988. \cr

#' @keywords eddy-covariance, roughness length, turbulent flux

#' @examples
#' def.dist.rgh (distZaxsMeas = 13.5, distObkv = 45, veloXaxs = 3, veloFric = 0.8, RngStblObkv = c(-2, 1))

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
#CALCULATION OF AERODYNAMIC ROUGHNESS LENGTH distRgh (NEEDED FOR K04 footprint model)

def.dist.rgh <- function(
  distZaxsMeas,
  distObkv,
  veloXaxs,
  veloFric,
  RngStblObkv = c(-2, 1)
) {
  
  #integral over the universal function after Businger (1971) in the form of Högström (1988)
  #Foken (2008) Eq. (2.85) - (2.88)
  tmp <- sapply(1:length(distObkv), function(x) def.func.univ(distZaxsMeas = distZaxsMeas[x],
                                                             distObkv = distObkv[x],
                                                             RngStblObkv = RngStblObkv
  )
  )
  tmp <- data.frame(t(tmp))
  univFunc <- unlist(tmp$univFunc)
  flagStblObkv <- unlist(tmp$flagStblObkv)
  rm(tmp)
  
  #calculation of roughness length distRgh [m]
  distRgh <- distZaxsMeas / exp(eddy4R.base::Natu$VonkFokn * veloXaxs / veloFric + univFunc)
  names(distRgh) <- "distRgh"
  
  #return result
  return(distRgh)
  
}
