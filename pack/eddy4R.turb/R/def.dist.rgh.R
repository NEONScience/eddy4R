##############################################################################################
#' @title Aerodynamic roughness length

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Aerodynamic roughness length.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords eddy-covariance, turbulent flux

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
##############################################################################################

############################################################
#CALCULATION OF AERODYNAMIC ROUGHNESS LENGTH d_z_0 (NEEDED FOR K04 footprint model)
############################################################

roughness <- function(
  d_z_m,
  d_L_v_0,
  u_hor,
  u_star,
  sigma_range=c(-2, 1)
) {
  
  #integral over the universal function after Businger (1971) in the form of Hoegstroem (1988)
  #Foken (2006) Eq. (2.85) - (2.88)
  dum <- sapply(1:length(d_L_v_0), function(x) def.univ.func(distZaxsMeas=d_z_m[x],
                                                             distObkv=d_L_v_0[x],
                                                             RngStblObkv=sigma_range
  )
  )
  dum <- data.frame(t(dum))
  Psi <- unlist(dum$univFunc)
  sigma_flag <- unlist(dum$flagStblObkv)
  rm(dum)
  
  #calculation of roughness length d_z_0 [m]
  d_z_0 <- d_z_m / exp(eddy4R.base::Natu$VonkFokn * u_hor / u_star + Psi)
  names(d_z_0) <- "d_z_0"
  
  #return result
  return(d_z_0)
  
}

