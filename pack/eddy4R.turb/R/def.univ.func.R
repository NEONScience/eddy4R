##############################################################################################
#' @title Integral over the universal function

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Integral over the universal function.

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
#CALCULATION OF THE INTEGRAL OVER THE UNIVERSAL FUNCTION
#after Businger (1971) in the form of Hogstrom (1988)
#from Foken (2006) Eq. (2.85) - (2.88)
############################################################

univfun <- function(
  d_z_m,
  d_L_v_0,
  sigma_range=c(-2, 1)
) {
  
  #calculation of local stability sigma [-]
  sigma <- d_z_m / d_L_v_0
  
  #assign a flag for universal function being calculated within defined stability range
  sigma_flag <- 0
  
  #instable case
  if(!is.na(sigma) & sigma < 0) {
    
    #catch cases for which parametrization is not defined
    if (sigma < sigma_range[1]) { 
      sigma <- sigma_range[1]
      sigma_flag <- -1
    }
    
    #actual calculation
    x <- (1 - 19.3 * sigma)^(1/4)
    y <- 0.95 * (1 - 11.6 * sigma)^(1/2)
    Psi <- log(((1 + x^2) / 2) * ((1 + x) / 2)^2) - 2 * atan(x) + pi / 2
    rm(x, y)
    
    #stable case
  } else {
    
    #catch cases for which parametrization is not defined
    if (!is.na(sigma) & sigma > sigma_range[2]) {
      sigma <- sigma_range[2]
      sigma_flag <- 1
    }
    
    #actual calculation
    Psi <- -6 * sigma
    
  }
  
  #return result
  OUT <- list(Psi=Psi, sigma_flag=sigma_flag)
  return(OUT)
  
  #universal function for momentum exchange after Skeib (1980)
  #not integrated, only defined for sigma_range <- c(-2,2)
  #only defined for sigma_range <- c(-2,2)
  
  #any case not fulfilling sigma_range
  #Psi <- NA
  #labile case
  #if(sigma > sigma_range[1] & sigma < -0.0625) Psi <- (-sigma / 0.0625)^(-1/4)
  #neutral case
  #if(sigma >= -0.0625 & sigma <= 0.125) Psi <- 1
  #stable case
  #if(sigma > 0.125 & sigma < sigma_range[2]) Psi <- (sigma / 0.125)
  
}
