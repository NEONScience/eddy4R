##############################################################################################
#' @title Definition function: Derive planar fit coefficients

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com},
#' Stefan Metzger \email{eddy4R.info@gmail.com}
#' David Durden \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Determines planar fit coefficients on wind vector in right-hand body coordinate system, e.g. CSAT3: Positive from front, left, below. Met: positive from west, south and below. The coefficients don't have to be in the East-North-Up (ENU) coordinate system (AngEnuXaxs); however, the names are consistent with NEON data terminology and in our data pipeline are in ENU.

#' @param veloXaxs numeric vector containing the horizontal (met: u) wind velocities in ms-1
#' @param veloYaxs numeric vector containing the horizontal (met: v) wind velocities in ms-1
#' @param veloZaxs numeric vector containing the vertical (met: z) wind velocities in ms-1

#' @return A data.frame \code{coefPf} containing the planar fit coefficients (\code{angEnuYaxs} = Alpha,\code{angEnuXaxs} = Beta,\code{Ofst} = Offset).

#' @references 
#' Wilczak et al., 2001
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords planar fit, coordinate rotation, coordinate transformation

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Ke Xu (2014-03-07)
#     original creation (Ke_planar fit rotation angle application.r) after earlier template from Stefan
#   Stefan Metzger (2015-01-07)
#     clean-up and packaging as function
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   David Durden (2022-01-18)
#     eddy4R.turb refactoring extracting functions to files and term updates
##############################################################################################

#after "Ke_planar fit rotation angle calculation.r"

def.pf.derv.coef <- function(
  veloXaxs, #u
  veloYaxs, #v
  veloZaxs #w
) {
  
  #perform regression
  modlLin <- stats::lm(veloZaxs ~ veloXaxs + veloYaxs)
  #LM <- robustbase::lmrob(w ~ u + v)
  
  #extract regression coefficients
  coef00 <- stats::coefficients(modlLin )[1] #b0
  coef01 <- stats::coefficients(modlLin )[2] #b1
  coef02 <- stats::coefficients(modlLin )[3] #b2
  
  #determine cosine matrix elements
  mtrx31 <- -coef01 / base::sqrt(coef01^2 + coef02^2 + 1) #p31
  mtrx32 <- -coef02 / base::sqrt(coef01^2 + coef02^2 + 1) #p32
  mtrx33 <-   1 / base::sqrt(coef01^2 + coef02^2 + 1) #p33
  
  #solve cosine matrix for rotation angles [rad]
  angEnuYaxs <- base::asin(mtrx31) #alpha
  angEnuXaxs <-  base::atan(-mtrx32 / mtrx33) #beta
  #alpha <- acos(sqrt(p32^2 + p33^2)) * 180 / pi
  #beta <-  asin(-p32 / sqrt(p32^2 + p33^2)) * 180 / pi
  
  #aggregate and return results
  CoefPf <- base::data.frame(AngEnuYaxs=angEnuYaxs, AngEnuXaxs=angEnuXaxs, Ofst=coef00) #Alpha (al), beta (be), offset(b0) 
  base::dimnames(CoefPf)[[1]] <- ""
  return(CoefPf)
  
}
