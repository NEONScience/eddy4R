##############################################################################################
#' @title Definition function: Determine planar fit coefficients

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com},
#' Stefan Metzger \email{eddy4R.info@gmail.com}
#' David Durden 

#' @description 
#' Function defintion. Determines planar fit coefficients on wind vector in right-hand body coordinate system, e.g. CSAT3: Positive from front, left, below.

#' @param Currently none

#' @return Currently none

#' @references Currently none

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
  modlLin <- lm(veloZaxs ~ veloXaxs + veloYaxs)
  #LM <- robustbase::lmrob(w ~ u + v)
  
  #extract regression coefficients
  coef00 <- coefficients(modlLin )[1]
  coef01 <- coefficients(modlLin )[2]
  coef02 <- coefficients(modlLin )[3]
  
  #determine cosine matrix elements
  mtrx31 <- -coef01 / sqrt(coef01^2 + coef02^2 + 1)
  mtrx32 <- -coef02 / sqrt(coef01^2 + coef02^2 + 1)
  mtrx33 <-   1 / sqrt(coef01^2 + coef02^2 + 1)
  
  #solve cosine matrix for rotation angles [rad]
  angEnuYaxs <- asin(mtrx31)
  angEnuXaxs <-  atan(-mtrx32 / mtrx33)
  #alpha <- acos(sqrt(p32^2 + p33^2)) * 180 / pi
  #beta <-  asin(-p32 / sqrt(p32^2 + p33^2)) * 180 / pi
  
  #aggregate and return results
  coefPf <- data.frame(AngEnuYaxs=angEnuYaxs, AngEnuXaxs=angEnuXaxs, Ofst=coef00) #Should coefficients be capitalized?
  dimnames(coefPf)[[1]] <- ""
  return(coefPf)
  
}
