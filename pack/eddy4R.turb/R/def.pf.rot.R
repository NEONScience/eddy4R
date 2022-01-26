


##############################################################################################
#' @title Definition function: Perform planar fit transformation

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com},
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Performs planar fit transformation on wind vector in right-hand body coordinate system, e.g. CSAT3: Positive from front, left, below.

#' @param dfVeloWind data.frame containing the horizontal (u,v) and vertical (z) wind velocities in ms-1
#' @param AngEnuYaxs The planar fit coefficient for pitch angle rotation - alpha (al)
#' @param AngEnuXaxs The planar fit coefficient for roll angle rotation - beta (be)
#' @param Ofst The planar fit coefficient for vertical velocity offset (b0)
#' 
#' @return A data.frame \code{dfVeloWindPf} containing data rotated using the planar fit transformation with the same dimensions as \code{dfVeloWind}.

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

def.pf.rot <- function(
#measured wind velocity vector [m s-1]
  dfVeloWind = data.frame(veloXaxs, veloYaxs, veloZaxs),
#pitch rotation angle [rad] - alpha (al)
  AngEnuYaxs = 0,
#roll rotation angle [rad] - beta (be)
  AngEnuXaxs = 0,
#vertical wind offset [m s-1] - b0
  Ofst = 0
) {

#mean offset correction for vertical wind
  dfVeloWind$veloZaxs <- dfVeloWind$veloZaxs - Ofst

#partial rotation matrix for the pitch angle (Wilczak et al., 2001, Eq. 2)
  mtrxAngYaxs <- c(base::cos(AngEnuYaxs), 0, -base::sin(AngEnuYaxs), 0, 1, 0, base::sin(AngEnuYaxs), 0, base::cos(AngEnuYaxs)) #Dmat
  mtrxAngYaxs  <- base::matrix(mtrxAngYaxs, nrow=3, ncol=3) #Dmat

#partial rotation matrix for the roll angle (Wilczak et al., 2001, Eq. 2)
  mtrxAngXaxs <- c(1,0, 0,0,base::cos(AngEnuXaxs), base::sin(AngEnuXaxs),0, -base::sin(AngEnuXaxs), base::cos(AngEnuXaxs))
  mtrxAngXaxs  <- base::matrix(mtrxAngXaxs, nrow=3, ncol=3)

#combination of pitch and roll angle rotation matrices (Wilczak et al., 2001, Eq. 36)
  mtrxRot <- t(mtrxAngYaxs) %*% t(mtrxAngXaxs) #Pmat

#perform planar-fit rotation (Wilczak et al., 2001, Eq. 35)
  dfVeloWindPf <- base::data.frame(t(mtrxRot %*% t(dfVeloWind)))
  base::dimnames(dfVeloWindPf)[[2]] <- base::dimnames(dfVeloWind)[[2]]

#return resulting wind vector
  return(dfVeloWindPf)

}

