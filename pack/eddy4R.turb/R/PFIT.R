##############################################################################################
#' @title Determine planar fit coefficients

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com},
#' Stefan Metzger \email{eddy4R.info@gmail.com}

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
##############################################################################################

#after "Ke_planar fit rotation angle calculation.r"

PFIT_det <- function(
  u,
  v,
  w
) {
  
  #perform regression
    LM <- lm(w ~ u + v)
    #LM <- robustbase::lmrob(w ~ u + v)
  
  #extract regression coefficients
    b0 <- coefficients(LM)[1]
    b1 <- coefficients(LM)[2]
    b2 <- coefficients(LM)[3]
  
  #determine cosine matrix elements
    p31 <- -b1 / sqrt(b1^2 + b2^2 + 1)
    p32 <- -b2 / sqrt(b1^2 + b2^2 + 1)
    p33 <-   1 / sqrt(b1^2 + b2^2 + 1)
  
  #solve cosine matrix for rotation angles [rad]
    alpha <- asin(p31)
    beta <-  atan(-p32 / p33)
    #alpha <- acos(sqrt(p32^2 + p33^2)) * 180 / pi
    #beta <-  asin(-p32 / sqrt(p32^2 + p33^2)) * 180 / pi
  
  #aggregate and return results
    out <- data.frame(al=alpha, be=beta, b0=b0)
    dimnames(out)[[1]] <- ""
    return(out)
  
}



##############################################################################################
#' @title Perform planar fit transformation

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com},
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Performs planar fit transformation on wind vector in right-hand body coordinate system, e.g. CSAT3: Positive from front, left, below.

#' @param Currently none

#' @return
#' Currently none

#' @references
#' Currently none

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
##############################################################################################

PFIT_apply <- function(
#measured wind velocity vector [m s-1]
  u_m = data.frame(xaxs, yaxs, zaxs),
#pitch rotation angle [rad]
  al = 0,
#roll rotation angle [rad]
  be = 0,
#vertical wind offset [m s-1]
  b0 = 0
) {

#mean offset correction for vertical wind
  u_m$zaxs <- u_m$zaxs - b0

#partial rotation matrix for the pitch angle (Wilczak et al., 2001, Eq. 2)
  Dmat <- c(cos(al), 0, -sin(al), 0, 1, 0, sin(al), 0, cos(al))
  Dmat <- matrix(Dmat, nrow=3, ncol=3)

#partial rotation matrix for the roll angle (Wilczak et al., 2001, Eq. 2)
  Cmat <- c(1,0, 0,0,cos(be), sin(be),0, -sin(be), cos(be))
  Cmat <- matrix(Cmat, nrow=3, ncol=3)

#combination of pitch and roll angle rotation matrices (Wilczak et al., 2001, Eq. 36)
  Pmat <- t(Dmat) %*% t(Cmat)

#perform planar-fit rotation (Wilczak et al., 2001, Eq. 35)
  u_p <- data.frame(t(Pmat %*% t(u_m)))
  dimnames(u_p)[[2]] <- dimnames(u_m)[[2]]

#return resulting wind vector
  return(u_p)

}

