##############################################################################################
#' @title Definition function: Integral length scales

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}
#' Kenny Pratt

#' @description 
#' Function defintion. Integral length scales.

#' @param \code{distHorFlht}  A vector or data frame containing flight distances and of class "numeric". [m]
#' @param \code{data} A vector containing the input data. Of class "numeric" or "integer". [user-defined]


#' @return Estimated Integral length scale.

#' @references 
#' J. Bange, F. Beyrich, D.A.M. Engelbart (2002) Airbourne measurements of turbulent fluxes during LITFASS-98: Comparison with ground measurements and remote sensing in a case study. Theor. Appl. Climatol., 73, 35-51. \cr 
#' D.H. Lenschow, B.B.Stankov. (1986) Length Scales in the Convective Boundary Layer. Journal of the Atmospheric Sciences. 43:12, 1198-1209 \cr

#' @keywords eddy-covariance, turbulent flux

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Ke Xu (2016-09-19)
#     Add two arguments PltfEc and flagCh4 to adjust tower data
#   Kenny Pratt(2017-02-03)
#     Renaming in accordance with Eddy4R conventions
##############################################################################################
def.dist.isca <- function(
  distHorFlht,
  data
) {
  
  #fill gaps via linear interpolation
  data <- stats::approx(distHorFlht, data, xout=distHorFlht)[[2]]
  
  #get rid of NAs at start and end
  tmp <- stats::na.omit(base::data.frame(distHorFlht=distHorFlht, data=data))
  distHorFlht <- tmp$distHorFlht
  data <- tmp$data
  rm(tmp)
  
  #demeaning and detrending
  data <- stats::lm(data ~ distHorFlht)$residuals
  
  #path through air [m] per increment, the stepwidth of the integral scale
  incr <- base::max(distHorFlht - base::min(distHorFlht)) / base::length(distHorFlht)
  
  #calculate auto-correlation function
  lag <- 10; crit <- 1
  while(crit > 0) {
    lag <- lag * 2
    rptAcf <- stats::acf(data, lag.max = lag, type = "correlation", plot = FALSE, na.action = na.fail, demean = TRUE)
    crit <- base::min(rptAcf$acf)
  }
  
  #integral length scale: typical size of the largest or most energy-transporting eddies.
  #a) integral over acf until first zero crossing (Bange, 2002);
  #b) first maximum of the integral (Lenschow, 1986);
  #find first zero crossing
  #      posZero <- rptAcf$lag[zeroCross(rptAcf$acf, slope="negative")[1]]
  #      posZero <- rptAcf$lag[GenKern::nearest(x=rptAcf$acf, xval=0)[1]]
  require(EMD)
  #data needs have extrema, otherwise it will not identify the zero crossing
  #hence attaching sin(1:10) to the end
  posZero <- EMD::extrema(y=c(rptAcf$acf, sin(1:10)))$cross[1,2]
  
  #for each cell, weight distance increment with correlation coefficient
  distIsca <- base::sum(rptAcf$acf[1:posZero]) * incr
  #alternatively: all in one, assume that correlation monotonously decreases after peak
  #distIsca <- base::max(base::cumsum(rptAcf$acf)) * incr
  
  #return result
  return(distIsca)
  
}