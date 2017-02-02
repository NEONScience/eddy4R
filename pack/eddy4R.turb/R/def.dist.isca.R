
##############################################################################################
#' @title Definition function: Integral length scales

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Integral length scales.

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
#   Ke Xu (2016-09-19)
#     Add two arguments PltfEc and flagCh4 to adjust tower data
##############################################################################################

############################################################
#INTEGERAL LENGTH SCALES
############################################################


INTsca <- function(
  d_xy_flow,
  depe
) {
  
  #fill gaps via linear interpolation
  depe <- approx(d_xy_flow, depe, xout=d_xy_flow)[[2]]
  
  #get rid of NAs at start and end
  dum_NA <- na.omit(data.frame(d_xy_flow=d_xy_flow, depe=depe))
  d_xy_flow <- dum_NA$d_xy_flow
  depe <- dum_NA$depe
  rm(dum_NA)
  
  #demeaning and detrending
  depe <- lm(depe ~ d_xy_flow)$residuals
  
  #path through air [m] per increment, the stepwidth of the integral scale
  incr <- max(d_xy_flow - min(d_xy_flow)) / length(d_xy_flow)
  
  #calculate auto-correlation function
  lag <- 10; crit <- 1
  while(crit > 0) {
    lag <- lag * 2
    ACF <- acf(depe, lag.max = lag, type = "correlation", plot = FALSE, na.action = na.fail, demean = TRUE)
    crit <- min(ACF$acf)
  }
  
  #integral length scale: typical size of the largest or most energy-transporting eddies.
  #a) integral over acf until first zero crossing (Bange, 2002);
  #b) first maximum of the integral (Lenschow, 1986);
  #find first zero crossing
  #      whr_zero <- ACF$lag[zeroCross(ACF$acf, slope="negative")[1]]
  #      whr_zero <- ACF$lag[GenKern::nearest(x=ACF$acf, xval=0)[1]]
  require(EMD)
  #data needs have extrema, otherwise it will not identify the zero crossing
  #hence attaching sin(1:10) to the end
  whr_zero <- EMD::extrema(y=c(ACF$acf, sin(1:10)))$cross[1,2]
  
  #for each cell, weight distance increment with correlation coefficient
  I <- sum(ACF$acf[1:whr_zero]) * incr
  #alternatively: all in one, assume that correlation monotonously decreases after peak
  #I <- max(cumsum(ACF$acf)) * incr
  
  #return result
  return(I)
  
}