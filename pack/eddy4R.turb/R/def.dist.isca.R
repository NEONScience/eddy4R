##############################################################################################
#' @title Definition function: Integral length scales

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}
#' Kenny Pratt

#' @description 
#' Function defintion. Integral length scales.

#' @param \code{scalEddy}  A vector containing distances or times and of class "numeric". [m] or [s]
#' @param \code{data} A vector containing the input data. Of class "numeric" or "integer". [user-defined]
#' @param \code{veloXaxs} Mean along-axis horizontal wind speed. Only supplied when users define scalEddy in terms of time. If provided, of class "numeric", otherwise NULL. [m/s]


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
  scalEddy,
  data,
  veloXaxs = NULL
) {
  
  # test if units exist for input variables
  
  if(!("unit" %in% names(attributes(scalEddy)))) {
    
    stop("def.dist.isca(): scalEddy is missing unit attribute.")
  }
  
  # test for correct units of input variables
  if(attributes(scalEddy)$unit != "m") {
    
    if(attributes(scalEddy)$unit != "s") {
    
    stop("def.dist.isca(): input units are not matching internal units, please check.")
      
    }
    
  }
  
  # If user supplies a time for scalEddy, but does not provide mean wind speed measurement, throw error.
  if(attributes(scalEddy)$unit == "s" && is.null(veloXaxs) ) {  
    
    stop("def.dist.isca(): input units for scalEddy are in [s], therefore user must provide wind speed measurements in order for scalEddy to be converted to [m].")
    
  }
  
  # If user specifies a time for scalEddy, convert to a distance using frozen turbulence hypothesis
  if(attributes(scalEddy)$unit == "s") {  
    
    # test for correct units of input variables
    if(attributes(veloXaxs)$unit != "m s-1") {
      
      stop("def.dist.isca(): input units are not matching internal units, please check.")
      
    }
    
    scalEddy = scalEddy * veloXaxs
    
  }
  
  #fill gaps via linear interpolation
  data <- stats::approx(scalEddy, data, xout=scalEddy)[[2]]
  
  #get rid of NAs at start and end
  tmp <- stats::na.omit(base::data.frame(scalEddy=scalEddy, data=data))
  scalEddy <- tmp$scalEddy
  data <- tmp$data
  rm(tmp)
  
  #demeaning and detrending
  data <- stats::lm(data ~ scalEddy)$residuals
  
  #path through air [m] per increment, the stepwidth of the integral scale
  incr <- base::max(scalEddy - base::min(scalEddy)) / base::length(scalEddy)
  
  #calculate auto-correlation function
  lag <- 10; crit <- 1
  while(crit > 0) {
    lag <- lag * 2
    rptCorr <- stats::acf(data, lag.max = lag, type = "correlation", plot = FALSE, na.action = na.fail, demean = TRUE)
    crit <- base::min(rptCorr$acf)
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
  posZero <- EMD::extrema(y=c(rptCorr$acf, sin(1:10)))$cross[1,2]
  
  #for each cell, weight distance increment with correlation coefficient
  distIsca <- base::sum(rptCorr$acf[1:posZero]) * incr
  #alternatively: all in one, assume that correlation monotonously decreases after peak
  #distIsca <- base::max(base::cumsum(rptAcf$acf)) * incr
  
  
  # assign output unit
  attributes(distIsca)$unit <- "m"
  
  #return result
  return(distIsca)
  
}