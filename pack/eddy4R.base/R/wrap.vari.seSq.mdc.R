##############################################################################################
#' @title Calculate aggregated variance and squared standard error from hourly resoltion to diurnal cycle

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com} \cr

#' @description 
#' Function wrapper. Calculate aggregated variance and squared standard error from hourly to diurnal cycle

#' @param \code{rpt} The input parameter list containing \code{hrly}, also used to store reported output.
#' @param \code{hrly} The input parameter data frame containing \code{DOY}, \code{mean}, \code{vari}, and \code{seSq}.
#' @param \code{DOY} input DOY time in UTC [float number].
#' @param \code{mean} mean of the scalar at hourly resolution, e.g. hourly mean of sensible heat. [same unit as scalar, e.g. W m-2, gC m-2 s-1].
#' @param \code{vari} variance of the scalar at hourly resolution, e.g. hourly variance of sensible heat. [square of unit of scalar, e.g. (W m-2)^2].
#' @param \code{seSq} square of standard error of the scalar at hourly resolution, e.g. hourly square of standard error of sensible heat. [square of unit of scalar, e.g. (W m-2)^2].
#' @param \code{zone} The time zone of the location [unitless].


#' @return Returns object of class "dataframe" [24, 1:4] containing  hour, mean, variance, square of standard error of the scalar at diurnal cycle

#' @references Currently none

#' @keywords variance, squared standard error,  aggregate, hourly, aggregated diurnal cycle

#' @examples
#' aa <- wrap.sd.se.mdc(
#'  rpt = list(
#'    hrly = data.frame(
#'      DOY = (0:47)/24,  
#'      mean = rnorm(2 * 24), 
#'      vari = rnorm(2 * 24),   
#'      seSq = rnorm(2 * 24)
#'    )),
#'  zone = 6
#'  )

#' @seealso Currently none

#' @export


# changelog and author contributions / copyrights
#   Ke Xu (TBD)
#     original creation (2016-02-12)
#   Ke Xu (2016-04-11)
#     apply eddy4R code-style convention
#   Ke Xu (2016-04-20)
#    re-formualtion into a function() and several wrapper(): wrap.sd.se.hrly, wrap.sd.se.mdc, wrap.sd.se.moly to allow broader use
#   Ke Xu (2016-05-10)
#    Use data.frame as input and use sapply for batch computing
##############################################################################################



wrap.sd.se.mdc <- function(
  rpt = list(
    hrly = data.frame(
      DOY,  
      mean, 
      vari,   
      seSq
    )
  ),
  zone = 6
  
){
  
  
  ################  Prepare hourly data  #####################
  
  #translate from UTC to local time
  rpt$hrly$DOY <- rpt$hrly$DOY - zone/24   
  rpt$hrly$date <- floor(rpt$hrly$DOY) 
  #translate from UTC to local time
  rpt$hrly$hour <- (rpt$hrly$DOY - floor(rpt$hrly$DOY))*24 
  
  
  
  ################ aggregated diurnal cycle #####################
  rpt$mdc <- list()
  
  rpt$mdc <- as.data.frame(t(as.matrix(sapply(0:23, function(hh)  
  { #select all measurements in a particular hour, e.g. 0:00-1:00, within the whole measurement period.
    whr <- which(floor(rpt$hrly$hour) == hh & !is.na(rpt$hrly$mean))
    
    if(length(whr) > 0){
      
      tmp <-  def.sd.se.agr(
        mean = rpt$hrly$mean[whr], 
        vari = rpt$hrly$vari[whr],
        seSq = rpt$hrly$seSq[whr]
      )
      
      list(
        hour = floor(rpt$hrly$hour[whr][1]),
        mean = tmp$mean,
        vari = tmp$vari,
        seSq = tmp$seSq
      )
      
    } else {
      
      list(
        hour = NaN,
        mean = NaN,
        vari = NaN,
        seSq = NaN
        
      )
      
    }
  }))))
  
  return(rpt)
}


