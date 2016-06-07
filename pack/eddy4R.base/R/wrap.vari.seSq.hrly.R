##############################################################################################
#' @title Calculate aggregated variance and squared standard error from minutely to hourly resolution

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com} \cr

#' @description 
#' Function wrapper. Calculate aggregated variance and squared standard error from minutely to hourly resolution

#' @param \code{rpt} The input parameter list containing \code{mily}, also used to store reported output.
#' @param \code{mily} The input parameter data frame containing \code{DOY}, \code{mean}, \code{vari}, and \code{seSq}.
#' @param \code{DOY} input DOY time in UTC [float number].
#' @param \code{mean} mean of the scalar at minutely resolution, e.g. minutely mean of sensible heat. [same unit as scalar, e.g. W m-2, gC m-2 s-1].
#' @param \code{vari} variance of the scalar at minutely resolution, e.g. minutely variance of sensible heat. [square of unit of scalar, e.g. (W m-2)^2].
#' @param \code{seSq} squared standard error of the scalar at minutely resolution, e.g. minutely square of standard error of sensible heat. [square of unit of scalar, e.g. (W m-2)^2].
#' @param \code{zone} The time zone of the location [unitless].

#' @return Returns object of class "list" [n, 1:5] containing date, hour, mean, variance, square of standard error of the scalar at hourly resolution

#' @references Currently none

#' @keywords variance, standard error,  aggregate, minutely, hourly

#' @examples
#' aa <- wrap.vari.seSq.hrly(
#'   rpt = list(
#'     mily = data.frame(
#'       DOY = (0:59)/60/24, 
#'       mean = rnorm(60), 
#'       vari = rnorm(60)^2,   
#'       seSq = rnorm(60)^2
#'     )),
#'   zone = 6
#' )

#' @seealso Currently none

#' @export


# changelog and author contributions / copyrights
#   Ke Xu (TBD)
#     original creation (2016-02-12)
#   Ke Xu (2016-04-11)
#     apply eddy4R code-style convention
#   Ke Xu (2016-04-20)
#    re-formualtion into a function() and several wrapper(): wrap.vari.seSq.hrly, wrap.vari.seSq.mdc, wrap.vari.seSq.moly to allow broader use
#   Ke Xu (2016-05-10)
#    change the name to wrap.vari.seSq.hrly and re-formulation again to code-style convention
##############################################################################################



wrap.vari.seSq.hrly <- function(
  rpt = list(
    mily = data.frame(
      DOY, 
      mean, 
      vari,   
      seSq
    )),
  zone
  
){
  
  
  ################  Prepare minutely data  #####################
  
  #translate from UTC to local time
  rpt$mily$DOY <- rpt$mily$DOY - zone/24   
  rpt$mily$date <- floor(rpt$mily$DOY) 
  #translate from UTC to local time
  rpt$mily$hour <- (rpt$mily$DOY - floor(rpt$mily$DOY))*24 
  
  
  ################  hourly aggregated  #####################
  
  rpt$hrly <- list()
  tmp <- list()
  #idx for sapply function
  idx <- as.vector(sapply(unique(rpt$mily$date), function(xx) (xx + c(0:23)/24)))
  
  rpt$hrly <- sapply(idx, function(xx) {
     
    hh <- 24 * (as.integer(xx-floor(xx)))
    dd <- floor(xx)
    whr <- which(floor(rpt$mily$hour) == hh & rpt$mily$date == dd)
    
    if(length(whr) > 0){
      
      tmp <- def.vari.seSq.agr(data = data.frame(
        mean = rpt$mily$mean[whr],
        vari = rpt$mily$vari[whr],
        seSq = rpt$mily$seSq[whr]
      ))
      
      list(
        date = rpt$mily$date[whr][1],
        hour = rpt$mily$hour[whr][1],
        mean = tmp$mean,
        vari = tmp$vari,
        seSq = tmp$seSq
      )
      
    } else {
      
      list(
        date = NaN,
        hour = NaN,
        mean = NaN,
        vari = NaN,
        seSq = NaN
      )
    }
    
    
  })
  
  return(rpt)
}


