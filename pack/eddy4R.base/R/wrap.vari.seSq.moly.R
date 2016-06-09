##############################################################################################
#' @title Calculate aggregated standard deviation and standard error from diurnal cycle to monthly timescale

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com} \cr

#' @description 
#' Function wrapper. Calculate aggregated standard deviation and standard error from diurnal cycle to monthly timescale

#' @param \code{rpt} The input parameter list containing \code{mdc}, also used to store reported output.
#' @param \code{mdc} The input parameter data frame containing \code{mean}, \code{vari}, and \code{seSq}.
#' @param \code{mean} mean of the scalar of 24 hours in a diurnal cycle, e.g. aggregated mean of sensible heat over 0:00 - 1:00. [same unit as scalar, e.g. W m-2, gC m-2 s-1].
#' @param \code{vari} variance of the scalar of 24 hours in a diurnal cycle, e.g. aggregated variance of sensible heat over 0:00 - 1:00. [square of unit of scalar, e.g. (W m-2)^2].
#' @param \code{seSq} square of standard error of the scalar of 24 hours in a diurnal cycle, e.g. aggregated square of standard error of sensible heat over 0:00 - 1:00. [square of unit of scalar, e.g. (W m-2)^2].


#' @return Returns object of class "dataframe" [1, 1:5] containing mean, variance, standard deviation, square of standard error, and standard error of the scalar at monthly timescale

#' @references Currently none

#' @keywords standard deviation, standard error,  aggregate

#' @examples
#' aa <- wrap.sd.se.moly(
#'  rpt = list(
#'   mdc = data.frame(
#'     mean = rnorm(24) 
#'     vari = rnorm(24),   
#'     seSq = rnorm(24)
#'   )
#'  )
#' )

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
#    change input from list to data.frame
##############################################################################################



wrap.sd.se.moly <- function(
  rpt = list(
    mdc = data.frame(
      mean = rpt$mdc$mean, 
      vari = rpt$mdc$vari,   
      seSq = rpt$mdc$seSq
    )
  )
  
){
  
  
  ###############  monthly aggregated ######################
  rpt$moly <- list()
  
  whr <- which(!is.na(rpt$mdc$mean))
  
  if(length(whr) > 0){
    rpt$moly <- def.sd.se.agr(
      mean = rpt$mdc$mean[whr],
      vari = rpt$mdc$vari[whr],
      seSq = rpt$mdc$seSq[whr]
    )
  } else {
    rpt$moly <- list(
      mean = NaN,
      variExt = NaN,
      variIntl = NaN,
      vari = NaN,
      seSq = NaN
    )
  }
  
  
  return(rpt)
}
