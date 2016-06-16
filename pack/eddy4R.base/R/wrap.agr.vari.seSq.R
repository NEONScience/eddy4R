##############################################################################################
#' @title Calculate aggregated variance and squared standard error from a small to large temporal scale.

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com} \cr

#' @description 
#' Function wrapper. Calculate aggregated variance and squared standard error from a finer to a coarser temporal resolution: from minutely to hourly resolution, from hourly to diurnal cycle, and from diurnal cycle to monthly. The input data can be irregular, because data expansion is included in the current wrapper to generate unbiased mean, variance and squared standard error. 


#' @param \code{data} Dataframe of type numeric containing column vectors \code{DOY}, \code{mean}, \code{vari}, and \code{seSq} of equal length.
#' @param \code{DOY} input DOY time in UTC [float number].
#' @param \code{mean} Vector of type numeric. Means of the variable of interest at finer resolution, e.g. minutely [user-defined].
#' @param \code{vari} Vector of type numeric. Variances of the variable of interest at finer resolution, e.g. minutely [user-defined^2].
#' @param \code{seSq} squared standard error of the scalar at minutely resolution, e.g. minutely square of standard error of sensible heat. [square of unit of scalar, e.g. (W m-2)^2].
#' @param \code{zone} The time zone of the location, which is the offset from Coordinated Universal Time (UTC) by a whole number of hours. If local time is ahead (behind) the Greenwich mean time, zone is a positive (negative) number. e.g. CST" [hour]. a time offset in Wisconsin, in the North American Central Time Zone, would be -6.
#' @param \code{MethAgr} String type. One of three choices: "hrly", "mdc", "moly". "hrly" represents aggregation from minutely to hourly resolution; "mdc" represents aggregation from hourly to diurnal cycle; "moly" represents aggregation from diurnal cycle to monthly resolution.

#' @return Returns object of class "dataframe": if MethAgr is "hrly", the dataframe is [n, 1:5] containing date, hour, mean, variance, square of standard error of the scalar at hourly resolution
#'                                              if MethAgr is "mdc", the dataframe is [n, 1:4] containing hour, mean, variance, square of standard error of the scalar at diurnal cycle
#'                                              if MethAgr is "moly", the dataframe is [n, 1:3] containing mean, variance, square of standard error of the scalar at monthly scale

#' @references 
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords variance, standard error,  aggregate, minutely, hourly, diurnal cycle

#' @examples
#' wrap.agr.vari.seSq(
#' data = data.frame(
#'   DOY = c(25:124)/24, 
#'   mean = rnorm(100),  
#'   vari = rnorm(100)^2,   
#'   seSq = rnorm(100)^2
#' ), 
#' zone = -6,
#' MethAgr = "hrly"
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
#   Ke Xu (2016-06-09)
#    combine wrap.hrly.vari.seSq.R, wrap.mdc.vari.seSq.R, and wrap.moly.vari.seSq.R into wrap.agr.vari.seSq.R
##############################################################################################

wrap.agr.vari.seSq <- function(
  
  data = data.frame(
    DOY,
    mean, 
    vari,   
    seSq
  ), 
  zone, 
  MethAgr
  
){
  
  rpt <- list()
  
  ################  Prepare data  #####################
  if (MethAgr == "hrly" | MethAgr == "mdc"){
    
    #translate from UTC to local time
    data$DOY <- data$DOY + zone/24   
    data$date <- floor(data$DOY) 
    #translate from UTC to local time
    data$hour <- (data$DOY - floor(data$DOY))*24 
    
  }
  
  #prepare idx for sapply function
  if (MethAgr == "hrly"){
    idx <- base::as.vector(base::sapply(base::unique(data$date), function(x) (x + c(0:23)/24)))
    hour <- 24 * (idx-floor(idx))
    date <- floor(idx)
  }
  
  if (MethAgr == "mdc") idx <- c(0:23)
  
  if (MethAgr == "moly") idx <- 1
  
  
  # calculate aggregated mean, variance, squared standard error
  tmp <- list()
  tmp <- sapply(1:length(idx), function(x) {
    
    if (MethAgr == "hrly") whr <- which(abs(floor(data$hour) - hour[x]) < 0.1 & abs(data$date - date[x]) < 0.1)
    
    if (MethAgr == "mdc") whr <- which(abs(data$hour - (x-1)) < 0.1)
    
    if (MethAgr == "moly") whr <- c(1:length(data$mean))
    
    eddy4R.base::def.agr.vari.seSq(data = data.frame(
      mean = data$mean[whr],
      vari = data$vari[whr],
      seSq = data$seSq[whr]
    ))
    
  })
  
  #prepare the reported data frame
  rpt <- data.frame(
    mean = base::unlist(tmp["mean",]),
    vari = base::unlist(tmp["vari",]),
    seSq = base::unlist(tmp["seSq",])
  )
  
  if (MethAgr == "hrly"){
    rpt <- data.frame(
      date = date,
      hour = hour,
      rpt
    )
  }
  
  if (MethAgr == "mdc"){
    rpt <- data.frame(
      hour = idx,
      rpt
    )
  }
  
  return(rpt)
  
}
