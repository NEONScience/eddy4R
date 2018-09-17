##############################################################################################
#' @title Wrapper function: Calculate aggregated variance and squared standard error from a small to large temporal scale.

#' @author
#' Ke Xu \email{xuke2012abroad@gmail.com}

#' @description 
#' Function wrapper. Calculate aggregated variance and squared standard error from a finer to a coarser temporal resolution: from minutely to hourly resolution, from hourly to diurnal cycle, and from diurnal cycle to monthly. The input data can be irregular, because data expansion is included in the current wrapper to generate unbiased mean, variance and squared standard error. 


#' @param data Dataframe of type numeric containing column vectors \code{timeDoyDecm}, \code{mean}, and \code{vari} of equal length.
#' @param timeDoyDecm input decimal doy time in UTC [float number].
#' @param mean Vector of type numeric. Means of the variable of interest at finer resolution, e.g. minutely [user-defined].
#' @param vari Vector of type numeric. Variances of the variable of interest at finer resolution, e.g. minutely [user-defined^2].
#' @param zone The time zone of the location, which is the offset from Coordinated Universal Time (UTC) by a whole number of hours. If local time is ahead (behind) the Greenwich mean time, zone is a positive (negative) number. e.g. CST" [hour]. a time offset in Wisconsin, in the North American Central Time Zone, would be -6.
#' @param MethAgr String type. One of three choices: "agrHour", "mdc", "agrMnth". "agrHour" represents aggregation from minutely to hourly resolution; "mdc" represents aggregation from hourly to diurnal cycle; "agrMnth" represents aggregation from diurnal cycle to monthly resolution.

#' @return Returns object of class "dataframe": cr\
#' if \code{MethAgr} is "agrHour", the dataframe is [n, 1:5] containing timeDoyIntg, hour, mean, variance, square of standard error of the scalar at hourly resolution \cr
#' if \code{MethAgr} is "mdc", the dataframe is [n, 1:4] containing hour, mean, variance, square of standard error of the scalar at diurnal cycle \cr
#' if \code{MethAgr} is "agrMnth", the dataframe is [n, 1:3] containing mean, variance, square of standard error of the scalar at monthly scale

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords variance, standard error,  aggregate, minutely, hourly, diurnal cycle

#' @examples
#' wrap.agr.vari.seSq(
#' data = data.frame(
#'   timeDoyDecm = c(1441:(5*1440))/24/60, 
#'   mean = rnorm(4*1440), 
#'   vari = rnorm(4*1440)^2
#' ), 
#' zone = -6,
#' MethAgr = "agrHour"
#' )

#' @seealso Currently none

#' @export


# changelog and author contributions / copyrights
#   Ke Xu (TBD)
#     original creation (2016-02-12)
#   Ke Xu (2016-04-11)
#     apply eddy4R code-style convention
#   Ke Xu (2016-04-20)
#    re-formualtion into a function() and several wrapper(): wrap.vari.seSq.hour, wrap.vari.seSq.mdc, wrap.vari.seSq.Mnth to allow broader use
#   Ke Xu (2016-05-10)
#    change the name to wrap.vari.seSq.hour and re-formulation again to code-style convention
#   Ke Xu (2016-06-09)
#    combine wrap.hour.vari.seSq.R, wrap.mdc.vari.seSq.R, and wrap.Mnth.vari.seSq.R into wrap.agr.vari.seSq.R
#   Ke Xu (2016-11-28)
#    minor change to remove the unnecessary sdSq in the input
#   Natchaya P-Durden (2018-03-28)
#    updated function header
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################

wrap.agr.vari.seSq <- function(
  
  data = data.frame(
    timeDoyDecm,
    mean, 
    vari
  ), 
  zone, 
  MethAgr
  
){
  
  rpt <- list()
  
  ################  Prepare data  #####################
  if (MethAgr == "agrHour" | MethAgr == "mdc"){
    
    #translate from UTC to local time
    data$timeDoyDecm <- data$timeDoyDecm + zone/24   
    data$timeDoyIntg <- floor(data$timeDoyDecm) 
    #translate from UTC to local time
    data$timeHour <- (data$timeDoyDecm - floor(data$timeDoyDecm))*24 
    
  }
  
  #prepare idx for sapply function
  if (MethAgr == "agrHour"){
    idx <- base::as.vector(base::sapply(base::unique(data$timeDoyIntg), function(x) (x + c(0:23)/24)))
    timeHour <- 24 * (idx-floor(idx))
    timeDoyIntg <- floor(idx)
  }
  
  if (MethAgr == "mdc") idx <- c(0:23)
  
  if (MethAgr == "agrMnth") idx <- 1
  
  
  # calculate aggregated mean, variance, squared standard error
  tmp <- list()
  tmp <- sapply(1:length(idx), function(x) {
    
    if (MethAgr == "agrHour") whr <- which(abs(data$timeHour - timeHour[x]) < 0.1 & abs(data$timeDoyIntg - timeDoyIntg[x]) < 0.1)
    
    if (MethAgr == "mdc") whr <- which(abs(data$timeHour - (x-1)) < 0.1)
    
    if (MethAgr == "agrMnth") whr <- c(1:length(data$mean))
    
    eddy4R.base::def.agr.vari.seSq(data = data.frame(
      mean = data$mean[whr],
      vari = data$vari[whr]
    ))
    
  })
  
  #prepare the reported data frame
  rpt <- data.frame(
    mean = base::unlist(tmp["mean",]),
    vari = base::unlist(tmp["vari",]),
    seSq = base::unlist(tmp["seSq",])
  )
  
  if (MethAgr == "agrHour"){
    rpt <- data.frame(
      timeDoyIntg = timeDoyIntg,
      timeHour = timeHour,
      rpt
    )
  }
  
  if (MethAgr == "mdc"){
    rpt <- data.frame(
      timeHour = idx,
      rpt
    )
  }
  
  return(rpt)
  
}
