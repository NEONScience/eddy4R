##############################################################################################
#' @title Wrapper function: Create NEON Level 1 data product descriptive statistics across list elements

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Wrapper function. Compute NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points) across list elements. 

#' @param data A data.frame or list containing the L0p (calibrated) input data at native resolution. Of type numeric or integer. [-]
#' @param idx If data is a list, which list entries should be processed into Level 1 data products? Defaults to NULL which expects data to be a data.frame. Of type character. [-]

#' @return Descriptive statistics, for \code{vrbs = FALSE} a data frame and for \code{vrbs = TRUE} a list:\cr
#' \code{mean} The mean of non-NA values in \code{data}
#' \code{min} The minimum value of non-NA values in \code{data}
#' \code{max} The maximum value of non-NA values in \code{data}
#' \code{vari} The variance of non-NA values in \code{data}
#' \code{num} The number of non-NA values in \code{data}
#' \code{se} The standard error of the mean of non-NA values in \code{data}

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords average, aggregate, descriptive statistics

#' @examples
#'   # data.frame which variables contain the unit attributes
#'   
#'     # create list with L0p data
#'     data <- list(
#'       sens01 = data.frame(
#'         velo = rnorm(10),
#'         temp = rnorm(10)),
#'       sens02 = data.frame(
#'         velo = rnorm(10),
#'         temp = rnorm(10)),
#'       sens03 = data.frame(
#'         velo = rnorm(10),
#'         temp = rnorm(10))
#'       )
#'   
#'     # assign unit attribute
#'     attributes(data$sens01$velo)$unit <- "m s-1"
#'     attributes(data$sens01$temp)$unit <- "K"
#'     attributes(data$sens02$velo)$unit <- "m s-1"
#'     attributes(data$sens02$temp)$unit <- "K"
#'     attributes(data$sens03$velo)$unit <- "m s-1"
#'     attributes(data$sens03$temp)$unit <- "K"
#'   
#'   # calculate L1 data products only for sensors sens01 and sens02
#'   rpt <- wrap.dp01(data = data, idx = c("sens01", "sens02"))
#'   # units are propagated
#'   attributes(rpt$sens02$se$velo)$unit
#'   # [1] "m s-1"

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   Stefan Metzger (2016-12-02)
#     original creation
#   David Durden (2017-06-10)
#     Adding calculations for wind direction
#   David Durden (2017-06-10)
#     Adding fix for infinite min and max wind direction
#   Natchaya P-Durden (2018-04-03)
#     update @param format
#   Natchaya P-Durden (2018-05-22)
#     rename function from def.neon.dp01() to def.dp01()
#     rename function from wrap.neon.dp01() to wrap.dp01()
##############################################################################################


# start function wrap.dp01()
wrap.dp01 <- function(
  data,
  idx = NULL
) {
  
  # stop if data is a list but idx is not specified  
  if(!is.data.frame(data) & is.null(idx)) stop("wrap.dp01: data is not a data.frame; if data is a list please specify idx.")
  
  # if data is a data.frame, calculate NEON Level 1 data products directly
  if(is.data.frame(data)) rpt <- eddy4R.base::def.dp01(data = data, vrbs = TRUE)
  
  # if data is a list, calculate NEON Level 1 data products recursively for each list element
  if(is.list(data) & !is.data.frame(data)) rpt <- lapply(X = data[idx], FUN = eddy4R.base::def.dp01, vrbs = TRUE)

  if("soni" %in% idx){
    #Calculate wind direction using vector averaging approach
    dirWind <- eddy4R.base::def.dir.wind(inp = data$soni$angZaxsErth, MethVari = "Yama")
    #Rotating to NED coordinates
    rpt$soni$mean$angZaxsErth <- eddy4R.base::def.rot.enu.ned(angEnu = dirWind$mean)
    #Rotating to NED coordinates to find minimum
    rpt$soni$min$angZaxsErth <- base::min(eddy4R.base::def.rot.enu.ned(angEnu = data$soni$angZaxsErth), na.rm = TRUE)
    #Checking for infinite output and replacing with NaN
    rpt$soni$min$angZaxsErth[is.infinite(rpt$soni$min$angZaxsErth)] <- NaN
    base::attr(rpt$soni$min$angZaxsErth, which = "unit") <- "rad"
    #Rotating to NED coordinates to find maximum
    rpt$soni$max$angZaxsErth  <- base::max(eddy4R.base::def.rot.enu.ned(angEnu = data$soni$angZaxsErth), na.rm = TRUE)
    #Checking for infinite output and replacing with NaN
    rpt$soni$max$angZaxsErth[is.infinite(rpt$soni$max$angZaxsErth)] <- NaN
    base::attr(rpt$soni$max$angZaxsErth, which = "unit") <- "rad"
    #Applying vector average function output to wrapper output
    rpt$soni$numSamp$angZaxsErth <- dirWind$numSamp
    rpt$soni$vari$angZaxsErth <- dirWind$vari
    rpt$soni$se$angZaxsErth <- dirWind$se
  }
  # return results
  return(rpt)
  
}
# end function wrap.dp01()
