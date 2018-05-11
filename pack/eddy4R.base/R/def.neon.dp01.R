##############################################################################################
#' @title Definition function: Create NEON Level 1 data product descriptive statistics

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description 
#' Function definition. Compute NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points) by aggregating the input data over its entire range. 

#' @param data A numeric vector or data.frame containing the L0p (calibrated) input data at native resolution. Of class numeric". [-]
#' @param vrbs Verbose output, one of either TRUE of FALSE, defaults to FALSE. If \code{vrbs = FALSE} is selected, the returned object is a data.frame. If  \code{vrbs = TRUE} is selected, the returned object is a list supporting the propagation of unit attributes for individual variables in \code{data}. Of class logical". [-]


#' @return Descriptive statistics, for \code{vrbs = FALSE} a data frame and for \code{vrbs = TRUE} a list:\cr
#' \code{mean} The mean of non-NA values in \code{data}
#' \code{min} The minimum value of non-NA values in \code{data}
#' \code{max} The maximum value of non-NA values in \code{data}
#' \code{vari} The variance of non-NA values in \code{data}
#' \code{num} The number of non-NA values in \code{data}
#' \code{se} The standard error of the mean of non-NA values in \code{data}

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON.DOC.003311 - NEON DATA PRODUCTS DEVELOPMENT PLAN

#' @keywords average, aggregate, descriptive statistics

#' @examples 
#' # argument vrbs changes format of reported object
#' 
#'    # Calibrated raw data
#'      data <- c(1,2,3,NA,5,6,7,NaN,9,10)
#'      
#'    # Level 1 descriptive statistics
#'      dp01 <- def.neon.dp01(data = data)
#'      
#'    # Level 1 descriptive statistics in verbose output
#'      dp01Vrbs <- def.neon.dp01(data = data, vrbs = TRUE)
#' 
#' # argument vrbs = TRUE is useful for preserving unit information on
#' # per-variable basis and use with lapply() and do.call()
#' 
#'    # data.frame which variables contain the unit attributes
#' 
#'      # create data.frame
#'      data <- data.frame(
#'        velo = rnorm(10),
#'        temp = rnorm(10),
#'        dist = rnorm(10)
#'      )
#'  
#'      # assign unit attribute
#'      attributes(data$velo)$unit <- "m s-1"
#'      attributes(data$temp)$unit <- "K"
#'      attributes(data$dist)$unit <- "m"
#' 
#'    # vrbs = FALSE does not propagate unit information
#'    attributes(def.neon.dp01(data = data, vrbs = FALSE)$se.velo)$unit
#'    # NULL
#'    
#'    # vrbs = TRUE propagates unit information
#'    attributes(def.neon.dp01(data = data, vrbs = TRUE)$se$velo)$unit
#'    # [1] "m s-1"

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   Stefan Metzger (2016-11-08)
#     original creation 
#   Cove Sturtevant (2016-11-22)
#     original creation (def.dp01.stat.R, which was consolidated into def.neon.dp01.R)
#   Stefan Metzger (2016-12-02)
#     consolidation of def.dp01.stat.R into def.neon.dp01.R
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################

# definition function to calculate standard NEON Level 1 data products from single data.frame

# start function def.neon.dp01()
def.neon.dp01 <- function(
  # assign data, data.frame or matrix of type numeric or integer
  data,
  # return list which also includes units?
  vrbs = FALSE
){
  
  # if input is vector, transform to data.frame      
  if(is.vector(data)) data <- data.frame(stringsAsFactors = FALSE, data)
  
  # check whether data is vector or data.frame
  if(!base::is.data.frame(data)) {
    base::stop("Input must either be a vector or a data.frame")
  }
  
  # check whether data is numeric
  if(!base::is.numeric(base::unlist(data))){
    base::stop("Input must be numeric")
  }
  
  # initialize object for reported variables
  rpt <- list()
  
  # mean
  
  # calculate column means
  rpt$mean <- base::data.frame(t(colMeans(data, na.rm = TRUE)))
  
  # replace all infinite values (NA, NaN, Inf or -Inf) with NaN
  rpt$mean[is.infinite(unlist(rpt$mean))] <- NaN
  
  # assign units for each variable
  rpt$mean <- eddy4R.base::def.unit.var(samp = rpt$mean, refe = data)
  
  # minimum
  
  # calculate column minimums
  rpt$min <- base::data.frame(t(suppressWarnings(splus2R::colMins(data, na.rm = TRUE))))
  
  # replace all infinite values (NA, NaN, Inf or -Inf) with NaN
  rpt$min[is.infinite(unlist(rpt$min))] <- NaN
  
  # assign units for each variable
  rpt$min <- eddy4R.base::def.unit.var(samp = rpt$min, refe = data)
  
  # maximum
  
  # calculate column maximums
  rpt$max <- base::data.frame(t(suppressWarnings(splus2R::colMaxs(data, na.rm = TRUE))))
  
  # replace all infinite values (NA, NaN, Inf or -Inf) with NaN
  rpt$max[is.infinite(unlist(rpt$max))] <- NaN
  
  # assign units for each variable
  rpt$max <- eddy4R.base::def.unit.var(samp = rpt$max, refe = data)
  
  # unbiased sample variance
  
  # calculate unbiased sample variance
  rpt$vari <- base::data.frame(t(splus2R::colVars(data, na.rm = TRUE)))
  
  # replace all infinite values (NA, NaN, Inf or -Inf) with NaN
  rpt$vari[is.infinite(unlist(rpt$vari))] <- NaN
  
  # assign units for each variable
  rpt$vari <- eddy4R.base::def.unit.var(samp = rpt$vari, refe = data)
  
  # sample size
  
  # calculate sample size
  rpt$numSamp <- base::data.frame(t(sapply(names(data), function(x) length(which(!is.na(data[x]))))))
  
  # replace all infinite values (NA, NaN, Inf or -Inf) with NaN
  rpt$numSamp[is.infinite(unlist(rpt$numSamp))] <- NaN
  
  # assign units for each variable
  rpt$numSamp <- eddy4R.base::def.unit.var(samp = rpt$numSamp, refe = data)
  
  # standard error
  rpt$se <- base::sqrt(rpt$vari)/base::sqrt(rpt$numSamp)
  
  # unlist to vector if verbose output is not needed
  # attention, even if data contains variables with unit attribute, they are not reported in this case
  if(vrbs == FALSE) {
    
    # unlist results
    rpt <- data.frame(stringsAsFactors = FALSE, base::t(unlist(rpt)))
    
    # strip names
    names(rpt) <- unlist(strsplit(names(rpt), ".data"))  
    
  }
  
  # return results
  return(rpt)
  
}
# end function def.neon.dp01()
