##############################################################################################
#' @title Create NEON Level 1 data product descriptive statistics

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org} \cr

#' @description 
#' Function definition. Compute NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points) by aggregating the input data over its entire range. 

#' @param \code{data} A numeric vector containing the L0' (calibrated) input data at native resolution. Of class numeric". [-] 

#' @return A data frame of descriptive statistics:\cr
#' \code{mean} The mean of non-NA values in \code{data}
#' \code{min} The minimum value of non-NA values in \code{data}
#' \code{max} The maximum value of non-NA values in \code{data}
#' \code{vari} The variance of non-NA values in \code{data}
#' \code{num} The number of non-NA values in \code{data}
#' \code{se} The standard error of the mean of non-NA values in \code{data}

#' @references 
#' NEON.DOC.003311 - NEON DATA PRODUCTS DEVELOPMENT PLAN

#' @keywords average, aggregate, descriptive statistics

#' @examples 
#' data <- c(1,2,3,NA,5,6,7,NaN,9,10) # Calibrated raw data
#' dp01 <- def.dp01.stat(data=data) # Level 1 descriptive statistics

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-11-22)
#     original creation 
##############################################################################################

# definition function to calculate standard NEON Level 1 data products from single data.frame

# start function def.neon.dp01()
def.neon.dp01 <- function(
  # assign data, data.frame or matrix of type numeric or integer
  data = data.frame(data),
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
