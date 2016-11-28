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
def.dp01.stat <- function (
  data
) {
  
  # Intialize output
  rpt <- base::data.frame(mean=NA,min=NA,max=NA,vari=NA,num=NA,se=NA)
  
  # Do some error catching
  if(!base::is.vector(data)) {
    base::stop("Input must be a vector")
  }
  
  # Check whether all NA values
  if(sum(is.na(data))==length(data)){
    return(rpt)
  }
    
  if(!base::is.numeric(data)){
    base::stop("Input must be numeric")
  }
  

  
  # Compute descriptive statistics
  rpt$mean <- base::mean(data,na.rm=TRUE)
  rpt$min <- base::min(data,na.rm=TRUE)
  rpt$max <- base::max(data,na.rm=TRUE)
  rpt$vari <- stats::var(data,na.rm=TRUE)
  rpt$num <- base::sum(!is.na(data))
  rpt$se <- base::sqrt(rpt$vari)/base::sqrt(rpt$num)
   
  return(rpt) 
}
