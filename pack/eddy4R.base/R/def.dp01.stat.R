##############################################################################################
#' @title Create NEON Level 1 data product descriptive statistics

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org} \cr

#' @description 
#' Function definition. Compute NEON Level 1 data product descriptive statistics (mean, minimum, maximum, variance, number of non-NA points) by aggregating the input data over its entire range. 

#' @param \code{data} A numeric vector containing the L0' (calibrated) input data at native resolution. Of class numeric". [-] 

#' @return A list of descriptive statistics:\cr
#' \code{mean} The mean of non-NA values in \code{data}
#' \code{min} The minimum value of non-NA values in \code{data}
#' \code{max} The maximum value of non-NA values in \code{data}
#' \code{vari} The variance of non-NA values in \code{data}
#' \code{num} The number of non-NA values in \code{data}

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
  
  # Do some error catching
  if(!is.vector(data)) {
    stop("Input must be a vector")
  }
  
  if(!is.numeric(data)){
    stop("Input must be numeric")
  }
  
  # Intialize output
  rpt <- list(mean=NA,min=NA,max=NA,vari=NA,num=NA)
  
  # Compute descriptive statistics
  rpt$mean <- mean(data,na.rm=TRUE)
  rpt$min <- min(data,na.rm=TRUE)
  rpt$max <- max(data,na.rm=TRUE)
  rpt$vari <- var(data,na.rm=TRUE)
  rpt$num <- sum(!is.na(data))
  
}
