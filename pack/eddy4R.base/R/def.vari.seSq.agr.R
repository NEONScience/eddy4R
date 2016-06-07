##############################################################################################
#' @title Determining mean, between-record, within-record and total variance, and squared standard error

#' @author Ke Xu \email{xuke2012abroad@gmail.com} \cr
#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Calculates the mean, between-record, within-record and total variance, and the squared standard error over all supplied values. Useful e.g. to aggregate from hourly to daily resolution by providing 24 hourly values, or similar.

#' @param \code{data} Dataframe of type numeric containing column vectors \code{mean} and \code{vari} of equal length.
#' @param \code{mean} Vector of type numeric. Means of the variable of interest at finer resolution, e.g. minutely [user-defined].
#' @param \code{vari} Vector of type numeric. Variances of the variable of interest at finer resolution, e.g. minutely [user-defined^2].

#' @return Returns a list containing mean \code{mean}, between-record variance \code{variBtw}, within-record variance \code{variWi}, total variance \code{variTota}, and squared squared standard error \code{seSq} at coarser resolution, e.g. hourly when \code{data} contains 60 minutely values.

#' @references
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords standard deviation, standard error, aggregate

#' @examples 
#' def.sd.se(data = data.frame(mean = rnorm(10), vari = rnorm(10)^2))

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Ke Xu (TBD)
#     original creation (2016-02-12)
#   Ke Xu (2016-04-11)
#     apply eddy4R code-style convention
#   Ke Xu (2016-04-20)
#    re-formualtion into a function() and a wrapper(): wrap.sd.se.moly to allow broader use
#   Stefan Metzger (2016-05-04)
#    formulate with data.frame input, use variance throughout, fix standard error equation, report list
#   Ke Xu (2016-05-10)
#     rename variWi and variBtw to be consistent with Mahrt (1998).
##############################################################################################


def.vari.seSq.agr <- function(data =
                                data.frame(
                                  mean,
                                  vari
                                )){
  
  # assign list for the reported variables
  rpt <- list()
  
  # calculate mean
  rpt$mean = base::mean(data$mean, na.rm = TRUE)
  
  # calculate between-record variance
  rpt$variBtw = stats::var(data$mean, na.rm = TRUE)
  
  # calculate within-record variance
  rpt$variWi = base::mean(data$vari, na.rm = TRUE)
  
  # calculate total variance = between-record variance + within-record variance
  rpt$vari = rpt$variBtw + rpt$variWi
  
  # calculate squared standard error = between-record variance / sample size:
  # how far is the sample mean likely to be from the population mean
  rpt$seSq = rpt$variBtw / length(na.omit(data$mean))
  
  # return results
  return(rpt)
  
}
