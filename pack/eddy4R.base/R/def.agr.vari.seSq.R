##############################################################################################
#' @title Definition function: Determining mean, external, internal and total variance, and squared standard error

#' @author
#' Ke Xu \email{xuke2012abroad@@gmail.com} \cr
#' Stefan Metzger \email{eddy4R.info@@gmail.com}

#' @description 
#' Function defintion. Calculates the mean, external, internal and total variance, and the squared standard error over all supplied input values. Useful e.g. to aggregate from daily to monthly resolution by using all provided values, or similar.

#' @param data Dataframe of type numeric containing column vectors \code{mean} and \code{vari} of equal length.
#' @param mean Vector of type numeric. Means of the variable of interest at finer resolution, e.g. minutely [user-defined].
#' @param vari Vector of type numeric. Variances of the variable of interest at finer resolution, e.g. minutely [user-defined^2].

#' @return Returns a list containing mean \code{mean}, external variance \code{variExt}, internal variance \code{variIntl}, total variance \code{variTota}, and squared standard error \code{seSq} at coarser resolution, e.g. hourly when \code{data} contains 60 minutely values.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords standard deviation, standard error, aggregate

#' @examples 
#' def.vari.seSq.agr(data = data.frame(mean = rnorm(10), vari = rnorm(10)^2))

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Ke Xu (TBD)
#     original creation (2016-02-12)
#   Ke Xu (2016-04-11)
#     apply eddy4R code-style convention
#   Ke Xu (2016-04-20)
#    re-formualtion into a function() and a wrapper(): wrap.sd.se.Mnth to allow broader use
#   Stefan Metzger (2016-05-04)
#    formulate with data.frame input, use variance throughout, fix standard error equation, report list
#   Ke Xu (2016-06-08)
#     rename variIntl and variExt to be consistent with Mahrt (1998).
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################


def.agr.vari.seSq <- function(data =
                                data.frame(
                                  mean,
                                  vari
                                )){
  
  # assign list for the reported variables
  rpt <- list()
  
  # if there is no data in the mean,  return a list of NaN
  if (length(which(!is.na(data$mean))) == 0){
    rpt <- list(
      mean = NaN,
      variExt = NaN,
      variIntl = NaN,
      vari = NaN,
      seSq = NaN
    )
  } else {
    # calculate mean
    rpt$mean = base::mean(data$mean, na.rm = TRUE)
    
    # calculate external variance
    rpt$variExt = stats::var(data$mean, na.rm = TRUE)
    
    # calculate internal variance
    rpt$variIntl = base::mean(data$vari, na.rm = TRUE)
    
    # calculate total variance = external variance + internal variance
    rpt$vari = rpt$variExt + rpt$variIntl
    
    # calculate squared standard error = external variance / sample size:
    # how far is the sample mean likely to be from the population mean
    rpt$seSq = rpt$variExt / length(na.omit(data$mean))
  }
  
  
  # return results
  return(rpt)
  
}
