##############################################################################################
#' @title Median and median absolute deviation as robust measures of scale and dispersion

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function defintion. Calculates the median as robust measure of scale, and the sample-size corrected median absolute deviation as robust measure of dispersion. By default, the univariate statistics of the test data \code{test} are returned. If reference data \code{refe} is provided, the bivariate statistics of the residuals \code{test - refe} are returned.

#' @param \code{test} A vector containing the test data. Of class "numeric" or "integer", and of the same length as \code{refe}. [same units as reference data]
#' @param \code{refe} A vector containing the reference data. Of class "numeric" or "integer", and of the same length as \code{test}. Defaults to \code{refe = 0}. [same units as test data]
#' @param \code{Perc} Report results in the same units as \code{refe} and \code{test} (\code{Perc = FALSE}) or as percentages (\code{Perc = TRUE})? Of class "logical", defaults to \code{Perc = FALSE}.

#' @return Returns object of class "numeric" [1, 1:3] containing median, median absolute deviation, and sample size (NaNs not included).

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr
#' Croux, C., and Rousseeuw, P. J.: Time-efficient algorithms for two highly robust estimators of scale, Computational Statistics, 1, 411-428, 1992. \cr

#' @keywords robust statistics

#' @examples
#' #works
#' def.med.mad(test = rnorm(500))
#' def.med.mad(test = rnorm(10), refe = rnorm(10))
#' 
#' #returns error message
#' def.med.mad(test = rnorm(10), refe = rnorm(5000))

#' @seealso Function eddy4R.base:::def.rmsd.bias.prec.cdet() for Gaussian statistics-based measures of scale and dispersion.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2012-03-11)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Stefan Metzger (2016-02-10)
#     apply eddy4R code-style convention
##############################################################################################


# start function def.med.mad()
def.med.mad <- function(test, refe = 0, Perc = FALSE) {

  # failsafe: test that refe and test are of same length
  if( refe != 0 && length(test) != length(refe) ) {
    stop("eddy4R.base/def.med.mad: test and refe are of unequal length.")
    }

  # sample size (omitting NAs) for MAD correction after Croux and Rousseeuw (1992)
  NumSamp <- length(which(!is.na(refe) & !is.na(test)))

  # differences
  if(Perc == FALSE) diff <- test - refe
  if(Perc == TRUE)  diff <- (test - refe) / abs(refe) * 100
  
  # calculate median
  med <- stats:::median(diff, na.rm = TRUE)
  
  # calculate MAD incl. sample size correction after Croux and Rousseeuw (1992)
  mad <- stats:::mad(diff, na.rm = TRUE) * NumSamp / (NumSamp - 0.8)

  # prepare reported object
  rpt <- cbind(med, mad, NumSamp)
  
  # return reported object
  return(rpt)

# end function def.med.mad()  
}
