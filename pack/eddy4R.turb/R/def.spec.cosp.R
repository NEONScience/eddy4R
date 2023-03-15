##############################################################################################
#' @title Definition function: Generate cospectra

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' @author David Durden 

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   David Durden (2022-02-09)
#     terms update

#' @description Generate cospectra.

#' @param dataSpec data.frame or matrix frequency space data, raw spectra as complex numbers
#' @param dataTimeDomn data.frame of time domain data, identical variables and length as dataSpec
#' @param CoefConvEngy A single factor (specific heat at constant pressure * air density) for conversion from kinematic to energetic units; only used for comparison with Reynolds-Covariance, not for output; defaults to 1

#' @return A list \code{rpt} that contains the cospectral data (\code{rpt$dataCosp}) and the summed cospectra (\code{rpt$sumCosp}), which includes evaluation against total covariance and the relative difference

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Fast Fourier Transform, FFT, spectral, cospectra

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#GENERATE COSPECTRA
########################################################
def.spec.cosp <- function(
  dataSpec, #dataSpec: frequency space data, raw spectra as complex numbers
  dataTimeDomn, #dataTimeDomn: time space data, identical variables and length as dataSpec
  CoefConvEngy=1 #CoefConvEngy: (1) single factor (specific heat at constant pressure * air density) for conversion from kinematic to energetic units; only used for comparison with Reynolds-Covariance, not for output
  ){
  
   #calculate cospectra
  for(idx in 2:base::ncol(dataSpec)) {
    if(idx == 2) {
      dataCosp <- base::Re(dataSpec[,1]) * base::Re(dataSpec[,idx]) + base::Im(dataSpec[,1]) * base::Im(dataSpec[,idx])
    } else {
      dataCosp <- base::cbind(dataCosp, base::Re(dataSpec[,1]) * base::Re(dataSpec[,idx]) + base::Im(dataSpec[,1]) * base::Im(dataSpec[,idx]))
    }
  }
  dataCosp <- base::as.matrix(dataCosp)
  if(base::ncol(dataCosp) == 1) {
    base::dimnames(dataCosp)[[2]] <- base::list(paste("Co(w'", base::dimnames(dataSpec)[[2]][2:base::ncol(dataSpec)], "')", sep=""))
  } else {
    base::dimnames(dataCosp)[[2]] <- base::paste("Co(w'", base::dimnames(dataSpec)[[2]][2:base::ncol(dataSpec)], "')", sep="")
  }
  
  #the sum should be the same as the covariance:
  sumCosp <- base::sapply(1:ncol(dataCosp), function(x) base::sum(dataCosp[2:base::nrow(dataCosp),x], na.rm=T)) * CoefConvEngy
  covTotl <- stats:::cov(dataTimeDomn)[1,c(2:ncol(dataTimeDomn))] * CoefConvEngy #covTotl == REYsum, should be equal to total covariance
  base::names(sumCosp) <- base::names(covTotl) <- base::dimnames(dataCosp)[[2]]
  #relative difference
  percDiffCosp <- (sumCosp - covTotl) / covTotl * 100
  
  #generate output
  sumCospOut <- base::rbind(sumCosp, covTotl, percDiffCosp)
  rpt <- base::list(dataCosp, sumCospOut)
  base::attributes(rpt)$names <- c("dataCosp", "sumCosp")
  
  #return results
  return(rpt)
  ########################################################
}
########################################################
