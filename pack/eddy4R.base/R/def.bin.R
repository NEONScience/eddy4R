##############################################################################################
#' @title Binning data

#' @author Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description Smooth data using Binning method.

#' @param \code{idep} Either a vector or matrix of class numeric or integer containing the independent variable and of the same length as \code{depe}. []
#' @param \code{depe} Either a vector or matrix of class numeric or integer containing the dependent variable and of the same length as \code{idep}. []
#' @param \code{RngMinMax} An object of class numeric or integer containing the minimum and maximum values of the independent variable. Defaults to NULL. []
#' @param \code{NumBin} An object of class numeric or integer containing the number of bins. []
#' @param \code{widtBin} An object of class string containing the functions ("lin", "log10", "exp10", "logExp", "expLog") to determine bin width distribution of the independent variable. []
#' @param \code{meanFunc} An object of class string containing the arithmetic "mean" and "median". []

#' @return \code{idep} A list object of class "numeric" containing the resulted binning of independent variable and of the same length as {widtBin}. [] \cr
#' @return \code{depe} A matrix containing the the resulted binning of dependent variable and of the same length as {widtBin}. [] \cr

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords bin

#' @examples 
#' def.bin(idep = rnorm(5000), depe = rnorm(5000), RngMinMax = NULL, NumBin = 23, widtBin = "log10", meanFunc = "mean" )
#' def.bin(idep = rnorm(500), depe = rnorm(500), RngMinMax = c(0.1, 0.4), NumBin = 12, widtBin = "lin", meanFunc = "median" )

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-07-23)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-04-28)
#     Initail naming convention for eddy4R
##############################################################################################
#BIN DATA

def.bin <- function(
  idep,
  depe,
  RngMinMax = NULL,
  NumBin,
  widtBin = c("lin", "log10", "exp10", "logExp", "expLog"),
  meanFunc = c("mean", "median")
) {
  #aggregation with binning 
  #  idep: independent variable, frequency, wavenumber etc.
  #  depe: dependent variable, vector or matrix of same length as idep
  #  RngMinMax: min and max range
  #  NumBin: number of bins
  #  widtBin: c("lin", "log10", "exp10", "logExp", "expLog") bin width distribution as function of idep
  
  #prepare variables
  idep <- idep
  depe <- as.matrix(depe)
  minMax <- RngMinMax
  
  #define boundary
  if(is.null(RngMinMax)) minMax <- base::range(idep)
  if(widtBin == "lin") bou <- base::seq(minMax[1], minMax[2], length.out=(NumBin + 1))
  if(widtBin == "log10") bou <- base::log10(base::seq(10^(minMax[1]), 10^(minMax[2]), length.out=(NumBin + 1)))
  if(widtBin == "exp10") bou <- 10^(base::seq(base::log10(minMax[1]), base::log10(minMax[2]), length.out=(NumBin + 1)))
  if(widtBin == "logExp") bou <- base::log(base::seq(base::exp(minMax[1]), base::exp(minMax[2]), length.out=(NumBin + 1)))
  if(widtBin == "expLog") bou <- base::exp(base::seq(base::log(minMax[1]), base::log(minMax[2]), length.out=(NumBin + 1)))
  if(is.null(RngMinMax)) bou[c(1,length(bou))] <- c(0,Inf)
  
  #actual binning
  for(i in 1:(length(bou)-1)) {
    whrBin <- which(idep > bou[i] & idep <= bou[i+1])
    if(meanFunc == "median") {
      idepDum <- stats::median(idep[whrBin], na.rm=T)
      depeDum <- sapply(1:ncol(depe), function(x) stats::median(depe[whrBin,x], na.rm=T))
    } else {
      idepDum <- base::mean(idep[whrBin], na.rm=T)
      depeDum <- sapply(1:ncol(depe), function(x) base::mean(depe[whrBin,x], na.rm=T))
    }
    if(i == 1) {
      idepBin <- idepDum
      depeBin <- depeDum
    } else {
      idepBin <- c(idepBin, idepDum)
      depeBin <- rbind(depeBin, depeDum)
    }
  }
  if(!is.null(RngMinMax)) idepBin <- sapply(1:(length(bou)-1), function(x) base::mean(bou[x:(x+1)]))
  
  #generate output
  rpt <- list(idepBin, depeBin)
  attributes(rpt)$names <- c("idep","depe")
  
  #return outputs
  return(rpt)
  
  # end function def.bin()
}
