##############################################################################################
#' @title Definition function: Generate cospectra

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Generate cospectra.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#GENERATE COSPECTRA
########################################################
COSP.fwd <- function(FS, TS, cparho=1) {
  #FS: frequency space data, raw spectra as complex numbers
  #TS: time space data, identical variables and length as FS
  #cparho: (1) single factor (specific heat at constant pressure * air density) for conversion from kinematic to energetic units; only used for comparison with Reynolds-Covariance, not for output
  
  #calculate cospectra
  for(i in 2:ncol(FS)) {
    if(i == 2) {
      COSPdata <- Re(FS[,1]) * Re(FS[,i]) + Im(FS[,1]) * Im(FS[,i])
    } else {
      COSPdata <- cbind(COSPdata, Re(FS[,1]) * Re(FS[,i]) + Im(FS[,1]) * Im(FS[,i]))
    }
  }
  COSPdata <- as.matrix(COSPdata)
  if(ncol(COSPdata) == 1) {
    dimnames(COSPdata)[[2]] <- list(paste("Co(w'", dimnames(FS)[[2]][2:ncol(FS)], "')", sep=""))
  } else {
    dimnames(COSPdata)[[2]] <- paste("Co(w'", dimnames(FS)[[2]][2:ncol(FS)], "')", sep="")
  }
  
  #the sum should be the same as the covariance:
  COSPsum <- sapply(1:ncol(COSPdata), function(x) sum(COSPdata[2:nrow(COSPdata),x], na.rm=T)) * cparho
  REYsum <- stats:::cov(TS)[1,c(2:ncol(TS))] * cparho
  names(COSPsum) <- names(REYsum) <- dimnames(COSPdata)[[2]]
  #relative difference
  DIFFperc <- (COSPsum - REYsum) / REYsum * 100
  
  #generate output
  OUTsum <- rbind(COSPsum, REYsum, DIFFperc)
  export <- list(COSPdata, OUTsum)
  attributes(export)$names <- c("FScosp", "sum")
  
  #return results
  return(export)
  ########################################################
}
########################################################
