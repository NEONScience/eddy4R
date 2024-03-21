##############################################################################################
#' @title Definition function: Function to organize and define wavelet coefficients into a Lorenz curve. Used to find most energetic coefficients for adjustment in spectral corrections.

#' @author
#' Adam Young \email{younga1@battelleecology.org}

#' @description 
#' TBD

#' @param para TBD.
#' @param freq TBD.

#' 
#' @return TBD.
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords Currently none.

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   Adam Young(2024-03-14)
#     original creation
##############################################################################################

# start function def.wave.lorenz.calc()

def.wave.lorenz.calc._test_CWT <- function(veloZaxsCoef) {
  
  waveScalOrd <- do.call(cbind, lapply(seq(1, ncol(veloZaxsCoef)), function(x) sort(veloZaxsCoef[,x], decreasing = FALSE))) # Sort data in descending order
  idxWaveScalOrd <- do.call(cbind, lapply(seq(1, ncol(veloZaxsCoef)), function(x) sort.int(veloZaxsCoef[,x], decreasing = FALSE, index.return = TRUE)$ix)) # Sort data in descending order

  waveScalSum <- colSums(waveScalOrd)

  frac <- seq(1, nrow(veloZaxsCoef)) / nrow(veloZaxsCoef)
  lorenzOut <- do.call(cbind, lapply(seq(1, ncol(veloZaxsCoef)), function(x) (cumsum(waveScalOrd[,x])) / waveScalSum[x]))
  
  return(list(lorenz = lorenzOut, frac = frac, index = idxWaveScalOrd))

}