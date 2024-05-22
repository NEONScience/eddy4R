##############################################################################################
#' @title Definition function: Function to organize and define wavelet coefficients into a Lorenz curve. Used to find most energetic coefficients for adjustment in spectral corrections.

#' @author
#' Adam Young \email{younga1@battelleecology.org}

#' @description 
#' Function to organize and define wavelet coefficients into a Lorenz curve. Used to find most energetic coefficients for adjustment in spectral corrections.


#' @param waveCoef list of wavelet coefficients for each scale.
#' 
#' @return results from Lorenz curve calculations.
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords Wavelet, spectrum, cospectrum, NK12, frequency response correction

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   Adam Young (2024-03-14)
#     original creation
# Adam Young (2024-05-06)
#     Comment, clean-up and finalize
##############################################################################################

# start function def.wave.lorenz.calc()

def.wave.lorenz.calc <- function(waveCoef) {
  
  # sort wavelet coefficient values for given scale
  xsort <- lapply(waveCoef, function(x) sort(x))
  
  # same as previous command but sort wavelet coefficient values for given scale and return sorted index
  index <- lapply(waveCoef, function(x) sort.int(x, index.return = TRUE)$ix)
  
  # Fraction of observations 
  frac <- lapply(index, function(x) seq(1, length(x)) / length(x))
  lorenz <- lapply(xsort, function(x) cumsum(x) / sum(x))
  
  return(list(lorenz = lorenz, index = index, frac = frac))
  
}