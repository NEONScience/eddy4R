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

def.wave.lorenz.calc <- function(x) {
  
  xsort <- lapply(x, function(x) sort(x))
  index <- lapply(x, function(x) sort.int(x, index.return = TRUE)$ix)
  
  frac <- lapply(index, function(x) seq(1, length(x)) / length(x))
  lorenz <- lapply(xsort, function(x) cumsum(x) / sum(x))
  
  return(list(lorenz = lorenz, index = index, frac = frac))
  
}