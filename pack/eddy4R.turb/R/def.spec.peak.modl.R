##############################################################################################
#' @title Definition function: Function to define nonlinear model to fit to veloZaxsHor spectra to determine frequency peak

#' @author
#' Adam Young \email{younga1@battelleecology.org}

#' @description 
#' Definition function. Function to define nonlinear model to fit to veloZaxsHor spectra and determine frequency peak. The output from this model is then used with 'optim' function to the frequency at which vertical wind speak spectral power reaches it's peak value.

#' @param para Parameters to be fit model via optimization.
#' @param freq frequency vector as independent variable.

#' 
#' @return Vector of spectra values for set of parameters.
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords Currently none.

#' @examples Currently none.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   Adam Young(2024-02-21)
#     original creation
#   Adam Young (2024-05-07)
#     Clean up, commenting, and modified function description
##############################################################################################


# start function def.spec.peak.modl()

def.spec.peak.modl <- function(para, freq) {
  
  # Simple non-linear model used in NK12 to model weighted vertical wind spectra
  # and then use modeled spectral curve to identify frequency that spectral peak
  # occurs at. Based on unpublished Matlab code provide to Stefan Metzger from 
  # authors of NK12.
  specFit <- (para[1] * freq) / ((1 + para[2] * freq)^(5/3))
  
  return(specFit)
  
}
