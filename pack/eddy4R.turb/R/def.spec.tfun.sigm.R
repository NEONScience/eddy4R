##############################################################################################
#' @title Definition function: Sigmoidal transfer function (Lorentzian)

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21
########################################################
fun_TSIG <- function(freq_0, freq) 1 / (1 + (freq / freq_0)^2)



