##############################################################################################
#' @title Definition function: Determine cutoff frequency empirically 

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Determine cutoff frequency empirically .

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#determine cutoff frequency empirically 
########################################################
find_F0 <- function(
  #cutoff frequency
  f0,
  #independent variable, preferabley f, but n is possible
  ide = SPEout$fr_obs[SPEout$fr_whr][which(SPEout$fr_obs[SPEout$fr_whr] >= 0.01)],
  #dependent variable, spectra or cospectra
  dep = SPEout$FSunfold[SPEout$fr_whr,fpo][which(SPEout$fr_obs[SPEout$fr_whr] >= 0.01)],
  #reference correction factor
  corfac_ref = corfac_out
) {
  
  #calculate transfer function
  fun_tsig <- fun_TSIG(freq_0=f0, freq=ide)
  
  #plotting
  #       plot(fun_tsig ~ ide)
  
  #calculate resulting correction factor over all frequencies
  corfac <- sum(dep / fun_tsig, na.rm=TRUE) / sum(dep, na.rm=TRUE)
  
  #calculate optimality criterion
  crit <- abs(corfac_ref - corfac)
  
  #return results
  return(crit)
  
  ########################################################  
}
########################################################

