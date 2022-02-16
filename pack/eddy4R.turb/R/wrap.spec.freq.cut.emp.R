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
#   David Durden (2022-02-09)
#     terms update

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
def.spec.freq.cut.emp <- function(
  #cutoff frequency
  FreqCut,
  #independent variable, preferabley f, but n is possible
  idep,
  #dependent variable, spectra or cospectra
  depe,
  #reference correction factor
  CoefCorRefe
) {
  
  #sigmoidal transfer function (Lorentzian) after Eugster and Senn (1995) in Aubinet et al. (2012) Eq. 4.21
  tfunSigm <- eddy4R.turb::def.spec.tfun.sigm(FreqCut=FreqCut, Freq=idep)
  
  #plotting
  #       plot(fun_tsig ~ ide)
  
  #calculate resulting correction factor over all frequencies
  CoefCor <- base::sum(depe / tfunSigm, na.rm=TRUE) / base::sum(depe, na.rm=TRUE)
  
  #calculate optimality criterion
  critOptm <- base::abs(CoefCorRefe - CoefCor)
  
  #return results
  return(critOptm)
  
  ########################################################  
}
########################################################

