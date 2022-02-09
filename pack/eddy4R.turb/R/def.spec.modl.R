#############################################################################################
#' @title Definition function: Model (co)spectrum after Massman, 2005 (in Lee, 2005)

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-09-22)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Model (co)spectrum after Massman, 2005 (in Lee, 2005).

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#function to generate model (co)spectrum after Massman, 2005 (in Lee, 2005)
#continuous approximation of the Kaimal (1972) cospectra
########################################################
def.spec.modl <- function(
  #independent variable, preferabley f, but n is possible
  Idep = SPEout$fr_obs[SPEout$fr_whr],
  #spectrum or cospectrum?
  MethSpec = c("spec", "cosp")[2],
  #stability parameter
  paraStbl = OUT$REYN$mn$sigma[FILE],
  #frequency f at which fCO(f) reaches its maximum value
  FreqPeak = 0.1,
  #output frequency-weighted (co)spectrum?
  MethWght = TRUE
){
  
  #(inertial subrange) slope parameter
  #3/2 for -5/3 (spectra) power law
  if(MethSpec == "spec") paraSlp <- 3/2
  #3/4 for -7/3 (cospectra), 
  if(MethSpec == "cosp") paraSlp <- 3/4
  
  #broadness parameter
  #1/2 for unstable
  if(paraStbl <= 0) paraBrd <- 1/2  
  #7/6 for stable stratification 
  if(paraStbl > 0)  paraBrd <- 7/6
  
  #calculate non-scaled, frequency-weighted model Cospectrum (fCo or nCo)
  modlSpec <- (Idep / FreqPeak) / (
    ( 1 + paraSlp * (Idep / FreqPeak)^(2 * paraBrd) )^( (1/(2*paraBrd)) * ((paraSlp+1)/paraSlp) )
  )
  
  #(un)weight the (co)spectrum if necessary
  if(MethWght == FALSE) modlSpec <- modlSpec / Idep
  
  #normalize to sum of 1
  modlSpec <- modlSpec / base::sum(modlSpec, na.rm=TRUE)
  
  #return output
  return(modlSpec)
  
  
  ########################################################	  
} #end of function
########################################################
