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
SPEmod <- function(
  #independent variable, preferabley f, but n is possible
  ide = SPEout$fr_obs[SPEout$fr_whr],
  #spectrum or cospectrum?
  sc = c("spe", "cos")[2],
  #stability parameter
  si = OUT$REYN$mn$sigma[FILE],
  #frequency f at which fCO(f) reaches its maximum value
  fx = 0.1,
  #output frequency-weighted (co)spectrum?
  weight = TRUE
){
  
  #(inertial subrange) slope parameter
  #3/2 for -5/3 (spectra) power law
  if(sc == "spe") m <- 3/2
  #3/4 for -7/3 (cospectra), 
  if(sc == "cos") m <- 3/4
  
  #broadness parameter
  #1/2 for unstable
  if(si <= 0) mue=1/2  
  #7/6 for stable stratification 
  if(si > 0)  mue=7/6
  
  #calculate non-scaled, frequency-weighted model Cospectrum (fCo or nCo)
  COmM <- (ide / fx) / (
    ( 1 + m * (ide / fx)^(2 * mue) )^( (1/(2*mue)) * ((m+1)/m) )
  )
  
  #(un)weight the (co)spectrum if necessary
  if(weight == FALSE) COmM <- COmM / ide
  
  #normalize to sum of 1
  COmM <- COmM / sum(COmM, na.rm=TRUE)
  
  ########################################################	  
}
########################################################
