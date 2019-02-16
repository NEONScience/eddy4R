#' Name Species
#' 
#' generate eddy4r style names for chemical species
#' 
#' @param species
#' @param type
#' 
#' @author W. S. Drysdale
#' 
#' @export

def.spec.name = function(species,type = c("mole","mass","kin")[1]){
  
  if(is.null(species))
    return(NULL)
  
  if(type == "mole")
    spcNamed = paste0("FD_mole_",toupper(species))
  
  if(type == "mass")
    spcNamed = paste0("F_",toupper(species),"_mass")
  
  if(type == "kin")
    spcNamed = paste0("F_",toupper(species),"_kin")
  
  spcNamed
}
