#' Name Species
#' 
#' generate eddy4r style names for chemical species
#' 
#' @param species species name such that FD_mole_species corresponds to a column header in data [character]
#' @param type one of c("mole","mass","kin") for each type of name required
#' 
#' @author W. S. Drysdale
#' 
#' @export

def.spec.name = function(species,type = c("mole","mass","kin")[1]){
  
  if(is.null(species) | length(species) == 0)
    return(NULL)
  
  if(type == "mole")
    spcNamed = paste0("FD_mole_",toupper(species))
  
  if(type == "mass")
    spcNamed = paste0("F_",toupper(species),"_mass")
  
  if(type == "kin")
    spcNamed = paste0("F_",toupper(species),"_kin")
  
  spcNamed
}
