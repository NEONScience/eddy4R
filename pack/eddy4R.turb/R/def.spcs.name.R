#' Name Species
#' 
#' generate eddy4r style names for chemical species
#' 
#' @param spcs species name such that FD_mole_species corresponds to a column header in data [character]
#' @param type one of c("mole","mass","kin") for each type of name required
#' 
#' @author W. S. Drysdale
#' 
#' @export

def.spcs.name = function(spcs,type = c("mole","mass","kin")[1]){
  
  if(is.null(spcs) | length(spcs) == 0)
    return(NULL)
  
  if(type == "mole")
    nameSpcs = paste0("FD_mole_",toupper(spcs))
  
  if(type == "mass")
    nameSpcs = paste0("F_",toupper(spcs),"_mass")
  
  if(type == "kin")
    nameSpcs = paste0("F_",toupper(spcs),"_kin")
  
  nameSpcs
}
