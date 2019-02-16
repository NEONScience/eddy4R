#' Define Chemical Species Flux
#' 
#' General Version of the CH4 flux calculation. Can use built in species RMM from IntlNatu or supply custom values
#' 
#' @param imfl immediate fluctuations, created during "BASE STATE AND DEVIATIONS" step. [data.frame]
#' @param mn mean of input data. created during "TIME SERIES AVERAGES" step [data.frame]
#' @param cor correlations [data.frame]
#' @param base base state of data, created during "BASE STATE AND DEVIATIONS" step. [data.frame]
#' @param species names of species as molecular formulae, using IUPAC style where approprate
#'   e.g \code{c("NO","NO2","C6H6")} [character vector]
#' @param speciesRMM relative molecular mass of species in kg mol-1. must be of equal length to species
#'  e.g \code{c(0.03001,0.46005,0.07811)}. See \code{eddy4R.base::IntlNatu$Molm*} for some built in speciesRMM
#'
#' @return list containing the updated imfl, mn and cor objects
#' 
#' @author W. S. Drysdale
#' 
#' @export

def.flux.chem = function(imfl,
                         mn,
                         cor,
                         base,
                         species,
                         speciesRMM){
  
  if(length(species) != length(speciesRMM))
    stop("in def.flux.chem() length of species does not equal length of speciesRMM")

    for (i in 1:length(species)){
      #Define  name of mole ratio for chemical species
      F_species_var <- def.spec.name(species[i],"mole")
      #Define name of chemical species kinamatic flux
      F_species_kin <- def.spec.name(species[i],"kin")
      #Define name of chemical species mass flux
      F_species_mass <- def.spec.name(species[i],"mass")
      
      #Chemical flux in kinematic units [mol m-2 s-1]
      imfl[,F_species_kin] <- base$rho_dry * imfl$w_hor * imfl[,F_species_var]
      attributes(imfl[,F_species_kin])$unit <- "mol m-2 s-1"
      mn[,F_species_kin] <- mean(imfl[,F_species_kin], na.rm = TRUE)
      attributes(mn[,F_species_kin])$unit <- "mol m-2 s-1"
      
      #Chemical flux in mass units [mg m-2 h-1]
      imfl[,F_species_mass] <- imfl[,F_species_kin] * speciesRMM[[i]] * 1e6 * 3600
      attributes(imfl[,F_species_mass])$unit <- "mg m-2 h-1"
      mn[,F_species_mass] <- mean(imfl[,F_species_mass], na.rm = TRUE)
      attributes(mn[,F_species_mass])$unit <- "mg m-2 h-1"
      
      #correlation
      cor[,F_species_kin] <- suppressWarnings(stats::cor(imfl$w_hor, imfl[,F_species_var], use="pairwise.complete.obs"))
      cor[,F_species_mass] <- cor[,F_species_kin]
    }
    
    #Return
    list(imfl = imfl,
         mn = mn,
         cor = cor
         )
    
}
