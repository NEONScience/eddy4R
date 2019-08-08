#' Define Chemical Species Flux
#' 
#' General Version of the CH4 flux calculation. Can use built in species RMM from IntlNatu or supply custom values
#' 
#' @param imfl immediate fluctuations, created during "BASE STATE AND DEVIATIONS" step. [data.frame]
#' @param mn mean of input data. created during "TIME SERIES AVERAGES" step [data.frame]
#' @param corr correlations [data.frame]
#' @param base base state of data, created during "BASE STATE AND DEVIATIONS" step. [data.frame]
#' @param spcs names of species as molecular formulae, using IUPAC style where approprate
#'   e.g \code{c("NO","NO2","C6H6")} [character vector]
#' @param spcsRMM relative molecular mass of spcs in kg mol-1. must be of equal length to spcs
#'  e.g \code{c(0.03001,0.46005,0.07811)}. See \code{eddy4R.base::IntlNatu$Molm*} for some built in spcsRMM
#'
#' @return list containing the updated imfl, mn and cor objects
#' 
#' @author W. S. Drysdale
#' 
#' @export

def.flux.chem = function(imfl,
                         mn,
                         corr,
                         base,
                         spcs,
                         spcsRMM){
  
  if(length(spcs) != length(spcsRMM))
    stop("in def.flux.chem() length of spcs does not equal length of spcsRMM")

    for (i in 1:length(spcs)){
      #Define  name of mole ratio for chemical species
      F_spcs_var <- def.spcs.name(spcs[i],"mole")
      #Define name of chemical species kinamatic flux
      F_spcs_kin <- def.spcs.name(spcs[i],"kin")
      #Define name of chemical species mass flux
      F_spcs_mass <- def.spcs.name(spcs[i],"mass")
      
      #Chemical flux in kinematic units [mol m-2 s-1]
      imfl[,F_spcs_kin] <- base$rho_dry * imfl$w_hor * imfl[,F_spcs_var]
      attributes(imfl[,F_spcs_kin])$unit <- "mol m-2 s-1"
      mn[,F_spcs_kin] <- mean(imfl[,F_spcs_kin], na.rm = TRUE)
      attributes(mn[,F_spcs_kin])$unit <- "mol m-2 s-1"
      
      #Chemical flux in mass units [mg m-2 h-1]
      imfl[,F_spcs_mass] <- imfl[,F_spcs_kin] * spcsRMM[[i]] * 1e6 * 3600
      attributes(imfl[,F_spcs_mass])$unit <- "mg m-2 h-1"
      mn[,F_spcs_mass] <- mean(imfl[,F_spcs_mass], na.rm = TRUE)
      attributes(mn[,F_spcs_mass])$unit <- "mg m-2 h-1"
      
      #correlation
      corr[,F_spcs_kin] <- suppressWarnings(stats::cor(imfl$w_hor, imfl[,F_spcs_var], use="pairwise.complete.obs"))
      corr[,F_spcs_mass] <- corr[,F_spcs_kin]
    }
    
    #Return
    list(imfl = imfl,
         mn = mn,
         corr = corr
         )
    
}
