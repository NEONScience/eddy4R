##############################################################################################
#' @title Definition function: Calculation of the thermodynamic properties of a mix of dry air and water vapor

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function definition. Calculation of the thermodynamic properties of a mix of dry air and water vapor.

#' @param rtioMoleDryH2o A vector containing the water vapor dry mole fraction, of class "numeric". [molH2o mol-1Dry]

#' @return 
#' The returned object is a data frame with the same number of observations as rtioMoleDryH2o, containing the following variables:
#'    $cpWet: specific heat at constant pressure of dry air + water vapor mix. [J kg-1 K-1]
#'    $cvWet: specific heat at constant volume of dry air + water vapor mix. [J kg-1 K-1]
#'    $rsWet: specific gas constant. [J kg-1 K-1]
#'    $gmmaWet: ratio of specific heat at constant pressure to specific heat at constant volume. [-]
#'    $kppaWet: Kappa exponent for ideal gas law (Poisson's equation of adiabatic change). [-]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords thermodynamics, adiabatic change, water vapor, constant

#' @examples
#' Example 1, this will cause an error message due to tempAir has no unit:
#' def.natu.air.wet(rtioMoleDryH2o = 0.0003)
#' Example 2, assign values and units to variables first, the function should run ok.
#' rtioMoleDryH2o <- 0.0003
#' attributes(rtioMoleDryH2o)$unit <- "molH2o mol-1Dry"
#' def.natu.air.wet(rtioMoleDryH2o)

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-03-04)
#     original creation
#   Stefan Metzger (2021-11-17)
#     update to eddy4R terminology and modularize into definition function
###############################################################################################

# thermodynamic properties of a mix of dry air and water vapor
def.natu.air.wet <- function(
  
  # water vapor dry mole fraction
  rtioMoleDryH2o
  
) {
  
  # test for presence of unit attribute
  
  if(!("unit" %in% names(attributes(rtioMoleDryH2o)))) {
    
    stop("def.natu.air.wet(): rtioMoleDryH2o is missing unit attribute.")
    
  }
  
  # test for correct units of input variables
  if(attributes(rtioMoleDryH2o)$unit != "molH2o mol-1Dry") {
    
    stop("def.natu.air.wet(): input units are not matching internal units, please check.")
    
  }
  
  # perform calculations
  
    # initialize data frame  
    # calculate specific heat at constant pressure as function of humidity (Webb 1980 Eq 40) [J kg-1 K-1] == [m2 s-2 K-1]
    # density-based formulation identical to within 1e-13 (keep here for future extensibility)
    # natuAirWet$cpWet <- (eddy4R.base::IntlNatu$CpDry * densMoleAirDry + eddy4R.base::IntlNatu$CpH2o * densMoleH2o) / densMoleAir
    natuAirWet <- data.frame(cpWet = (eddy4R.base::IntlNatu$CpDry * 1 + eddy4R.base::IntlNatu$CpH2o * rtioMoleDryH2o) / (1 + rtioMoleDryH2o))
    attr(natuAirWet$cpWet,"unit") <- "J kg-1 K-1"
    
    # calculate specific heat at constant volume as function of humidity [J kg-1 K-1] == [m2 s-2 K-1]
    # natuAirWet$cvWet <- (eddy4R.base::IntlNatu$CvDry * densMoleAirDry + eddy4R.base::IntlNatu$CvH2o * densMoleH2o) / densMoleAir
    natuAirWet$cvWet <- (eddy4R.base::IntlNatu$CvDry * 1 + eddy4R.base::IntlNatu$CvH2o * rtioMoleDryH2o) / (1 + rtioMoleDryH2o)
    attr(natuAirWet$cvWet,"unit") <- "J kg-1 K-1"
    
    # calculate specific gas constant as function of humidity [J kg-1 K-1] == [m2 s-2 K-1]
    natuAirWet$rsWet <- natuAirWet$cpWet - natuAirWet$cvWet
    attr(natuAirWet$rsWet,"unit") <- "J kg-1 K-1"
    
    # calculate ratio of specific heat at constant pressure to specific heat at constant volume as function of humidity [-]
    natuAirWet$gmmaWet <- natuAirWet$cpWet / natuAirWet$cvWet
    attr(natuAirWet$gmmaWet,"unit") <- "-"
    
    # calculate kappa exponent for ideal gas law (Poisson) as function of humidity [-]
    natuAirWet$kppaWet <- natuAirWet$rsWet / natuAirWet$cpWet
    attr(natuAirWet$kppaWet,"unit") <- "-"
  
  # return results
  return(natuAirWet) 
  
}
