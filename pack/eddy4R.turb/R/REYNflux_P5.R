##############################################################################################
#' @title Definition function: Calculate turbulent vertical flux and auxiliary variables

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function defintion. Calculate turbulent vertical flux and auxiliary variables.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords eddy-covariance, turbulent flux

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-06-12)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Cove Sturtevant (2016-02-16)
#     updated reference to base.state.r --> def.ec.state.base.R and associated input arguments
#   Ke Xu (2016-09-07)
#     change sapply parameter "simplify" to FALSE, and transfer the resulted list to dataframe data type
#   Ke Xu (2016-09-19)
#     Add two arguments PltfEc and flagCh4 to adjust tower data
#   Will Drysdale (2018-01-25)
#     Added Support for other species to be defined in SiteInfo
#   Adam Vaughan (2018-11-27)
#     Added Water Vapour Switch for all functions
#   Adam Vaughan (2018-11-27)
#     Added Chemistry Flux Calculation for non-methane tracers
#   Will Drysdale (2019-01-29)
#     Removed SiteInfo dependance, added switches as explicit variables
#   Will Drysdale (2019-02-16)
#     Convert Chemistry Flux to def.flux.chem and rotation to def.rot. Update some related variable names
##############################################################################################



# attributes(data)$names
# [1] "t_utc"        "d_x_utm"      "d_y_utm"      "d_z_m"        "d_xy_travel"  "d_xy_flow"    "PSI_aircraft" "uvw"         
# [9] "u_met"        "v_met"        "w_met"        "p_air"        "T_air"        "FD_mole_H2O"  "FD_mole_CH4"  "d_z_terrain" 
# [17] "d_z_ABL"      "T_surface"    "R_SW_down"


REYNflux_FD_mole_dry <- function(
  data = eddy.data,
# -------------------------------------- optional, can omit here for simplicity, just make sure to include ...
  AlgBase = c("mean", "trnd", "ord03")[1],
  SlctPot = FALSE,
  PresPot = eddy4R.base::IntlNatu$Pres00,  
# -------------------------------------- these should go away
  PltfEc = "airc",
  flagCh4 = TRUE,
  spcs = NULL,
  rmm = NULL,
  ...
)
{
  
  ### rename input data in preparation of terms update (replace with unit test when completing refactoring)
  # keep "data" data.frame object with minimum required variables that are tested for in the beginning of the wrapper
  # in that way additional variables can be supplied (e.g., UTM coordinates for aircraft) and stats calculated
  # algorithm / processing settings as separate, individual specifications of the function call
  
    # to be consolidated
    # $ t_utc         : num 0.25
    base::attr(x = data$t_utc, which = "unit") <- "h"
    # $ t_doy_utc     : num 332
    base::attr(x = data$t_doy_utc, which = "unit") <- "d"
    # $ t_doy_local   : num 332
    base::attr(x = data$t_doy_local, which = "unit") <- "d"
    # $ d_x_utm       : num 710730
    base::attr(x = data$d_x_utm, which = "unit") <- "m"
    # $ d_y_utm       : num 4330788
    base::attr(x = data$d_y_utm, which = "unit") <- "m"
    # $ d_z_m         : num 8
    base::attr(x = data$d_z_m, which = "unit") <- "m"
    # $ d_xy_travel   : num 900
    base::attr(x = data$d_xy_travel, which = "unit") <- "s"
    # re-assign as independent variable, which can be used exchangably for tower, aircraft etc. with corresponding units
    data$idep <- data$d_xy_travel
    # $ d_xy_flow     : num 900
    base::attr(x = data$d_xy_flow, which = "unit") <- "s"
    # $ PSI_aircraft  : num 290
    data$PSI_aircraft <- eddy4R.base::def.conv.poly(data = data$PSI_aircraft, coefPoly = eddy4R.base::IntlConv$DegRad)
    attributes(data$PSI_aircraft)$unit <- "rad"
    # $ uvw_aircraft  : num 2.02
    base::attr(x = data$uvw_aircraft, which = "unit") <- "m s-1"
    # $ uv_met        : num 2.02
    base::attr(x = data$uv_met, which = "unit") <- "m s-1"
    # $ d_z_terrain   : num 414
    base::attr(x = data$d_z_terrain, which = "unit") <- "m"
    # $ d_z_ABL       : num 1000
    base::attr(x = data$d_z_ABL, which = "unit") <- "m"
  
    # wind vector
    data$veloXaxs <- data$u_met
      data$u_met <- NULL
      base::attr(x = data$veloXaxs, which = "unit") <- "m s-1"
    data$veloYaxs <- data$v_met
      data$v_met <- NULL
      base::attr(x = data$veloYaxs, which = "unit") <- "m s-1"
    data$veloZaxs <- data$w_met
      data$w_met <- NULL
      base::attr(x = data$veloZaxs, which = "unit") <- "m s-1"
    
    # scalars
    data$presAtm <- data$p_air
      data$p_air <- NULL
      base::attr(x = data$presAtm, which = "unit") <- "Pa"
    data$tempAir <- data$T_air
      data$T_air <- NULL
      base::attr(x = data$tempAir, which = "unit") <- "K"
    data$rtioMoleDryH2o <- data$FD_mole_H2O
      data$FD_mole_H2O <- NULL
      base::attr(x = data$rtioMoleDryH2o, which = "unit") <- "molH2o mol-1Dry"
    data$rtioMoleDryCo2 <- data$FD_mole_CH4
      data$FD_mole_CH4 <- NULL
      base::attr(x = data$rtioMoleDryCo2, which = "unit") <- "molCo2 mol-1Dry"
  
  
  
  ############################################################
  # THERMODYNAMICS: MOISTURE, DENSITIES AND TEMPERATURES
  ############################################################
  
  #-----------------------------------------------------------
  # GENERAL CONVERSIONS
  
  # partial pressure of water vapor [Pa = kg m-1 m-2]; add unit that is missing from def.pres.h2o.rtio.mole.h2o.dry.pres()
  data$presH2o <- eddy4R.base::def.pres.h2o.rtio.mole.h2o.dry.pres(
    rtioMoleDryH2o = data$rtioMoleDryH2o,
    pres = data$presAtm)
  base::attr(x = data$presH2o, which = "unit") <- "Pa"
  
  # molar density of H2O [mol m-3]
  data$densMoleH2o <- eddy4R.base::def.dens.mole.air(
    presSum = data$presH2o,
    tempMean = data$tempAir)

  # total (wet) air density [mol m-3]
  data$densMoleAir <- eddy4R.base::def.dens.mole.air(
    presSum = data$presAtm,
    tempMean = data$tempAir)
  
  # dry air density [mol m-3]; temporarily change data$densMoleH2o attribute to pass (inconsistent) def.dens.mole.air.dry() unit test
  base::attr(x = data$densMoleH2o, which = "unit") <- "molH2o m-3"
  data$densMoleAirDry <- eddy4R.base::def.dens.mole.air.dry(
    densMoleAir = data$densMoleAir,
    densMoleH2o = data$densMoleH2o)
  base::attr(x = data$densMoleH2o, which = "unit") <- "mol m-3"

  # virtual temperature [K]
  # definition function (to be exported)
  {
    ##############################################################################################
    #' @title Definition function: Calculation of the virtual temperature
    
    #' @author
    #' Stefan Metzger \email{eddy4R.info@gmail.com}
    
    #' @description Function definition. Calculation of the virtual temperature.
    #' The virtual temperature is the temperature of a moist air parcel at which a theoretical 
    #' dry air parcel would have a total pressure and density equal to the moist parcel of air.
    
    #' @param tempAir A vector containing the air temperature, of class "numeric". [K]
    #' @param presH2o A vector containing the water vapor partial pressure, of class "numeric". [Pa]
    #' @param presAtm A vector containing the atmospheric pressure, of class "numeric". [Pa]
    
    #' @return 
    #' The returned object is the the virtual temperature. [K]
    
    #' @references
    #' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
    #' http://en.wikipedia.org/wiki/Virtual_temperature
    
    #' @keywords temperature, pressure, virtual
    
    #' @examples
    #' Example 1, this will cause an error message due to tempAir, presH2o, presAtm have no units:
    #' def.temp.virt.temp.air.pres.h2o.pres.atm(tempAir = 268, presH2o = 30, presAtm = 93344)
    #' Example 2, assign values and units to variables first, the function should run ok.
    #' tempAir <- 268
    #' presH2o <- 30
    #' presAtm <- 93344
    #' attributes(tempAir)$unit <- "K"
    #' attributes(presH2o)$unit <- "Pa"
    #' attributes(presAtm)$unit <- "Pa"
    #' def.temp.virt.temp.air.pres.h2o.pres.atm(tempAir, presH2o, presAtm)
    
    #' @seealso Currently none.
    
    #' @export
    
    # changelog and author contributions / copyrights
    #   Stefan Metzger (2011-03-04)
    #     original creation
    #   Stefan Metzger (2021-11-16)
    #     update to eddy4R terminology and modularize into definition function
    ###############################################################################################
    
    # molar density of the mixture of dry air and water vapor
    def.temp.virt.temp.air.pres.h2o.pres.atm <- function(
      
      # air temperature
      tempAir,
      
      # water vapor partial pressure
      presH2o,
      
      # atmospheric pressure
      presAtm
      
    ) {
      
      # test for presence of unit attribute
      
      if(!("unit" %in% names(attributes(tempAir)))) {
        
        stop("def.temp.virt.temp.air.pres.h2o.pres.atm(): tempAir is missing unit attribute.")
        
      }
      
      
      if(!("unit" %in% names(attributes(presH2o)))) {
        
        stop("def.temp.virt.temp.air.pres.h2o.pres.atm(): presH2o is missing unit attribute.")
        
      }
      
      if(!("unit" %in% names(attributes(presAtm)))) {
        
        stop("def.temp.virt.temp.air.pres.h2o.pres.atm(): presAtm is missing unit attribute.")
        
      }
      
      # test for correct units of input variables
      if(attributes(tempAir)$unit != "K" || attributes(presH2o)$unit != "Pa" || attributes(presAtm)$unit != "Pa") {
        
        stop("def.temp.virt.temp.air.pres.h2o.pres.atm(): input units are not matching internal units, please check.")
        
      }
      
      
      # calculate the virtual temperature
      tempVirt <- tempAir / (1 - ((presH2o / presAtm) * (1 - eddy4R.base::IntlNatu$RtioMolmH2oDry)) )
      
      # alternate formulation based on specific humidity (retained for future extensibility): tempVirt <- tempAir * (1 + 0.61 * q)
  
      # assign output unit
      attributes(tempVirt)$unit <- "K"
      
      # return results
      return(tempVirt) 
      
    }
  }
  # actual calculation
  data$tempVirt <- def.temp.virt.temp.air.pres.h2o.pres.atm(
    tempAir = data$tempAir,
    presH2o = data$presH2o,
    presAtm = data$presAtm)

  # volumetric heat capacity [kg m-1 s-2 K-1] = specific heat [J kg-1 K-1] x mole density [mol m-3] x mole mass [kg mol-1]
  # definition function (to be exported)
  {
    ##############################################################################################
    #' @title Definition function: Calculation of the volumetric heat capacity
    
    #' @author
    #' Stefan Metzger \email{eddy4R.info@gmail.com}
    
    #' @description Function definition. Calculation of the volumetric heat capacity
    
    #' @param densMoleAirDry A vector containing the dry air mole density, of class "numeric". [mol m-3]
    #' @param densMoleH2o A vector containing the H2O mole density, of class "numeric". [mol m-3]
    
    #' @return 
    #' The returned object is the volumetric heat capacity. [kg m-1 s-2 K-1] = specific heat [J kg-1 K-1] x mole density [mol m-3] x mole mass [kg mol-1]
    
    #' @references
    #' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
    
    #' @keywords sensible heat flux, temperature, heat
    
    #' @examples
    #' Example 1, this will cause an error message due to densMoleAirDry, densMoleH2o have no units:
    #' def.heat.air.wet(densMoleAirDry = 41.9054, densMoleH2o = 0.01342256)
    #' Example 2, assign values and units to variables first, the function should run ok.
    #' densMoleAirDry = 41.9054; attributes(densMoleAirDry)$unit <- "mol m-3"
    #' densMoleH2o = 0.01342256; attributes(densMoleH2o)$unit <- "mol m-3"
    #' def.heat.air.wet(densMoleAirDry, densMoleH2o)

    #' @seealso Currently none.
    
    #' @export
    
    # changelog and author contributions / copyrights
    #   Stefan Metzger (2011-03-04)
    #     original creation
    #   Stefan Metzger (2022-03-22)
    #     update to eddy4R terminology and modularize into definition function
    ###############################################################################################
    
    # volumetric heat capacity
    def.heat.air.wet <- function(

      # dry air mole density
      densMoleAirDry,

      # H2O mole density
      densMoleH2o
      
    ) {
      
      # test for presence of unit attribute
      
        # densMoleAirDry
        if(!("unit" %in% names(attributes(densMoleAirDry)))) {
          
          stop("def.heat.air.wet(): densMoleAirDry is missing unit attribute.")
          
        }

        # densMoleH2o
        if(!("unit" %in% names(attributes(densMoleH2o)))) {
          
          stop("def.heat.air.wet(): densMoleH2o is missing unit attribute.")
          
        }
      
      # test for correct units of input variables
      if(attributes(densMoleAirDry)$unit != "mol m-3" | attributes(densMoleH2o)$unit != "mol m-3") {
        
        stop("def.heat.air.wet(): input units are not matching internal units, please check.")
        
      }
      
      # calculate the volumetric heat capacity
      heatAirWet <- 
        # dry air 
        eddy4R.base::IntlNatu$CpDry * densMoleAirDry * eddy4R.base::IntlNatu$MolmDry + 
        # water vapor  
        eddy4R.base::IntlNatu$CpH2o * densMoleH2o * eddy4R.base::IntlNatu$MolmH2o    
      
      # assign output unit
      base::attr(heatAirWet, which = "unit") <- "kg m-1 s2 K-1"
      
      # return results
      base::return(heatAirWet)
      
    }
  }
  # actual calculation
  data$heatAirWet <- def.heat.air.wet(densMoleAirDry = data$densMoleAirDry, densMoleH2o = data$densMoleH2o)
  
  # latent heat of vaporization (Eq 2.55 Foken 2008) [J kg-1] == [m2 s-2]
  # definition function (to be exported)
  {
    ##############################################################################################
    #' @title Definition function: Calculation of the latent heat of vaporization
    
    #' @author
    #' Stefan Metzger \email{eddy4R.info@gmail.com}
    
    #' @description Function definition. Calculation of the latent heat of vaporization.
    
    #' @param tempAir A vector containing the air temperature, of class "numeric". [K]
    
    #' @return 
    #' The returned object is the latent heat of vaporization. [J kg-1] == [m2 s-2]
    
    #' @references
    #' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
    #' Foken 2008, Eq 2.55
    
    #' @keywords temperature, gas phase, liquid phase, heat
    
    #' @examples
    #' Example 1, this will cause an error message due to tempAir has no unit:
    #' def.heat.h2o.gas.temp(tempAir = 268)
    #' Example 2, assign values and units to variables first, the function should run ok.
    #' tempAir <- 268
    #' attributes(tempAir)$unit <- "K"
    #' def.heat.h2o.gas.temp(tempAir)
    
    #' @seealso Currently none.
    
    #' @export
    
    # changelog and author contributions / copyrights
    #   Stefan Metzger (2011-03-04)
    #     original creation
    #   Stefan Metzger (2021-11-17)
    #     update to eddy4R terminology and modularize into definition function
    ###############################################################################################
    
    # latent heat of vaporization
    def.heat.h2o.gas.temp <- function(
      
      # air temperature
      tempAir
      
    ) {
      
      # test for presence of unit attribute
      
      if(!("unit" %in% names(attributes(tempAir)))) {
        
        stop("def.heat.h2o.gas.temp(): tempAir is missing unit attribute.")
        
      }
      
      # test for correct units of input variables
      if(attributes(tempAir)$unit != "K") {
        
        stop("def.heat.h2o.gas.temp(): input units are not matching internal units, please check.")
        
      }
      
      
      # calculate the latent heat of vaporization
      heatH2oGas <- 2500827 - 2360 * eddy4R.base::def.unit.conv(data=as.numeric(tempAir), unitFrom="K", unitTo="C")
      
      # assign output unit
      attributes(heatH2oGas)$unit <- "J kg-1"
      
      # return results
      return(heatH2oGas) 
      
    }
  }
  # actual calculation
  data$heatH2oGas <- def.heat.h2o.gas.temp(tempAir = data$tempAir)

  # Thermodynamic properties of a mix of dry air and water vapor
  # definition function (to be exported)
  {
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
  }
  # actual calculation
  data <- base::cbind(data, def.natu.air.wet(rtioMoleDryH2o = data$rtioMoleDryH2o))

  
  #-----------------------------------------------------------
  # POTENTIAL TEMPERATURE AND DENSITIES
  {
  # mean kappa exponent for ideal gas law (Poisson) as function of humidity
  KppaWet <- base::mean(data$kppaWet, na.rm = TRUE)
  attr(KppaWet,"unit") <- "-"
  
  # potential temperature at NIST standard pressure (1013.15 hPa) [K]; here only for completeness (not further used)
  data$tempPot00 <- eddy4R.base::def.temp.pres.pois(
    temp01 = data$tempAir,
    pres01 = data$presAtm,
    pres02 = eddy4R.base::IntlNatu$Pres00,
    Kppa = KppaWet)
  
  # virtual potential temperature at NIST standard pressure (1013.15 hPa) [K]; used to calculate buoyancy flux, stability, Deardorff velocity
  data$tempVirtPot00 <- eddy4R.base::def.temp.pres.pois(
    temp01 = data$tempVirt,
    pres01 = data$presAtm,
    pres02 = eddy4R.base::IntlNatu$Pres00,
    Kppa = KppaWet)
  
  # use potential temperature and densities?
  if(SlctPot == TRUE) {
    
    # check correct units if potential pressure reference is provided
    if(!is.null(PresPot)){
    
      # test for presence of unit attribute
      if(!("unit" %in% names(attributes(PresPot)))) {
        stop("PresPot is missing unit attribute.")}
      
      # test for correct unit
      if(attributes(PresPot)$unit != "Pa") {
        stop("PresPot units are not matching internal units, please check.")}
      
    }
    
    # define pressure level for potential quantities
    PresPotLoca <- ifelse(!is.null(PresPot), PresPot, base::mean(data$presAtm, na.rm=TRUE))
    base::attr(x = PresPotLoca, which = "unit") <- "Pa"
    
    # potential temperature at defined pressure level
    data$tempAir <- eddy4R.base::def.temp.pres.pois(
      temp01 = data$tempAir,
      pres01 = data$presAtm,
      pres02 = PresPotLoca,
      Kppa = KppaWet)
    
    # potential densities at defined pressure level
    
      # dry air
      data$densMoleAirDry <- eddy4R.base::def.dens.pres.pois(
        dens01 = data$densMoleAirDry,
        pres01 = data$presAtm,
        pres02 = PresPotLoca,
        Kppa = KppaWet)
      
      # H2O
      data$densMoleH2o <- eddy4R.base::def.dens.pres.pois(
        dens01 = data$densMoleH2o,
        pres01 = data$presAtm,
        pres02 = PresPotLoca,
        Kppa=KppaWet)
      
      # wet air
      data$densMoleAir <- eddy4R.base::def.dens.pres.pois(
        dens01 = data$densMoleAir,
        pres01 = data$presAtm,
        pres02 = PresPotLoca,
        Kppa = KppaWet)
      
      # clean up
      rm(PresPotLoca)
    
  }
  
  # clean up
  base::rm(KppaWet)
  }
  
  
  ############################################################
  # ROTATION INTO THE MEAN WIND
  ############################################################
  
    # definition function (to be exported)
  {
    ##############################################################################################
    #' @title Definition function: Streamwise rotation of the wind vector
    
    #' @author
    #' Stefan Metzger \email{eddy4R.info@gmail.com}
    
    #' @description Function definition. This function rotates the wind vector into the mean wind, thus allowing to separate stream-wise and cross-wind components for subsequent use in footprint models, calculating shear stress etc. It consists of a single azimuth-rotation around the vertical axis. Any more comprehensive rotation such as double rotation or planar fit should be applied prior to calling this function.
    
    #' @param inp A data frame containing the wind vector in meteorological convention with the variables veloXaxs (latitudinal wind speed, positive from west), veloYaxs (longitudinal wind speed, positive from south), and veloZaxs (vertical wind speed, positive from below) of class "numeric, each with unit attribute. [m s-1]
    
    #' @return 
    #' The returned object is a list containing the elements data and rot.
    #' data is a dataframe with the same number of observations as the function call inputs. It contains the wind vector variables in streamwise convention veloXaxsHor (streamwise wind speed, positive from front), veloYaxsHor (cross-wind speed, positive from left) and veloZaxsHor (vertical wind speed, positive from below) [m s-1], and the wind direction angZaxsErth [rad], each of class "numeric and with unit attribute.
    #' rot is a list with the objects used in the rotation, averaged over all observations in the function call inputs. It contains the mean wind direction angZaxsErth [rad], the resulting rotation matrix mtrxRot01 [-] and the transpose of the rotation matrix mtrxRot02 [-], each of class "numeric and with unit attribute. It should be noted that angZaxsErth is calculated by first averaging each horizontal component of the wind vector, which minimizes the mean cross-wind thus satisfying conditions for footprint modeling and separating shear into stream-wise and cross-wind terms.
    
    #' @references
    #' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
    
    #' @keywords rotation, stream-wise, mean wind, footprint, shear
    
    #' @examples
    #' Example 1, this will cause an error message due to inp01$veloYaxs is missing:
    #' inp01 <- base::data.frame(
    #'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
    #'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
    #' )
    #' attr(inp01$veloXaxs,"unit") <- "m s-1"; attr(inp01$veloZaxs,"unit") <- "m s-1"
    #' def.rot.ang.zaxs.erth(inp = inp01)
    #' base::rm(inp01)
    #' Example 2, make sure to assign all variables and units, the function should run ok.
    #' inp02 <- base::data.frame(
    #'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
    #'   veloYaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
    #'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
    #' )
    #' attr(inp02$veloXaxs,"unit") <- "m s-1"; attr(inp02$veloYaxs,"unit") <- "m s-1"; attr(inp02$veloZaxs,"unit") <- "m s-1"
    #' out02 <- def.rot.ang.zaxs.erth(inp = inp02)
    #' utils::str(out02)
    #' base::rm(inp02, out02)

    #' @seealso Currently none.
    
    #' @export
    
    # changelog and author contributions / copyrights
    #   Stefan Metzger (2011-03-04)
    #     original creation
    #   Stefan Metzger (2021-11-24)
    #     update to eddy4R terminology and modularize into definition function
    ###############################################################################################

        
    # rotation into the mean wind
    def.rot.ang.zaxs.erth <- function(
      
      # input dataframe with variables veloXaxs, veloYaxs, and veloZaxs of class "numeric, each with unit attribute. [m s-1]
      inp
      
    ) {
      
      # check that input is of class data.frame
      if(base::class(inp) != "data.frame") {
        stop(base::paste0("def.rot.ang.zaxs.erth(): inp is not of class data.frame, please check."))  
      }
      
      # test input variables and unit attributes
      for(idx in c("veloXaxs", "veloYaxs", "veloZaxs")){
      # idx <- "veloXaxs"
        
        # test for presence/absence of variables
        if(!(idx %in% base::names(inp))) {
          stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", idx, " is missing."))        }
        
        # test for presence/absence of unit attribute
        if(!("unit" %in% base::names(attributes(inp[[idx]])))) {
          stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", idx, " is missing unit attribute."))        }
        
        # test for correct units
        if(attributes(inp[[idx]])$unit != "m s-1") {
          stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", idx, 
                            " input units are not matching internal units, please check."))}
        
      }
      
      # clean up
      base::rm(idx)
  
      # calculate wind direction
        
        # instantaneous wind direction [rad]
        angZaxsErth <- eddy4R.base::def.unit.conv(data = eddy4R.base::def.pol.cart(cart = base::matrix(c(
          inp$veloYaxs,
          inp$veloXaxs), ncol=2)),
          unitFrom = "deg", unitTo = "rad")
      
        # mean wind direction [rad], based on first averaging each horizontal component of the wind vector
        # minimizes mean cross-wind, thus satisfying conditions for footprint modeling (required) 
        # and separating shear into stream-wise and cross-wind terms (optional)
        # however, dp04 results (124.824 deg) differ from reported dp01 (118.9117 deg,
        # for first 30 min in gold data per 2021-11-23)
        # that is because dp01are based on INSTANTANEOUS wind directions 
        # (eddy4R.base::wrap.dp01.R calls eddy4R.base::def.dir.wind(inp = dataLoca$soni$angZaxsErth, MethVari = "Yama"))
        # how to best reconcile, different community standards for dp01 (states -> 2D sonics) and dp04 (fluxes)?
        angZaxsErthMean <- eddy4R.base::def.unit.conv(data = eddy4R.base::def.pol.cart(cart = base::matrix(c(
          base::mean(inp$veloYaxs, na.rm = TRUE),
          base::mean(inp$veloXaxs, na.rm = TRUE)), ncol=2)),
          unitFrom = "deg", unitTo = "rad")
      
      # rotation angle
      angRot <- (angZaxsErthMean + base::pi) %% (2 * base::pi)
      
      # rotation matrix
      mtrxRot01 <- base::matrix(nrow=3, ncol=3)
        mtrxRot01[1,1] <- base::cos(angRot)
        mtrxRot01[1,2] <- base::sin(angRot)
        mtrxRot01[1,3] <- 0.
        mtrxRot01[2,1] <- -base::sin(angRot)
        mtrxRot01[2,2] <- base::cos(angRot)
        mtrxRot01[2,3] <- 0.
        mtrxRot01[3,1] <- 0.
        mtrxRot01[3,2] <- 0.
        mtrxRot01[3,3] <- 1.
        
      # transpose of rotation matrix
      mtrxRot02 <- base::t(mtrxRot01)
      
      # wind velocity vector in (horizontal) geodetic coordinates
      veloVect <- rbind(inp$veloYaxs, inp$veloXaxs, inp$veloZaxs)
      
      # actual rotation
      veloVectRot <- mtrxRot01 %*% veloVect
        
      # create object for export
      
        # create list
        rpt <- base::list()

        # populate rpt$data
        
          # along-wind
          rpt$data <- base::data.frame(veloXaxsHor = veloVectRot[1,])
          attr(rpt$data$veloXaxsHor,"unit") <- "m s-1"
          
          # cross-wind
          # requires mirroring as output is still in geodetic axes order, downstream impact on diff$u_star2_y
          rpt$data$veloYaxsHor <- -veloVectRot[2,]
          attr(rpt$data$veloYaxsHor,"unit") <- "m s-1"
          
          # vertical wind
          rpt$data$veloZaxsHor <- veloVectRot[3,]
          attr(rpt$data$veloZaxsHor,"unit") <- "m s-1"
          
          # wind direction
          rpt$data$angZaxsErth <- angZaxsErth
          
        # populate rpt$rot
          
          # assign data
          rpt$rot <- base::list(
            angZaxsErth = angZaxsErthMean,
            mtrxRot01 = mtrxRot01,
            mtrxRot02 = mtrxRot02)
          
          # assign output units
          attributes(rpt$rot$angZaxsErth)$unit <- "rad"
          attributes(rpt$rot$mtrxRot01)$unit <- "-"
          attributes(rpt$rot$mtrxRot02)$unit <- "-"

      # clean up
      rm(angRot, angZaxsErth, angZaxsErthMean, inp, mtrxRot01, mtrxRot02, veloVect, veloVectRot)
      
      # return results
      return(rpt) 
      
    }
  }
  
  # actual calculation
  
  # function call
  dataTmp <- def.rot.ang.zaxs.erth(inp = data[c("veloXaxs", "veloYaxs", "veloZaxs")])
  
  # assign results and clean up
  data <- base::cbind(data, dataTmp$data)
  rot <- dataTmp$rot
  base::rm(dataTmp)
  
  
  
  ############################################################
  # SUMMARY STATISTICS, BASE STATES AND DIFFERENCES
  ############################################################
  
  
  # definition function (to be exported)
  {
    ##############################################################################################
    #' @title Definition function: Summary statistics, base states, instantaneous differences
    
    #' @author
    #' Stefan Metzger \email{eddy4R.info@gmail.com}
    
    #' @description Function definition. The function calculates summary statistics (min, max, mean), selected base state (mean, trend, 3rd order polynomial), and the instantaneous differences and standard deviations with respect to the selected base state. This enables subsequent calculations (eddy-covariance turbulent fluxes, footprint modeling etc.) with respect to the selected base state.
    
    #' @param inp A data frame containing the variables for which to perform the calculations, class "numeric", each with unit attribute.
    #' @param refe A list of reference quantities that can be supplied to overwrite internal calculations, class "numeric", each with unit attribute. Currently implemented only for refe$mean in combination with inp variables in units [rad].
    #' @param AlgBase A vector of length 1 that defines the base state with respect to which instantaneous differences and standard deviations are calculated, of class "character" and no unit attribute. Contains one of AlgBase <- c("mean", "trnd", "ord03")[1] and defaults to "mean", with the additional options detrending "trnd" and 3rd-order polynomial "ord03". See ?eddy4R.base::def.base.ec() for additional details. When AlgBase is set to "trnd" or "ord03", the variable inp$idep is required, which provides the independent variable for interpolation.
    
    #' @return 
    #' The returned object is a list containing the element dataframes min, max, mean, base, diff, and sd, each of class "numeric" and with unit attributes. The elements min, max and mean are the minimum, maximum and mean of the variables in inp with a single observation. The element base is the base state for each of the variables in inp, with a single observation (AlgBase == "mean") or the same number of observations as inp (AlgBase %in% c("trnd", "ord03")). The element diff are the point-by-point differences from the selected base state for each of the variables in inp, with the same number of observations as inp. The element sd are the standard deviations of diff for each of the variables in inp, with a single observation. It should be noted that the mean (and base state for AlgBase == "mean") for angular quantities with unit [rad] is computed from 1) point-wise unit vector decomposition to polar coordinates [-], 2) averaging the polar coordinates [-], and 3) re-composing the mean polar coordinates to Cartesian angle [rad]. In the special case of wind direction (example: angZaxsErth) it is recommended to supply the argument refe$mean$angZaxsErth to the function call as provided from ?def.rot.ang.zaxs.erth. This ensures consistency with downstream applications for footprint modeling, separating shear into stream-wise and cross-wind terms etc. (minimizes the mean cross-wind from directly averaging the wind vector horizontal components instead of unit vector components).
    
    #' @references
    #' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
    
    #' @keywords minimum, maximum, mean, statistics, standard deviation, turbulence
    
    #' @examples
    #' Example 1, this will cause an error message due to missing unit attribute for inp01$veloYaxs:
    #' inp01 <- base::data.frame(
    #'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
    #'   veloYaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
    #'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
    #' )
    #' attr(inp01$veloXaxs,"unit") <- "m s-1"; attr(inp01$veloZaxs,"unit") <- "m s-1"
    #' def.stat.sta.diff(inp = inp01)
    #' base::rm(inp01)
    #' Example 2, make sure to assign all variables and units, the function should run ok.
    #' inp02 <- base::data.frame(
    #'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
    #'   veloYaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
    #'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
    #' )
    #' attr(inp02$veloXaxs,"unit") <- "m s-1"; attr(inp02$veloYaxs,"unit") <- "m s-1"; attr(inp02$veloZaxs,"unit") <- "m s-1"
    #' out02 <- def.stat.sta.diff(inp = inp02)
    #' utils::str(out02)
    #' base::rm(out02)
    #' Example 3, difference when computing angular average without and with reference (wind direction).
    #' out03 <- def.stat.sta.diff(inp = def.rot.ang.zaxs.erth(inp = inp02)$data)
    #' out04 <- def.stat.sta.diff(inp = def.rot.ang.zaxs.erth(inp = inp02)$data,
    #'                            refe = base::list("mean" = base::list("angZaxsErth" = 
    #'                                                                    def.rot.ang.zaxs.erth(inp = inp02)$rot$angZaxsErth))
    #'                            )
    #' utils::str(out03)
    #' utils::str(out04)
    #' out03$mean$angZaxsErth - out04$mean$angZaxsErth
    #' base::rm(inp02, out03, out04)
    
    #' @seealso Currently none.
    
    #' @export
    
    # changelog and author contributions / copyrights
    #   Stefan Metzger (2011-03-04)
    #     original creation
    #   Stefan Metzger (2022-02-07)
    #     update to eddy4R terminology and modularize into definition function
    ###############################################################################################
    
    
    # Summary statistics, base states, instantaneous differences
    def.stat.sta.diff <- function(
      
      # input dataframe containing the variables for which to perform the calculations, each with unit attribute
      inp,
      
      # list of reference quantities that can be supplied to overwrite internal calculations
      # currently implemented only for refe$mean in combination with inp variables in units [rad]
      refe = NULL,
      
      # base state, defaults to "mean", additional options are detrending "trnd" and 3rd-order polynomial "ord03"
      AlgBase = c("mean", "trnd", "ord03")[1]
    ) {

      # check that input is of class data.frame
      if(base::class(inp) != "data.frame") {
        stop(base::paste0("def.stat.sta.diff(): inp is not of class data.frame, please check."))  
      }
    
        # test input variables and unit attributes
        for(idx in base::names(inp)){
          # idx <- "veloXaxs"
          
          # test for presence/absence of unit attribute
          if(!("unit" %in% base::names(attributes(inp[[idx]])))) {
            stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", idx, " is missing unit attribute."))        }
      
        }; base::rm(idx)
      
        # check that inp$idep is present in case AlgBase != "mean"
        if(AlgBase != "mean" & !("idep" %in% base::names(inp))) {
          stop(base::paste0("def.stat.sta.diff(): please specify function argument inp$idep when AlgBase != 'mean'."))}
      
      
      # minimum and maximum
      
        # min
      
          # calculate
          min <- plyr::colwise("min")(inp, na.rm=TRUE)
          
          # apply units from inp to min
          base::sapply(base::names(min), function(x) {base::attr(min[[x]], which = "unit") <<- 
            base::attr(inp[[x]], which = "unit")})
          
        # max
          
          # calculate
          max <- plyr::colwise("max")(inp, na.rm=TRUE)
          
          # apply units from inp to max
          base::sapply(base::names(max), function(x) {base::attr(max[[x]], which = "unit") <<- 
            base::attr(inp[[x]], which = "unit")})
    
      
      # always calculate means as reference
      
        # calculate means for euclidean (linear, cartensian) quantities
        mean <- plyr::colwise("mean")(inp, na.rm = TRUE)
    
        # apply units from inp to mean
        base::sapply(base::names(mean), function(x) {base::attr(mean[[x]], which = "unit") <<- 
          base::attr(inp[[x]], which = "unit")})
        
        # re-calculate / re-assign means for circular (polar) quantities as vector average
        
          # test for correct units
        
            # determine if there are any variables in unit "deg"
            tmp01 <- base::sapply(base::names(mean), function(x) {base::attr(mean[[x]], which = "unit") == "deg"})
            
            # stop and print error message to screen
            if(length(which(tmp01)) > 0) {
              stop(base::paste0("def.stat.sta.diff(): ", base::names(base::which(tmp01)), 
                                " units are not matching internal units (radians), please check."))}
            
            # clean up
            base::rm(tmp01)
          
          # determine whether there are any variables in unit "rad"
          tmp02 <- base::sapply(base::names(mean), function(x) {base::attr(mean[[x]], which = "unit") == "rad"})
          
          # continue only if there is at least one variable in unit "rad"
          if(length(which(tmp02)) > 0) {
            
            # variables to re-calculate / re-assign
            tmp03 <- base::names(base::which(tmp02))
            
            # re-calculate / re-assign
            for(idx in tmp03) {
              
              # re-assign reference value if provided
              if(!is.null(refe$mean) & idx %in% base::names(refe$mean)) {
                
                # check that refe is of class list
                if(base::class(refe) != "list") {
                  stop(base::paste0("def.stat.sta.diff(): refe is not of class list, please check."))  
                }
                
                # test for correct units
                if(attributes(refe$mean[[idx]])$unit != attributes(inp[[idx]])$unit) {
                  stop(base::paste0("def.stat.sta.diff(): refe$mean$", idx, 
                                    " unit is not matching inp$", idx, " unit, please check."))}
                
                # re-assign
                mean[[idx]] <- refe$mean[[idx]]
              
              # re-calculate otherwise
              # need to change from degree to internal units radians: eddy4R.base::def.pol.cart() and 
              # eddy4R.base::def.cart.pol(); inconsistent results when calculating mean wind direction
              # 1) directly from mean horizontal wind vector [m s-1] via single call to eddy4R.base::def.pol.cart() vs.
              # 2) re-calculating from high-frequency wind direction -> unit vector via call to 
              # eddy4R.base::def.pol.cart(eddy4R.base::def.cart.pol()); implemented above look-back as a
              # workaround specifically for wind direction to be consistently defined as the direction that
              # minimizes the mean cross-wind; example:
              # 124.8 deg from 1) wind vector: eddy4R.base::def.pol.cart(cart = base::matrix(c(0.9941063, -1.429056), ncol=2))
              # 123.6 deg from 2) unit vector: eddy4R.base::def.pol.cart(cart = base::matrix(c(0.5394431, -0.8106568), ncol=2))
              } else {
              
                mean[[idx]] <- eddy4R.base::def.unit.conv(data = eddy4R.base::def.pol.cart(
                  cart = base::matrix(
                    base::colMeans(
                      eddy4R.base::def.cart.pol(
                        az = eddy4R.base::def.conv.poly(data = data[[idx]], coefPoly = eddy4R.base::IntlConv$RadDeg)
                        ),
                      na.rm=TRUE),
                    ncol=2)
                  ), unitFrom = "deg", unitTo = "rad")
    
              } 
              
            }; base::rm(idx, tmp03)
            
          }; base::rm(tmp02)
    
          
      # base states; AlgBase <- c("mean", "trnd", "ord03")[1]
          
          # re-assign mean data if AlgBase == "mean"
          if(AlgBase == "mean") {
            
            base <- mean
            
          # compute base state otherwise
          } else {
            
            # calculate base states      
            base <- base::sapply(base::names(inp), function(x) eddy4R.base::def.base.ec(
              idxTime = inp$idep,
              var = inp[[x]],
              AlgBase = AlgBase),
              simplify = FALSE)
            
            # reshape results
            base <- base::as.data.frame(base::matrix(base::unlist(base), ncol = base::ncol(inp)))
            base::attributes(base)$names <- base::attributes(inp)$names
            
            # apply units from inp to base
            base::sapply(base::names(base), function(x) {base::attr(base[[x]], which = "unit") <<- 
              base::attr(inp[[x]], which = "unit")})
            
            # determine whether there are any variables in unit "rad"
            tmp01 <- base::sapply(base::names(base), function(x) {base::attr(base[[x]], which = "unit") == "rad"})
            
            # continue only if there is at least one variable in unit "rad"
            if(length(which(tmp01)) > 0) {
              
              # variables to re-calculate / re-assign
              tmp02 <- base::names(base::which(tmp01))
              
              # assign NAs - current computation of de-trending or 3rd order polynomial not defined for circular quantities
              for(idx in tmp02) {
                
                base[[idx]] <- base::rep(x = NaN, length.out = base::length(base[[idx]]))
                attributes(base[[idx]])$unit <- attributes(inp[[idx]])$unit
                
              }
              base::rm(idx)
              
            }
            # clean up
            base::rm(tmp01, tmp02)
          
          }
    
          
      # instantaneous differences (corresponding to chosen base state treatment)
          
        # calculate
        diff <- sapply(base::names(inp), function(x) inp[[x]] - base[[x]])
        
        # reshape data
        diff <- base::as.data.frame(base::matrix(diff, ncol = base::ncol(inp)))
        base::attributes(diff)$names <- base::attributes(inp)$names
        
        # apply units from inp to diff
        base::sapply(base::names(diff), function(x) {base::attr(diff[[x]], which = "unit") <<- 
          base::attr(inp[[x]], which = "unit")})
    
          
      # standard deviations (corresponding to chosen base state treatment)
        
        # calculate
        sd <- plyr::colwise("sd")(diff, na.rm=TRUE)
        
        # apply units from inp to sd
        base::sapply(base::names(sd), function(x) {base::attr(sd[[x]], which = "unit") <<- 
          base::attr(inp[[x]], which = "unit")})
        
      # create object for export
        
        # create list
        rpt <- base::list()
        
        # populate list
        rpt$min <- min
        rpt$max <- max
        rpt$mean <- mean
        rpt$base <- base
        rpt$diff <- diff
        rpt$sd <- sd
        
        # clean up
        rm(base, diff, max, mean, min, sd)
        
        # return results
        return(rpt)
    
    }
  
  }
  
  # actual calculation
  
  # function call
  statStaDiff <- def.stat.sta.diff(
    inp = data,
    refe = base::list("mean" = base::list("angZaxsErth" = rot$angZaxsErth)),
    AlgBase = AlgBase
    )

  
  
  ############################################################
  # MOMENTUM FLUX AND FRICTION VELOCITY
  ############################################################
  

  # definition function (to be exported)
  {
    ##############################################################################################
    #' @title Definition function: Eddy-covariance turbulent flux calculation for vector quantities
    
    #' @author
    #' Stefan Metzger \email{eddy4R.info@gmail.com}
    
    #' @description Function definition. This function calculates eddy-covariance turbulent flux for vector quantities, such as the wind vector -> momentum flux and friction velocity.
    
    #' @param inp A data frame containing the instantaneous differences produced by ?def.stat.sta.diff() of 1) the wind vector in meteorological ENU convention with the variables veloXaxs (latitudinal wind speed, positive from west), veloYaxs (longitudinal wind speed, positive from south), and veloZaxs (vertical wind speed, positive from below), and 2) the wind vector in streamwise ENU convention with the variables veloXaxsHor (streamwise wind speed, positive from front), veloYaxsHor (cross-wind speed, positive from left), and veloZaxsHor (vertical wind speed, positive from below) derived from ?def.rot.ang.zaxs.erth, of class "numeric", each with unit attribute [m s-1]. The wind vector inputs can be viewed as a specific example that can be generalized through replacement by other vector quantities that share the same coordinate conventions and consistent units among inp and Unit.
    #' @param rot A list of rotation matrices with the list elements mtrxRot01 and mtrxRot02 derived from ?def.rot.ang.zaxs.erth, class "numeric", each with unit attribute. [-]
    #' @param Unit A data frame with the entries In (input units), Out (output units), and OutSq (squared output units), of class "character".
    
    #' @return 
    #' The returned object is a list containing the element dataframes corr, diff, mean, and sd, each of class "numeric" and with unit attribute.
    #' The elements corr, mean and sd are all calculated from the same stress tensor based on the inp (veloXaxs, veloYaxs, veloZaxs) and rot (mtrxRot01 and mtrxRot02) arguments. The element corr contains the horizontal-vertical correlations, the element mean contains the horizontal-vertical covariances, and the element sd contains the standard deviation for each wind vector component in streamwise ENU convention, with a single observation each.
    #' The element diff contains the instantaneous horizontal-vertical products of inp (veloXaxsHor, veloYaxsHor, veloZaxsHor) with the same number of observations as inp.
    
    #' @references
    #' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
    
    #' @keywords correlation, flux, friction velocity, shear stress, standard deviation, vector
    
    #' @examples
    #' Make sure to assign all variables and units, the function should run ok.
    #' inp <- base::data.frame(
    #'   veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
    #'   veloYaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
    #'   veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
    #' )
    #' attr(inp$veloXaxs,"unit") <- "m s-1"; attr(inp$veloYaxs,"unit") <- "m s-1"; attr(inp$veloZaxs,"unit") <- "m s-1"
    #' out <- def.flux.vect(
    #'   inp = base::cbind(def.stat.sta.diff(inp = inp)$diff,
    #'                     def.stat.sta.diff(inp = def.rot.ang.zaxs.erth(inp = inp)$data)$diff),
    #'   rot = def.rot.ang.zaxs.erth(inp = inp)$rot,
    #'   Unit = base::data.frame(In = "m s-1", Out = "m s-1", OutSq = "m2 s-2")
    #' )
    #' utils::str(out)
    #' base::rm(inp, out)
    
    #' @seealso Currently none.
    
    #' @export
    
    # changelog and author contributions / copyrights
    #   Stefan Metzger (2011-03-04)
    #     original creation
    #   Stefan Metzger (2022-02-08)
    #     update to eddy4R terminology and modularize into definition function
    ###############################################################################################
    
    # Eddy-covariance flux calculation for vector quantities
    def.flux.vect <- function(
      
      # input dataframe with variables veloXaxs, veloYaxs, and veloZaxs of class "numeric, each with unit attribute. [m s-1]
      # limit to required wind components, use def.rot.ang.zaxs.erth example above
      inp,
      rot,
      Unit = base::data.frame(In = "m s-1", Out = "m s-1", OutSq = "m2 s-2")
    
    ) {

      # check presence of input arguments and consistent units
      
        # inp
        
          # check that input is of class data.frame
          if(base::class(inp) != "data.frame") {
            stop(base::paste0("def.flux.vect(): inp is not of class data.frame, please check."))  
          }
          
          # test input variables and unit attributes
          for(idx in c("veloXaxs", "veloYaxs", "veloZaxs", "veloXaxsHor", "veloYaxsHor", "veloZaxsHor")){
            # idx <- "veloXaxs"
            
            # test for presence/absence of variables
            if(!(idx %in% base::names(inp))) {
              stop(base::paste0("def.flux.vect(): inp$", idx, " is missing."))}
            
            # test for presence/absence of unit attribute
            if(!("unit" %in% base::names(attributes(inp[[idx]])))) {
              stop(base::paste0("def.flux.vect(): inp$", idx, " is missing unit attribute."))}
            
            # test for correct units
            if(attributes(inp[[idx]])$unit != Unit$In) {
              stop(base::paste0("def.flux.vect(): inp$", idx, 
                                " input units are not matching Unit$In, please check."))}
            
          }; base::rm(idx)
        
        # rot
          
          # check that rot is of class list
          if(base::class(rot) != "list") {
            stop(base::paste0("def.flux.vect(): rot is not of class list, please check."))  
          }
          
          # test rot list entries and unit attributes
          for(idx in c("mtrxRot01", "mtrxRot02")){
            # idx <- "mtrxRot01"
            
            # test for presence/absence of list entries
            if(!(idx %in% base::names(rot))) {
              stop(base::paste0("def.flux.vect(): rot$", idx, " is missing."))}
  
            # test for presence/absence of unit attribute
            if(!("unit" %in% base::names(attributes(rot[[idx]])))) {
              stop(base::paste0("def.flux.vect(): rot$", idx, " is missing unit attribute."))}
            
            # test for correct units
            if(attributes(rot[[idx]])$unit != "-") {
              stop(base::paste0("def.flux.vect(): rot$", idx, 
                                " input units are not matching internal units, please check."))}
            
          }; base::rm(idx)
      
        # Unit
        
          # check that Unit is of class data.frame
          if(base::class(Unit) != "data.frame") {
            stop(base::paste0("def.flux.vect(): Unit is not of class data.frame, please check."))  
          }
        
          # test that input and output Unit are identical
          if(!(Unit$In == Unit$Out)) {
            stop(base::paste0("def.flux.vect(): Unit$Out differs from Unit$In, please check"))}
  
      
      # instantaneous fluxes from instantaneous wind component differences in streamline coordinates
      # for downstream calculation of integral length scales and statistical errors
      # includes negative sign prefixes commonly used in ENU meteorological convention
      # identical to stress tensor results: veloFric <- (mean(diff$veloFricXaxsSq)^2 + mean(diff$veloFricYaxsSq)^2)^(1/4)
      
        # calculate
        diff <- base::data.frame(
          veloFricXaxsSq = -(inp$veloXaxsHor * inp$veloZaxsHor),
          veloFricYaxsSq = -(inp$veloYaxsHor * inp$veloZaxsHor),
          veloFric = base::rep(x = NaN, length.out = base::nrow(inp))
        )
        
        # assign units
        base::attr(diff$veloFricXaxsSq, which = "unit") <- Unit$OutSq
        base::attr(diff$veloFricYaxsSq, which = "unit") <- Unit$OutSq
        base::attr(diff$veloFric, which = "unit") <- Unit$Out
      
      # calculate stress tensor and rotate into streamline coordinates
    
        # transpose wind component instantaneous differences from ENU meteorological convention to NED geographic convention
        # this also means that negative-sign prefixes are omitted from veloFric calculations based on stress tensor
        veloXaxsIntl <- inp$veloYaxs
        veloYaxsIntl <- inp$veloXaxs
        veloZaxsIntl <- -inp$veloZaxs
        
        # stress tensor (defined in NED geographic convention)
        mtrxFric <- rbind(
          c(base::mean(veloXaxsIntl * veloXaxsIntl, na.rm = TRUE),
            base::mean(veloXaxsIntl * veloYaxsIntl, na.rm = TRUE),
            base::mean(veloXaxsIntl * veloZaxsIntl, na.rm = TRUE)),
          c(base::mean(veloYaxsIntl * veloXaxsIntl, na.rm = TRUE),
            base::mean(veloYaxsIntl * veloYaxsIntl, na.rm = TRUE),
            base::mean(veloYaxsIntl * veloZaxsIntl, na.rm = TRUE)),
          c(base::mean(veloZaxsIntl * veloXaxsIntl, na.rm = TRUE),
            base::mean(veloZaxsIntl * veloYaxsIntl, na.rm = TRUE),
            base::mean(veloZaxsIntl * veloZaxsIntl, na.rm = TRUE))
        )
        base::attr(mtrxFric, which = "unit") <- Unit$OutSq
        
        # rotate stress tensor into streamline coordinates
        mtrxRot03 <- rot$mtrxRot01 %*% mtrxFric
        base::attr(mtrxRot03, which = "unit") <- Unit$OutSq
        
        mtrxRot04 <- mtrxRot03 %*% rot$mtrxRot02
        base::attr(mtrxRot04, which = "unit") <- Unit$OutSq
        
        # clean up
        base::rm(mtrxFric, mtrxRot03, veloXaxsIntl, veloYaxsIntl, veloZaxsIntl)
      
      
      # friction velocity [m s-1]
      # optionally only considers the along wind stress veloFricXaxsSq; Foken (2008) Eq.(2.23)
      # negative sign prefixes commonly used for ENU Zaxs are omitted because stress tensor is already defined in NED
        
        # calculate
        mean <- base::data.frame(
          veloFricXaxsSq = mtrxRot04[1,3],
          veloFricYaxsSq = mtrxRot04[2,3])
        mean$veloFric <- (mean$veloFricXaxsSq^2 + mean$veloFricYaxsSq^2)^(1/4)
        
        # assign units
        base::attr(mean$veloFricXaxsSq, which = "unit") <- Unit$OutSq
        base::attr(mean$veloFricYaxsSq, which = "unit") <- Unit$OutSq
        base::attr(mean$veloFric, which = "unit") <- Unit$Out
    
    
      # standard deviation of wind components; deviations from sd calculated in def.stat.sta.diff are < 2%
        
        # calculate
        sd <- base::data.frame(
          veloXaxsHor = base::sqrt(base::abs(base::diag(mtrxRot04)))[1],
          veloYaxsHor = base::sqrt(base::abs(base::diag(mtrxRot04)))[2],
          veloZaxsHor = base::sqrt(base::abs(base::diag(mtrxRot04)))[3])
        
        # assign units
        base::attr(sd$veloXaxsHor, which = "unit") <- Unit$Out
        base::attr(sd$veloYaxsHor, which = "unit") <- Unit$Out
        base::attr(sd$veloZaxsHor, which = "unit") <- Unit$Out
    
    
      # calculate correlations
      # negative sign prefixes commonly used for ENU Zaxs are omitted because stress tensor is already defined in NED
        
        # calculate
        corr <- base::data.frame(
          veloFricXaxsSq = mtrxRot04[1,3] / sd$veloXaxsHor / sd$veloZaxsHor,
          veloFricYaxsSq = mtrxRot04[2,3] / sd$veloYaxsHor / sd$veloZaxsHor,
          veloFric = NaN)
        
        # assign units
        base::sapply(base::names(corr), function(x) {base::attr(corr[[x]], which = "unit") <<- "-"})
      
      
        
      # create object for export
        
        # create list
        rpt <- base::list()
        
        # populate list
        rpt$corr <- corr
        rpt$diff <- diff
        rpt$mean <- mean
        rpt$sd <- sd
        
        # clean up
        base::rm(corr, diff, mean, mtrxRot04, sd)
        
        
      # return results
      return(rpt)
  
    
    }

  }
  
  # actual calculation
  
  # function call
  fluxVect <- def.flux.vect(
    inp = statStaDiff$diff[c("veloXaxs", "veloYaxs", "veloZaxs", "veloXaxsHor", "veloYaxsHor", "veloZaxsHor")],
    rot = rot,
    Unit = base::data.frame(In = "m s-1", Out = "m s-1", OutSq = "m2 s-2")
  )
  

  
  
  ############################################################
  #SENSIBLE HEAT FLUX
  ############################################################

  
  # definition function (to be exported)
  {
    ##############################################################################################
    #' @title Definition function: Eddy-covariance turbulent flux calculation for scalar quantities
    
    #' @author
    #' Stefan Metzger \email{eddy4R.info@gmail.com}
    
    #' @description Function definition. This function calculates eddy-covariance turbulent flux for scalar quantities, such as temperature, moisture, CO2, CH4, NOx, VOCs etc.
    
    #' @param inp A data frame with the variables vect and sclr that each contain the instantaneous differences reported by ?def.stat.sta.diff. In a typical eddy-covariance application, vect would be the vertical wind speed in streamwise ENU convention (positive from below), e.g.  veloZaxs derived from ?def.rot.ang.zaxs.erth and further processed in ?def.stat.sta.diff, of class "numeric" and with unit attribute [m s-1]. scal would be any scalar quantity in SI base units that does not require WPL density correction (Webb et al., 1980), i.e. temperature in unit [K] and gas concentration in dry mole fraction [mol m-3], of class "numeric" and with unit attribute. These inputs can be viewed as a specific example that can be generalized through replacement by other variables that share the same coordinate conventions and consistent units among inp and Unit.
    #' @param conv An optional vector of class "numeric" with unit attribute to permit conversion of the results, e.g. to output units that are different from the product of the inp$vect unit and the inp$sclr unit. conv must be either of length = 1 or have the same length as number of observations in inp. If conv is of length = 1, then the same conversion factor is applied to all observations supplied in inp (e.g., unit conversion). On the other hand, if conv is of the same length as number of observations in inp, then a point-by-point conversion is performed individually for each observation supplied in inp (e.g., different weights for each observation).
    #' @param Unit A data frame with the entries InpVect, InpSclr, Conv, Out, of class "character". To ensure consistent units of the returned object, Unit needs to be specified with the constraint that Unit$Out = Unit$InpVect * Unit$InpSclr * Unit$Conv. If the function call argument conv is not specified, then Unit$Conv should be supplied as = "-".
    #' @param AlgBase A vector of length 1 that defines the base state with respect to which the element-dataframe base in the returned object is calculated, of class "character" and no unit attribute. Is set to one of AlgBase <- c("mean", "trnd", "ord03")[1] and defaults to "mean", with the additional options detrending "trnd" and 3rd-order polynomial "ord03". See ?eddy4R.base::def.base.ec() for additional details.
    #' @param idep An optional vector of class "numeric" with unit attribute. idep is only required to be specified if argument AlgBase is set to "trnd" or "ord03", in which case idep provides the independent variable for interpolation.
    
    #' @return 
    #' The returned object is a list containing the element vectors base, conv, corr, diff, max, mean, min, sd, each of class "numeric" and with unit attribute.
    #' All elements with the exception of conv and corr are calculated from the instantaneous products inp$vect * inp$scal * conv with the same number of observations as inp and assigned the Unit$Out unit attribute. diff provides these instantaneous products themselves, base provides their base state as specified per argument AlgBase, and max, mean, min and sd their maximum, mean, minimum and standard deviation, respectively. The element conv reports the conversion vector with unit attribute if specified in the function arguments, or NULL with unit attribute "-" otherwise. The element corr is calculated from inp$vect and inp$scal without invoking conv, and provides the correlation between vector and scalar quantity with unit attribute "-".

    
    #' @references
    #' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
    #' Webb, E. K., Pearman, G. I., and Leuning, R.: Correction of flux measurements for density effects due to heat and water vapour transfer, Q. J. R. Meteorolog. Soc., 106, 85-100, doi:10.1002/qj.49710644707, 1980.
    
    #' @keywords correlation, flux, temperature, moisture, water, humidity, H2O, CO2, CH4, NOx, VOC, standard deviation, scalar
    
    #' @examples
    #' Sensible heat flux in units of energy [kg s-3] = [W m-2]
    #' make sure to assign all variables and units, the function should run ok.
    #' input data: vertical wind speed and temperature instantaneous differences from base state, see ?def.stat.sta.diff for details
    #' inp <- base::data.frame(
    #'   vect = c(0.2259224, 0.2342562, 0.3936403, 0.2395388, 0.2885017),
    #'   sclr = c(0.1067013, 0.1015043, 0.1324425, 0.1732023, 0.1262345)
    #' )
    #' attr(inp$vect,"unit") <- "m s-1"; attr(inp$sclr,"unit") <- "K"
    #' volumetric heat capacity for conversion from kinematic units [K m s-1] to units of energy [W m-2], see ?def.heat.air.wet for details
    #' conv <- 1220.079
    #' attr(conv,"unit") <- "kg m-1 s2 K-1"
    #' function call
    #' out <- def.flux.sclr(
    #'   inp = inp,
    #'   conv = conv,
    #'   Unit = base::data.frame(InpVect = "m s-1", InpSclr = "K", Conv = "kg m-1 s2 K-1", Out = "W m-2")
    #' )
    #' utils::str(out)
    #' base::rm(inp, conv, out)

    #' @seealso Currently none.
    
    #' @export
    
    # changelog and author contributions / copyrights
    #   Stefan Metzger (2011-03-04)
    #     original creation
    #   Stefan Metzger (2022-03-24)
    #     update to eddy4R terminology and modularize into definition function
    ###############################################################################################

    # Eddy-covariance flux calculation for scalar quantities
    def.flux.sclr <- function(
      inp,
      conv = NULL,
      Unit,
      AlgBase = c("mean", "trnd", "ord03")[1],
      idep = NULL
    ) {
        
      # check presence of input arguments, consistent lengths and units
          
        # conv check 1 of 2
          
          # if conv defaults to NULL, convert to 1 and assign unit attribute. Also specify Unit$Conv if not already present (failsafe)
          if(is.null(conv)) {
            conv <- 1
            base::attr(conv, which = "unit") <- "-"
            if(!("Conv" %in% base::names(Unit))) Unit$Conv <- "-"
          }
          
          # check that conv is either of length 1 or of the same length as inp
          if(!(base::length(conv) %in% c(1, base::nrow(inp)))) {
            stop(base::paste0("def.flux.sclr(): conv needs to be either of length 1 or have the same number of observations as inp, please check."))}  
      
        # Unit
        
          # check that Unit is of class data.frame
          if(base::class(Unit) != "data.frame") {
            stop(base::paste0("def.flux.sclr(): Unit is not of class data.frame, please check."))  
          }

          # test unit variables
          for(idx in c("InpVect", "InpSclr", "Conv", "Out")){
            # idx <- "InpVect"
            
            # test for presence/absence of variables
            if(!(idx %in% base::names(Unit))) {
              stop(base::paste0("def.flux.sclr(): Unit$", idx, " is missing."))}
            
            # test for character type
            if(base::typeof(Unit[[idx]]) != "character") {
              stop(base::paste0("def.flux.sclr(): Unit$", idx, 
                                " is not of type character, please check."))}
            
          }; base::rm(idx)
      
        # inp
      
          # check that input is of class data.frame
          if(base::class(inp) != "data.frame") {
            stop(base::paste0("def.flux.sclr(): inp is not of class data.frame, please check."))  
          }
          
          # test for presence/absence of input variables and unit attributes
          for(idx in c("vect", "sclr")){
            # idx <- "vect"
            
            # test for presence/absence of variables
            if(!(idx %in% base::names(inp))) {
              stop(base::paste0("def.flux.sclr(): inp$", idx, " is missing."))}
            
            # test for presence/absence of unit attribute
            if(!("unit" %in% base::names(attributes(inp[[idx]])))) {
              stop(base::paste0("def.flux.sclr(): inp$", idx, " is missing unit attribute."))}
            
          }; base::rm(idx)
      
          # test for consistent units
            
            # inp$vect
            if(attributes(inp$vect)$unit != Unit$InpVect) {
              stop(base::paste0("def.flux.sclr(): inp$vect unit attribute does not match Unit$InpVect, please check."))}
      
            # inp$sclr
            if(attributes(inp$sclr)$unit != Unit$InpSclr) {
              stop(base::paste0("def.flux.sclr(): inp$sclr unit attribute does not match Unit$InpSclr, please check."))}
      
        # conv check 2 of 2
    
          # test for consistent units - do this after the Unit dataframe has been tested
          if(attributes(conv)$unit != Unit$Conv) {
            stop(base::paste0("def.flux.sclr(): conv unit attribute does not match Unit$Conv, please check."))}
    
        # AlgBase and idep
        if(AlgBase != "mean" & is.null(idep)) {
          stop(base::paste0("def.flux.sclr(): please specify function argument idep when AlgBase != 'mean'."))}

      
      # convert conv to the same number of observations as inp
      if(base::length(conv) == 1){
        convTmp <- base::rep(x = conv, length.out = base::nrow(inp))
        base::attr(convTmp, which = "unit") <- base::attr(conv, which = "unit")
        conv <- convTmp
        base::rm(convTmp)
      }
      
      
      # perform calculations
        
        # instantaneous fluxes from instantaneous vector and scalar differences in input (typically kinematic) units
      
          # calculation
          diff <- inp$vect * inp$sclr * conv

          # assign units
          base::attr(diff, which = "unit") <- Unit$Out
        
        # descriptive statistics incl. mean fluxes
        
          # calculation
          min <- base::min(diff, na.rm = TRUE)
          max <- base::max(diff, na.rm = TRUE)
          mean <- base::mean(diff, na.rm = TRUE)
          sd <- stats::sd(diff, na.rm = TRUE)
          if(AlgBase == "mean") base <- mean else base <- eddy4R.base::def.base.ec(
            idxTime = idep, var = diff, AlgBase = AlgBase)
  
          # assign units
          base::attr(base, which = "unit") <- Unit$Out
          base::attr(min, which = "unit") <- Unit$Out
          base::attr(max, which = "unit") <- Unit$Out
          base::attr(mean, which = "unit") <- Unit$Out
          base::attr(sd, which = "unit") <- Unit$Out
        
        # correlation
          
          # calculation
          corr <- stats::cor(inp$vect, inp$sclr, use = "pairwise.complete.obs")
          
          # assign units
          base::attr(corr, which = "unit") <- "-"
      
          
      # create object for export
        
        # create list
        rpt <- base::list()
        
        # populate list
        rpt$base <- base
        rpt$conv <- conv
        rpt$corr <- corr
        rpt$diff <- diff
        rpt$max <- max
        rpt$mean <- mean
        rpt$min <- min
        rpt$sd <- sd
        
        # clean up
        base::rm(base, conv, corr, diff, max, mean, min, sd)
        
        
      # return results
      return(rpt)
        
    }
  
  }

  
  # initiate dataframe to store conversion factors and correlations
  statStaDiff$conv <- base::data.frame(fluxTemp = base::rep(NaN, length.out = nrow(statStaDiff$diff)))
  statStaDiff$corr <- base::data.frame(fluxTemp = NaN)

  # SENSIBLE HEAT FLUX, BUOYANCY FLUX
  
    # sensible heat flux in kinematic units [K m s-1]
    fluxTmp <- def.flux.sclr(
      inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = statStaDiff$diff$tempAir),
      conv = NULL,
      Unit = base::data.frame(InpVect = "m s-1", InpSclr = "K", Conv = "-", Out = "K m s-1")
    )
    for(idx in base::names(fluxTmp)) statStaDiff[[idx]]$fluxTemp <- fluxTmp[[idx]]
    base::rm(fluxTmp, idx)
      
    # sensible heat flux in units of energy [kg s-3] = [W m-2]
    fluxTmp <- def.flux.sclr(
      inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = statStaDiff$diff$tempAir),
      conv = statStaDiff$base$heatAirWet,
      Unit = base::data.frame(InpVect = "m s-1", InpSclr = "K", Conv = "kg m-1 s2 K-1", Out = "W m-2")
    )
    for(idx in base::names(fluxTmp)) statStaDiff[[idx]]$fluxTempEngy <- fluxTmp[[idx]]
    base::rm(fluxTmp, idx)
    
    # buoyancy flux in kinematic units [K m s-1]
    # considering water vapor buoyancy and NIST standard pressure (1013.15 hPa) reference (virt. pot. temp.) -> z/L
    fluxTmp <- def.flux.sclr(
      inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = statStaDiff$diff$tempVirtPot00),
      conv = NULL,
      Unit = base::data.frame(InpVect = "m s-1", InpSclr = "K", Conv = "-", Out = "K m s-1")
    )
    for(idx in base::names(fluxTmp)) statStaDiff[[idx]]$fluxTempVirtPot00 <- fluxTmp[[idx]]
    base::rm(fluxTmp, idx)

  # LATENT HEAT FLUX, EVAPOTRANSPIRATION
    
    # latent heat flux in kinematic units [mol m-2 s-1]
    fluxTmp <- def.flux.sclr(
      inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = statStaDiff$diff$rtioMoleDryH2o),
      conv = statStaDiff$base$densMoleAirDry,
      Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molH2o mol-1Dry", Conv = "mol m-3", Out = "mol m-2 s-1")
    )
    for(idx in base::names(fluxTmp)) statStaDiff[[idx]]$fluxH2o <- fluxTmp[[idx]]
    base::rm(fluxTmp, idx)
    
    # latent heat flux in units of energy [kg s-3] = [W m-2]
    
      # define conversion from kinematic units to units of energy
      # dry air density [mol m-3] x latent heat of vaporization [J kg-1] x molar mass [kg mol-1] = [J m-3] = [kg m-1 s-1]
      conv <- statStaDiff$base$densMoleAirDry * statStaDiff$base$heatH2oGas * eddy4R.base::IntlNatu$MolmH2o
      base::attr(conv, which = "unit") <- "kg m-1 s-1"
      
      # actual flux calculation
      fluxTmp <- def.flux.sclr(
        inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = statStaDiff$diff$rtioMoleDryH2o),
        conv = conv,
        Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molH2o mol-1Dry", Conv = "kg m-1 s-1", Out = "W m-2")
      )
      
      # transfer results
      for(idx in base::names(fluxTmp)) statStaDiff[[idx]]$fluxH2oEngy <- fluxTmp[[idx]]
      base::rm(conv, fluxTmp, idx)
    
    # evapotranspiration depth [m s-1]
    # resources: https://www.fao.org/3/X0490E/x0490e04.htm
    # https://github.com/stefanmet/NEON-FIU-algorithm-stefanmet/commit/86c368cfe367aac6f5c90aa8704ed0caf6558e4b
    # https://chemistry.stackexchange.com/questions/23643/calculating-the-volume-of-1-mole-of-liquid-water
      
      # define conversion from kinematic units to units of evapotranspiration depth
      # dry air mole density [mol m-3] x (H2O molar mass [kg mol-1] / liquid H2O mass density at 4C and 1 Atm [kg m-3]) = [-]
      conv <- statStaDiff$base$densMoleAirDry * IntlNatu$MolmH2o / 1e3
      base::attr(conv, which = "unit") <- "-"
      
      # actual flux calculation
      fluxTmp <- def.flux.sclr(
        inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = statStaDiff$diff$rtioMoleDryH2o),
        conv = conv,
        Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molH2o mol-1Dry", Conv = "-", Out = "m s-1")
      )

      # transfer results
      for(idx in base::names(fluxTmp)) statStaDiff[[idx]]$fluxH2oVelo <- fluxTmp[[idx]]
      base::rm(conv, fluxTmp, idx)
    
  # OTHER SCALAR FLUXES INCL. CO2, CH4, NOx, VOCs ETC.
    
    # CO2 flux in kinematic units [mol m-2 s-1]
    fluxTmp <- def.flux.sclr(
      inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = statStaDiff$diff$rtioMoleDryCo2),
      conv = statStaDiff$base$densMoleAirDry,
      Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molCo2 mol-1Dry", Conv = "mol m-3", Out = "mol m-2 s-1")
    )
    for(idx in base::names(fluxTmp)) statStaDiff[[idx]]$fluxCo2 <- fluxTmp[[idx]]
    base::rm(fluxTmp, idx)
    

    
  ############################################################
  # BOUNDARY LAYER VARIABLES
  ############################################################

    
  # definition function (to be exported)
  {
    
    ##############################################################################################
    #' @title Definition function: Boundary layer variables
    
    #' @author
    #' Stefan Metzger \email{eddy4R.info@gmail.com}
    
    #' @description Function definition. This function calculates turbulence intensity, Obukhov length, atmospheric stability, convective velocity, convective timescale, temperature scale, and humidity scale.
    
    #' @param inp A data frame with the variables vect and sclr that each contain the instantaneous differences reported by ?def.stat.sta.diff. In a typical eddy-covariance application, vect would be the vertical wind speed in streamwise ENU convention (positive from below), e.g.  veloZaxs derived from ?def.rot.ang.zaxs.erth and further processed in ?def.stat.sta.diff, of class "numeric" and with unit attribute [m s-1]. scal would be any scalar quantity in SI base units that does not require WPL density correction (Webb et al., 1980), i.e. temperature in unit [K] and gas concentration in dry mole fraction [mol m-3], of class "numeric" and with unit attribute. These inputs can be viewed as a specific example that can be generalized through replacement by other variables that share the same coordinate conventions and consistent units among inp and Unit.
    #' @param conv An optional vector of class "numeric" with unit attribute to permit conversion of the results, e.g. to output units that are different from the product of the inp$vect unit and the inp$sclr unit. conv must be either of length = 1 or have the same length as number of observations in inp. If conv is of length = 1, then the same conversion factor is applied to all observations supplied in inp (e.g., unit conversion). On the other hand, if conv is of the same length as number of observations in inp, then a point-by-point conversion is performed individually for each observation supplied in inp (e.g., different weights for each observation).
    #' @param Unit A data frame with the entries InpVect, InpSclr, Conv, Out, of class "character". To ensure consistent units of the returned object, Unit needs to be specified with the constraint that Unit$Out = Unit$InpVect * Unit$InpSclr * Unit$Conv. If the function call argument conv is not specified, then Unit$Conv should be supplied as = "-".
    #' @param AlgBase A vector of length 1 that defines the base state with respect to which the element-dataframe base in the returned object is calculated, of class "character" and no unit attribute. Is set to one of AlgBase <- c("mean", "trnd", "ord03")[1] and defaults to "mean", with the additional options detrending "trnd" and 3rd-order polynomial "ord03". See ?eddy4R.base::def.base.ec() for additional details.
    #' @param idep An optional vector of class "numeric" with unit attribute. idep is only required to be specified if argument AlgBase is set to "trnd" or "ord03", in which case idep provides the independent variable for interpolation.
    
    #' @return 
    #' The returned object is a list containing the element vectors base, conv, corr, diff, max, mean, min, sd, each of class "numeric" and with unit attribute.
    #' All elements with the exception of conv and corr are calculated from the instantaneous products inp$vect * inp$scal * conv with the same number of observations as inp and assigned the Unit$Out unit attribute. diff provides these instantaneous products themselves, base provides their base state as specified per argument AlgBase, and max, mean, min and sd their maximum, mean, minimum and standard deviation, respectively. The element conv reports the conversion vector with unit attribute if specified in the function arguments, or NULL with unit attribute "-" otherwise. The element corr is calculated from inp$vect and inp$scal without invoking conv, and provides the correlation between vector and scalar quantity with unit attribute "-".
    
    
    #' @references
    #' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
    #' Stull, R. B.: An Introduction to Boundary Layer Meteorology, Kluwer Academic Publishers, Dordrecht, The Netherlands, 670 pp., 1988.
    #' Foken, T.: Micrometeorology, Springer, Berlin, Heidelberg, 306 pp., 2008.
    
    #' @keywords turbulence intensity, coefficient of variation, Obukhov length, atmospheric stability, convective velocity, convective timescale, temperature scale, humidity scale
    
    #' @examples
    #' Sensible heat flux in units of energy [kg s-3] = [W m-2]
    #' make sure to assign all variables and units, the function should run ok.
    #' input data: vertical wind speed and temperature instantaneous differences from base state, see ?def.stat.sta.diff for details
    #' inp <- base::data.frame(
    #'   vect = c(0.2259224, 0.2342562, 0.3936403, 0.2395388, 0.2885017),
    #'   sclr = c(0.1067013, 0.1015043, 0.1324425, 0.1732023, 0.1262345)
    #' )
    #' attr(inp$vect,"unit") <- "m s-1"; attr(inp$sclr,"unit") <- "K"
    #' volumetric heat capacity for conversion from kinematic units [K m s-1] to units of energy [W m-2], see ?def.heat.air.wet for details
    #' conv <- 1220.079
    #' attr(conv,"unit") <- "kg m-1 s2 K-1"
    #' function call
    #' out <- def.flux.sclr(
    #'   inp = inp,
    #'   conv = conv,
    #'   Unit = base::data.frame(InpVect = "m s-1", InpSclr = "K", Conv = "kg m-1 s2 K-1", Out = "W m-2")
    #' )
    #' utils::str(out)
    #' base::rm(inp, conv, out)
    
    #' @seealso Currently none.
    
    #' @export
    
    # changelog and author contributions / copyrights
    #   Stefan Metzger (2011-03-04)
    #     original creation
    #   Stefan Metzger (2022-12-23)
    #     update to eddy4R terminology and modularize into definition function
    ###############################################################################################
    
    
    def.var.abl <- function(
      velo = NULL,
      distZaxsMeas = NULL,
      distZaxsAbl = NULL,
      densMoleAirDry = NULL,
      tempVirtPot00 = NULL,
      veloFric = NULL,
      fluxTemp = NULL,
      fluxTempVirtPot00 = NULL,
      fluxH2o = NULL
    ) {
      
      # check presence of input arguments and consistent units
      
        # list of expected units for function arguments
        UnitExpc <- base::list()
        UnitExpc$velo <- "m s-1"
        UnitExpc$distZaxsMeas <- "m"
        UnitExpc$distZaxsAbl <- "m"
        UnitExpc$densMoleAirDry <- "mol m-3"
        UnitExpc$tempVirtPot00 <- "K"
        UnitExpc$veloFric <- "m s-1"
        UnitExpc$fluxTemp <- "K m s-1"
        UnitExpc$fluxTempVirtPot00 <- "K m s-1"
        UnitExpc$fluxH2o <- "mol m-2 s-1"
      
        # velo - separate checks because the only dataframe among the function arguments
      
          # presence or absence of object
          if(is.null(velo)) {
            velo <- base::data.frame(Xaxs = NaN, Yaxs = NaN, Zaxs = NaN)
            for(idx in c("Xaxs", "Yaxs", "Zaxs")) base::attr(velo[[idx]], which = "unit") <- UnitExpc$velo
            base::rm(idx)
          }
      
          # check that velo is of class data.frame
          if(base::class(velo) != "data.frame") {
            stop(base::paste0("def.var.abl(): velo is not of class data.frame, please check."))  
          }
          
          # presence or absence of object elements, unit attributes and units
          for(idx in c("Xaxs", "Yaxs", "Zaxs")) {
          # idx <- "Xaxs"
          
            # object elements  
            if(base::is.null(velo[[idx]])) {
              stop(base::paste0("def.var.abl(): velo$", idx, " is not provided, please check."))  
            }
            
            # presence/absence of unit attribute
            if(!("unit" %in% base::names(attributes(velo[[idx]])))) {
              stop(base::paste0("def.var.abl(): velo$", idx, " is missing unit attribute."))}

            # correct units of object elements            
            if(attributes(velo[[idx]])$unit != UnitExpc$velo) {
              stop(base::paste0("def.var.abl(): velo$", idx, " unit attribute is different from ",  base::dQuote(UnitExpc$velo), ", please check."))}
            
          }; base::rm(idx)
      
        # loop around all function arguments other than velo
        
          # add namespace to base::formals() call once compiled into eddy4R.turb
          for(idx in base::names(base::formals(fun = def.var.abl))[-which(base::names(base::formals(fun = def.var.abl)) == "velo")]) {
          # idx <- base::names(base::formals(fun = def.var.abl))[-which(base::names(base::formals(fun = def.var.abl)) == "velo")][3]
            
            # work with temporary variable, so that it is possible to assign attributes
            tmp <- base::get(idx)
            
            # presence or absence of object; re-assign to workspace object once done
            if(is.null(tmp)) {
              tmp <- NaN
              base::attr(tmp, which = "unit") <- UnitExpc[[idx]]
              base::assign(x = idx, value = tmp)
            }
            
            # check that object is of class numeric vector
            if(base::class(tmp) != "numeric") {
              stop(base::paste0("def.var.abl(): ", idx, " is not of class numeric vector, please check."))}
            
            # presence/absence of unit attribute
            if(!("unit" %in% base::names(attributes(tmp)))) {
              stop(base::paste0("def.var.abl(): ", idx, " is missing unit attribute."))}
            
            # correct units
            if(attributes(tmp)$unit != UnitExpc[[idx]]) {
              stop(base::paste0("def.var.abl(): ", idx, " unit attribute is different from ",  base::dQuote(UnitExpc[[idx]]), ", please check."))}
          
            # clean up prior to next iteration
            base::rm (tmp)  
            
          }; base::rm(idx)

      
      # create empty object for export
      rpt <- base::data.frame(coefSdMeanVelo = NaN)
        
      # turbulence intensity (aka coefficient of variation) from total wind vector (Stull, Eq. 1.4d)
        
        # total wind vector [m s-1]
        veloXaxsYaxsZaxs <- base::sqrt(velo$Xaxs^2 + velo$Yaxs^2 + velo$Zaxs^2)
        base::attr(veloXaxsYaxsZaxs, which = "unit") <- "m s-1"
        
        # turbulence intensity (aka coefficient of variation) should be <0.5 to allow for Taylor's hypothesis [-]
        rpt$coefSdMeanVelo <- stats::sd(veloXaxsYaxsZaxs, na.rm=TRUE) / base::mean(veloXaxsYaxsZaxs, na.rm=TRUE)
        base::attr(rpt$coefSdMeanVelo, which = "unit") <- "-"
    
      # Obukhov length and atmospheric stability
        
        # Obukhov length (used positive g!) [m]
        rpt$distObkv <- (-(((veloFric)^3 / (eddy4R.base::IntlNatu$VonkFokn * eddy4R.base::IntlNatu$Grav / tempVirtPot00 * fluxTempVirtPot00 ))))
        base::attr(rpt$distObkv, which = "unit") <- "m"
        
        # atmospheric stability [-]
        rpt$paraStbl <- distZaxsMeas / rpt$distObkv
        base::attr(rpt$paraStbl, which = "unit") <- "-"
      
      # convective velocity and timescale
      
        # convective (Deardorff) velocity [m s-1]
        # missing values in Deardorff velocity and resulting variables when buoyancy flux is negative!
        rpt$veloScalCvct <- ( eddy4R.base::IntlNatu$Grav * distZaxsAbl / tempVirtPot00 * fluxTempVirtPot00 )^(1/3)
        base::attr(rpt$veloScalCvct, which = "unit") <- "m s-1"
    
        # (free) convective time scale [s], often on the order of 5-15 min
        rpt$timeScalCvct <- distZaxsAbl / rpt$veloScalCvct
        base::attr(rpt$timeScalCvct, which = "unit") <- "s"
        
      # atmospheric temperature scale (eddy temperature fluctuations) [K]
      # negative sign for SL scales vs. positive sign for ML scales per convention in Stull (1988) p. 356
        
        # surface layer
        # rpt$tempScalAtmSurf <- - fluxTempVirtPot00 / veloFric	# per Stull (1988) p. 356, would be overall more consistent
        rpt$tempScalAtmSurf <- - fluxTemp / veloFric	# per Foken (2008) p.42, fits with / feeds into def.itc()
        base::attr(rpt$tempScalAtmSurf, which = "unit") <- "K"
        
        # mixed layer
        # according to Stull (1988) p. 356
        # pre-2022-12-21: consistent with Foken ITC above but inconsistent with derivation of rpt$veloScalCvct
        # rpt$tempScalAbl <- fluxTemp / rpt$veloScalCvct
        # post-2022-12-21: consistent with derivation of rpt$veloScalCvct based on virtual potential temperature
        rpt$tempScalAbl <- fluxTempVirtPot00 / rpt$veloScalCvct
        base::attr(rpt$tempScalAbl, which = "unit") <- "K"
    
      # atmospheric humidity scale (eddy moisture fluctuations) [mol mol-1 dry air]
        
        # surface layer
        rpt$rtioMoleDryH2oScalAtmSurf <- - base::mean(fluxH2o / densMoleAirDry, na.rm=TRUE) / veloFric
        base::attr(rpt$rtioMoleDryH2oScalAtmSurf, which = "unit") <- "molH2o mol-1Dry"
        
        # mixed layer
        rpt$rtioMoleDryH2oScalAbl <-   base::mean(fluxH2o / densMoleAirDry, na.rm=TRUE) / rpt$veloScalCvct
        base::attr(rpt$rtioMoleDryH2oScalAbl, which = "unit") <- "molH2o mol-1Dry"
    
      # clean up
      base::rm(UnitExpc, veloXaxsYaxsZaxs)
        
      # return results
      return(rpt)
      
    }
  }
    

  # actual function call
    
    # prepare input data
    velo <- data[c("veloXaxs", "veloYaxs", "veloZaxs")]
    base::dimnames(velo)[[2]] <- c("Xaxs", "Yaxs", "Zaxs")

    # call function
    varAblTmp <- def.var.abl(
      velo = velo,
      distZaxsMeas = statStaDiff$mean$d_z_m,
      distZaxsAbl = statStaDiff$mean$d_z_ABL,
      densMoleAirDry = statStaDiff$base$densMoleAirDry,
      tempVirtPot00 = statStaDiff$mean$tempVirtPot00,
      veloFric = fluxVect$mean$veloFric,
      fluxTemp = statStaDiff$mean$fluxTemp,
      fluxTempVirtPot00 = statStaDiff$mean$fluxTempVirtPot00,
      fluxH2o = statStaDiff$mean$fluxH2o
    )
    
    # assign outputs
    for(idx in base::names(varAblTmp)) statStaDiff$mean[[idx]] <- varAblTmp[[idx]]

    # clean up
    base::rm(idx, varAblTmp, velo)
    

    
    
############################################################
#EXPORT RESULTS
############################################################
  
  
  # mapping outputs
  # mn$I <- coefSdMeanVelo # turbulence intensity
  # mn$d_L_v_0 <- distObkv # Obukhov length
  # mn$sigma <- paraStbl # atmospheric stability
  # mn$w_star <- veloScalCvct
  # mn$t_star <- timeScalCvct
  # mn$T_star_SL <- tempScalSurf
  # mn$T_star_ML <- tempScalAbl
  # mn$rtioMoleDryH2o_star_SL <- rtioMoleDryH2oScalAtmSurf
  # mn$rtioMoleDryH2o_star_ML <- rtioMoleDryH2oScalAbl

  
  #convert the sd date to the mn date (as sd of the date range is meaningless)
  sd$date = mn$date
  
  #assemble export list
  export <- list(
    data=data,	#data including internal calculations
    base=base, #base state
    min=mi,		#min
    max=ma,		#max
    mn=mn,		#mean
    sd=sd,		#standard deviation
    diff=diff,	#instantaneous fluctuations
    corr=corr,		#correlation coefficient
    mtrxRot01=mtrxRot01       #transformation matrix for stress tensor
  )
  
  #clean up
  rm(mtrxRot01, base, data, mi, ma, mn, sd, diff, corr)
  
  #return result
  return(export)
}