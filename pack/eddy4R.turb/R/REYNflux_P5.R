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
  AlgBase = c("mean", "trnd", "ord03")[1],
  slctPot = TRUE,
  presPot = NULL,
  PltfEc = "airc",
  flagCh4 = TRUE,
  spcs = NULL,
  rmm = NULL,
  ...
)
{
  
  ### rename input data in preparation of terms update (replace with unit test when completing refactoring)
  
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
      base::attr(x = data$rtioMoleDryH2o, which = "unit") <- "-"
    data$rtioMoleDryCo2 <- data$FD_mole_CH4
      data$FD_mole_CH4 <- NULL
      base::attr(x = data$rtioMoleDryCo2, which = "unit") <- "-"
  
  
  
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
    
    #' @param rtioMoleDryH2o A vector containing the water vapor dry mole fraction, of class "numeric". [-]
    
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
    #' attributes(rtioMoleDryH2o)$unit <- "-"
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
      if(attributes(rtioMoleDryH2o)$unit != "-") {
        
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
  if(slctPot == TRUE) {
    
    # check correct units if potential pressure reference is provided
    if(!is.null(presPot)){
    
      # test for presence of unit attribute
      if(!("unit" %in% names(attributes(presPot)))) {
        stop("presPot is missing unit attribute.")}
      
      # test for correct unit
      if(attributes(presPot)$unit != "Pa") {
        stop("presPot units are not matching internal units, please check.")}
      
    }
    
    # define pressure level for potential quantities
    presPotLoca <- ifelse(!is.null(presPot), presPot, base::mean(data$presAtm, na.rm=TRUE))
    base::attr(x = presPotLoca, which = "unit") <- "Pa"
    
    # potential temperature at defined pressure level
    data$tempAir <- eddy4R.base::def.temp.pres.pois(
      temp01 = data$tempAir,
      pres01 = data$presAtm,
      pres02 = presPotLoca,
      Kppa = KppaWet)
    
    # potential densities at defined pressure level
    
      # dry air
      data$densMoleAirDry <- eddy4R.base::def.dens.pres.pois(
        dens01 = data$densMoleAirDry,
        pres01 = data$presAtm,
        pres02 = presPotLoca,
        Kppa = KppaWet)
      
      # H2O
      data$densMoleH2o <- eddy4R.base::def.dens.pres.pois(
        dens01 = data$densMoleH2o,
        pres01 = data$presAtm,
        pres02 = presPotLoca,
        Kppa=KppaWet)
      
      # wet air
      data$densMoleAir <- eddy4R.base::def.dens.pres.pois(
        dens01 = data$densMoleAir,
        pres01 = data$presAtm,
        pres02 = presPotLoca,
        Kppa = KppaWet)
      
      # clean up
      rm(presPotLoca)
    
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
      for(whr in c("veloXaxs", "veloYaxs", "veloZaxs")){
      # whr <- "veloXaxs"
        
        # test for presence/absence of variables
        if(!(whr %in% base::names(inp))) {
          stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", whr, " is missing."))        }
        
        # test for presence/absence of unit attribute
        if(!("unit" %in% base::names(attributes(inp[[whr]])))) {
          stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", whr, " is missing unit attribute."))        }
        
        # test for correct units
        if(attributes(inp[[whr]])$unit != "m s-1") {
          stop(base::paste0("def.rot.ang.zaxs.erth(): inp$", whr, 
                            " input units are not matching internal units, please check."))}
        
      }
      
      # clean up
      base::rm(whr)
  
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
          # requires mirroring as output is still in geodetic axes order, downstream impact on imfl$u_star2_y
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
  # MEANS, BASE STATES, DEVIATIONS AND SUMMARY STATISTICS
  ############################################################
  
  # prepare inputs
  # reference quantities that overwrite internal calculations
  refe <- list()
  refe$mean$angZaxsErth <- rot$angZaxsErth
  
  
  # always calculate means as reference
  
    # calculate means for euclidean (linear, cartensian) quantities
    mean <- plyr::colwise("mean")(data, na.rm = TRUE)

    # apply units from data to mean
    base::sapply(base::names(mean), function(x) {base::attr(mean[[x]], which = "unit") <<- 
      base::attr(data[[x]], which = "unit")})
    
    # re-calculate / re-assign means for circular (polar) quantities as vector average
    
      # test for correct units
    
        # determine if there are any variables in unit "deg"
        tmp01 <- base::sapply(base::names(mean), function(x) {base::attr(mean[[x]], which = "unit") == "deg"})
        
        # stop and print error message to screen
        if(length(which(tmp01)) > 0) {
          stop(base::paste0("my.function(): ", base::names(base::which(tmp01)), 
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
          
        }
        base::rm(idx)
        
      }
      # clean up
      base::rm(tmp02, tmp03)

      
  # base states; AlgBase <- c("mean", "trnd", "ord03")[2]
      
      # re-assign mean data if AlgBase == "mean"
      if(AlgBase == "mean") {
        
        base <- mean
        
      # compute base state otherwise
      } else {
        
        # calculate base states      
        base <- base::sapply(base::names(data), function(x) eddy4R.base::def.base.ec(
          idxTime = data$idep,
          var = data[[x]],
          AlgBase = AlgBase),
          simplify = FALSE)
        
        # reshape results
        base <- base::as.data.frame(base::matrix(base::unlist(base), ncol = base::ncol(data)))
        base::attributes(base)$names <- base::attributes(data)$names
        
        # apply units from data to base
        base::sapply(base::names(base), function(x) {base::attr(base[[x]], which = "unit") <<- 
          base::attr(data[[x]], which = "unit")})
        
        # determine whether there are any variables in unit "rad"
        tmp01 <- base::sapply(base::names(base), function(x) {base::attr(base[[x]], which = "unit") == "rad"})
        
        # continue only if there is at least one variable in unit "rad"
        if(length(which(tmp01)) > 0) {
          
          # variables to re-calculate / re-assign
          tmp02 <- base::names(base::which(tmp01))
          
          # assign NAs - current computation of de-trending or 3rd order polynomial not defined for circular quantities
          for(idx in tmp02) {
            
            base[[idx]] <- base::rep(x = NaN, length.out = base::length(base[[idx]]))
            attributes(base[[idx]])$unit <- attributes(data[[idx]])$unit
            
          }
          base::rm(idx)
          
        }
        # clean up
        base::rm(tmp01, tmp02)
      
      }
      



  

  #immidiate fluctuations
  imfl <- sapply(1:ncol(data), function(x) data[,x] - base[,x])
  imfl <- as.data.frame(matrix(imfl, ncol=ncol(data)))
  attributes(imfl)$names <- attributes(data)$names
  
  #correct wind direction from (detrended) wind components
  PSI_uv_dum <- eddy4R.base::def.pol.cart(matrix(c(imfl$veloYaxs + mn$veloYaxs, imfl$veloXaxs + mn$veloXaxs), ncol=2))
  imfl$PSI_uv <- (PSI_uv_dum - mn$PSI_uv)
  rm(PSI_uv_dum)
  #same should be done for PSI_aircraft
  
  #variances (corresponding to base state treatment)
  sd <- as.data.frame( matrix(sqrt(splus2R::colVars(imfl, na.rm=TRUE)), ncol=ncol(imfl)) )
  attributes(sd)$names <- attributes(imfl)$names
  
  
  
  
  
  
  
  
  
  
  ############################################################
  #ROTATION OF STRESS TENSOR
  ############################################################
  
  #wind deviations in MET coordinates
  dx <- imfl$veloYaxs
  dy <- imfl$veloXaxs
  dz <- imfl$veloZaxs
  
  #stress tensor
  M <- rbind(
    c(mean(dx * dx, na.rm = TRUE), mean(dx * dy, na.rm = TRUE), mean(dx * dz, na.rm = TRUE)),
    c(mean(dy * dx, na.rm = TRUE), mean(dy * dy, na.rm = TRUE), mean(dy * dz, na.rm = TRUE)),
    c(mean(dz * dx, na.rm = TRUE), mean(dz * dy, na.rm = TRUE), mean(dz * dz, na.rm = TRUE))
  )
  
  #rotation into mean wind coordinate system
  Mrot1 <- mtrxRot01 %*% M
  Mrot2 <- Mrot1 %*% mtrxRot02
  
  #clean up
  rm(mtrxRot02, dx, dy, dz, M, Mrot1)
  
  
  
  ############################################################
  #MOMENTUM FLUX AND FRICTION VELOCITY
  ############################################################
  
  
  #-----------------------------------------------------------
  #FROM INITIAL COMPONENTS
  
  #immidiate fluxes from deviations in mean wind coordinates
  imfl$u_star2_x <- -(imfl$veloXaxsHor * imfl$veloZaxsHor)
  imfl$u_star2_y <- -(imfl$veloYaxsHor * imfl$veloZaxsHor)
  imfl$u_star <- NaN
  
  #-----------------------------------------------------------
  #FROM STRESS TENSOR
  
  #u_star [m s-1]; optionally only considers the along wind stress u_star_x; Foken (2008) Eq.(2.23)
  mn$u_star2_x <- -Mrot2[1,3]
  mn$u_star2_y <- -Mrot2[2,3]
  mn$u_star <- (mn$u_star2_x^2 + mn$u_star2_y^2)^(1/4)
  
  #correlations
  cor <- data.frame(
    u_star2_x= -Mrot2[1,3] / sd$veloXaxsHor / sd$veloZaxsHor,
    u_star2_y= -Mrot2[2,3] / sd$veloYaxsHor / sd$veloZaxsHor
  )
  
  #wind variance; deviations from initial sd are < 2%
  sd_dum <- sqrt(abs(diag(Mrot2)))
  sd$veloXaxsHor <- sd_dum[1]
  sd$veloYaxsHor <- sd_dum[2]
  sd$veloZaxsHor <- sd_dum[3]
  
  #-----------------------------------------------------------
  #CLEAN UP
  rm(Mrot2, sd_dum)
  
  
  
  ############################################################
  #SENSIBLE HEAT FLUX
  ############################################################
  
  
  #SENSIBLE HEAT FLUX 
  #flux in kinematic units [K m s-1]
  imfl$F_H_kin <- imfl$veloZaxsHor * imfl$tempAir
  mn$F_H_kin <- mean(imfl$F_H_kin, na.rm=TRUE)
  
  #conversion to units of energy [W m-2] == [kg s-3]
  imfl$F_H_en <- (eddy4R.base::IntlNatu$CpDry * base$densMoleAirDry * eddy4R.base::IntlNatu$MolmDry + eddy4R.base::IntlNatu$CpH2o * base$densMoleH2o * eddy4R.base::IntlNatu$MolmH2o) * imfl$F_H_kin
  mn$F_H_en <- mean(imfl$F_H_en, na.rm=TRUE)
  
  #BUOYANCY FLUX considering water vapor buoyancy and NIST standard pressure (1013.15 hPa) reference (virt. pot. temp.) -> z/L
  #flux in kinematic units  [K m s-1]
  imfl$F_H_kin_v_0 <- imfl$veloZaxsHor * imfl$tempVirtPot00
  mn$F_H_kin_v_0 <- mean(imfl$F_H_kin_v_0, na.rm=TRUE)
  
  #CORRELATIONS
  cor$F_H_kin <- stats::cor(imfl$veloZaxsHor, imfl$tempAir, use="pairwise.complete.obs")
  cor$F_H_en <- cor$F_H_kin
  cor$F_H_kin_v_0 <- stats::cor(imfl$veloZaxsHor, imfl$tempVirtPot00, use="pairwise.complete.obs")
  
  
  
  ############################################################
  #LATENT HEAT FLUX
  ############################################################
  
  #latent heat flux in kinematic units [mol m-2 s-1]
  imfl$F_LE_kin <- base$densMoleAirDry * imfl$veloZaxsHor * imfl$rtioMoleDryH2o
  mn$F_LE_kin <- mean(imfl$F_LE_kin, na.rm=TRUE)
  #latent heat flux in units of energy  [W m-2] == [kg s-3]
  imfl$F_LE_en <- base$heatH2oGas * eddy4R.base::IntlNatu$MolmH2o * imfl$F_LE_kin
  mn$F_LE_en <- mean(imfl$F_LE_en, na.rm=TRUE)
  #correlation
  cor$F_LE_kin <- stats::cor(imfl$veloZaxsHor, imfl$rtioMoleDryH2o, use="pairwise.complete.obs")
  cor$F_LE_en <- cor$F_LE_kin
  
  ############################################################
  #CH4 FLUX - legacy, include CH4 via the chemistry flux settings
  ############################################################
  if(flagCh4 == TRUE){
    #CH4 flux in kinematic units [mol m-2 s-1]
    imfl$F_CH4_kin <- base$densMoleAirDry * imfl$veloZaxsHor * imfl$rtioMoleDryCo2
    mn$F_CH4_kin <- mean(imfl$F_CH4_kin, na.rm=TRUE)
    #CH4 flux in mass units [mg m-2 h-1]
    imfl$F_CH4_mass <- imfl$F_CH4_kin * eddy4R.base::IntlNatu$MolmCh4 * 1e6 * 3600
    mn$F_CH4_mass <- mean(imfl$F_CH4_mass, na.rm=TRUE)
    #correlation
    cor$F_CH4_kin <- stats::cor(imfl$veloZaxsHor, imfl$rtioMoleDryCo2, use="pairwise.complete.obs")
    cor$F_CH4_mass <- cor$F_CH4_kin
  }
  ############################################################
  # CHEMISTRY FLUX
  ############################################################
  if(!is.null(spcs) & !is.null(rmm)){
    # calculate flux
    fluxChem = def.flux.chem(imfl = imfl,
                             mn = mn,
                             corr = cor,
                             base = base,
                             spcs = spcs,
                             rmm = rmm)
    
    # tidy output
    # When REYNflux becomes wrapper - the arguments/return of this function should be changed such that both
    # the input and output are the same data structure, and this tidy step is no longer required
    # more like how functions can operate on the REYN object (the result of REYNflux)
    imfl = fluxChem$imfl
    mn = fluxChem$mn
    cor = fluxChem$corr
  }
  
  ############################################################
  #AUXILARY PARAMETERS
  ############################################################
  
  #turbulence intensity from total wind vector (Stull, Eq. 1.4d)
  #total wind vector
  tot <- sqrt(data$veloXaxs^2 + data$veloYaxs^2 + data$veloZaxs^2)
  #turbulence intensity should be <0.5 to allow for Taylors hypothesis
  mn$I <- sd(tot, na.rm=TRUE) / mean(tot, na.rm=TRUE)
  
  #Obukhov length (used positive g!)
  mn$d_L_v_0 <- (-(((mn$u_star)^3 / (eddy4R.base::IntlNatu$VonkFokn * eddy4R.base::IntlNatu$Grav / mn$tempVirtPot00 * mn$F_H_kin_v_0 ))))
  
  #stability
  mn$sigma <- mn$d_z_m / mn$d_L_v_0
  
  #convective (Deardorff) velocity [m s-1]
  #missing values in Deardorff velocity and resulting variables when buoyancy flux is negative!
  mn$w_star <- ( eddy4R.base::IntlNatu$Grav * mn$d_z_ABL / mn$tempVirtPot00 * mn$F_H_kin_v_0 )^(1/3)
  
  #(free) convective time scale [s], often in the order of 5-15 min
  mn$t_star <- mn$d_z_ABL / mn$w_star
  
  #temperature scale (eddy temperature fluctuations) [K]
  #surface layer
  #mn$T_star_SL <- - mn$F_H_kin_v_0 / mn$u_star	#according to Stull (1988) p. 356
  mn$T_star_SL <- - mn$F_H_kin / mn$u_star	#according to Foken (2008) p.42, fits with ITC assessment
  #mixed layer
  mn$T_star_ML <-   mn$F_H_kin / mn$w_star #according to Stull (1988) p. 356
  
  #humidity scale (eddy moisture fluctuations) [mol mol-1 dry air]
  #surface layer
  mn$rtioMoleDryH2o_star_SL <- - mean(mn$F_LE_kin / base$densMoleAirDry, na.rm=TRUE) / mn$u_star
  #mixed layer layer
  mn$rtioMoleDryH2o_star_ML <-   mean(mn$F_LE_kin / base$densMoleAirDry, na.rm=TRUE) / mn$w_star 

  #clean up
  rm(tot)
  
  ############################################################
  #EXPORT RESULTS
  ############################################################
  
  
  #PREPARE DATA
  #mins
  mi <- plyr::colwise(min)(data,na.rm=TRUE)
  attributes(mi)$names <- attributes(data)$names
  #maxs
  ma <- plyr::colwise(max)(data,na.rm=TRUE)
  attributes(ma)$names <- attributes(data)$names
  
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
    imfl=imfl,	#immidiate fluctuations
    cor=cor,		#correlation coefficient
    mtrxRot01=mtrxRot01       #transformation matrix for stress tensor
  )
  
  #clean up
  rm(mtrxRot01, base, data, mi, ma, mn, sd, imfl, cor)
  
  #return result
  return(export)
}