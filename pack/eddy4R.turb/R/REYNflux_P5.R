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
  data$tempVirt <- def.temp.virt.temp.air.pres.h2o.pres.atm(
    tempAir = data$tempAir,
    presH2o = data$presH2o,
    presAtm = data$presAtm)

  # volumetric heat capacity [kg m-1 s-2 K-1] = specific heat [J kg-1 K-1] x mole density [mol m-3] x mole mass [kg mol-1]
  data$heatAirWet <- def.heat.air.wet(densMoleAirDry = data$densMoleAirDry, densMoleH2o = data$densMoleH2o)
  
  # latent heat of vaporization (Eq 2.55 Foken 2008) [J kg-1] == [m2 s-2]
  data$heatH2oGas <- def.heat.h2o.gas.temp(tempAir = data$tempAir)

  # Thermodynamic properties of a mix of dry air and water vapor
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
  
  # function call
  dataTmp <- def.rot.ang.zaxs.erth(inp = data[c("veloXaxs", "veloYaxs", "veloZaxs")])
  
  # assign results and clean up
  data <- base::cbind(data, dataTmp$data)
  rot <- dataTmp$rot
  base::rm(dataTmp)
  
  
  
  ############################################################
  # SUMMARY STATISTICS, BASE STATES AND DIFFERENCES
  ############################################################
  
  # function call
  statStaDiff <- def.stat.sta.diff(
    inp = data,
    refe = base::list("mean" = base::list("angZaxsErth" = rot$angZaxsErth)),
    AlgBase = AlgBase
    )

  
  
  ############################################################
  # MOMENTUM FLUX AND FRICTION VELOCITY
  ############################################################

  # function call
  fluxVect <- def.flux.vect(
    inp = statStaDiff$diff[c("veloXaxs", "veloYaxs", "veloZaxs", "veloXaxsHor", "veloYaxsHor", "veloZaxsHor")],
    rot = rot,
    Unit = base::data.frame(Inp = "m s-1", Out = "m s-1", OutSq = "m2 s-2")
  )
  

  
  
  ############################################################
  #SENSIBLE HEAT FLUX
  ############################################################

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

    #' @param densMoleAirDry A vector of length 1 containing the dry air density, of class "numeric" and with unit attribute [mol m-3]. Required to return rtioMoleDryH2oScalAbl, rtioMoleDryH2oScalAtmSurf.
    #' @param distZaxsAbl A vector of length 1 containing the atmospheric boundary layer height above ground, of class "numeric" and with unit attribute [m]. Required to return rtioMoleDryH2oScalAbl, tempScalAbl, timeScalCvct, veloScalCvct.
    #' @param distZaxsMeas A vector of length 1 containing the effective measurement height (i.e. vertical distance of the measurement above displacement height), of class "numeric" and with unit attribute [m]. Required to return paraStbl.
    #' @param fluxH2o A vector of length 1 containing the latent heat flux in kinematic units, of class "numeric" and with unit attribute [mol m-2 s-1]. Required to return rtioMoleDryH2oScalAbl, rtioMoleDryH2oScalAtmSurf.
    #' @param fluxTemp A vector of length 1 containing the sensible heat flux in kinematic units, of class "numeric" and with unit attribute [K m s-1]. Required to return tempScalAtmSurf, which in turn feeds into def.itc().
    #' @param fluxTempVirtPot00 [K m s-1] A vector of length 1 containing the buoyancy flux in kinematic units, of class "numeric" and with unit attribute [K m s-1]. Required to return distObkv, paraStbl, rtioMoleDryH2oScalAbl, tempScalAbl, timeScalCvct, veloScalCvct.
    #' @param tempVirtPot00 [K] A vector of length 1 containing the virtual potential temperature, of class "numeric" and with unit attribute [K]. Required to return distObkv, paraStbl, rtioMoleDryH2oScalAbl,  tempScalAbl, timeScalCvct, veloScalCvct.
    #' @param velo A data frame of the 3-dimensional wind vector in any orthogonal coordinate representation. Contains the variables Xaxs, Yaxs, Xaxs, each of class "numeric" and with unit attribute [m s-1]. Required to return coefSdMeanVelo.
    #' @param veloFric A vector of length 1 containing the friction velocity, of class "numeric" and with unit attribute [m s-1]. Required to return distObkv, paraStbl, rtioMoleDryH2oScalAtmSurf, tempScalAtmSurf.

    #' @return
    #' The returned object is a data frame with a single observation, containing the following variables:
    #'    $coefSdMeanVelo: turbulence intensity (aka coefficient of variation) of the  total wind vector in unit [-]. Requires the following numeric function argument to be supplied for non-NaN result: velo.
    #'    $distObkv: Obukhov length in unit [m]. Requires the following numeric function arguments to be supplied for non-NaN result: fluxTempVirtPot00, tempVirtPot00, veloFric.
    #'    $paraStbl: Atmospheric stability in unit [-]. Requires the following numeric function arguments to be supplied for non-NaN result: distZaxsMeas, fluxTempVirtPot00, tempVirtPot00, veloFric.
    #'    $rtioMoleDryH2oScalAbl: Humidity scale in the atmospheric boundary layer in units [molH2o mol-1Dry]. Requires the following numeric function arguments to be supplied for non-NaN result: densMoleAirDry, distZaxsAbl, fluxH2o, fluxTempVirtPot00, tempVirtPot00.
    #'    $rtioMoleDryH2oScalAtmSurf: Humidity scale in the atmospheric surface layer in units [molH2o mol-1Dry]. Requires the following numeric function arguments to be supplied for non-NaN result: densMoleAirDry, fluxH2o, veloFric.
    #'    $tempScalAbl: Temperature scale in the atmospheric boundary layer in unit [K]. Requires the following numeric function arguments to be supplied for non-NaN result: distZaxsAbl, fluxTempVirtPot00, tempVirtPot00.
    #'    $tempScalAtmSurf: Temperature scale in the atmospheric surface layer in unit [K]. Requires the following numeric function arguments to be supplied for non-NaN result: fluxTemp, veloFric.
    #'    $timeScalCvct: Convective time scale in the atmospheric boundary layer in unit [s]. Requires the following numeric function arguments to be supplied for non-NaN result: distZaxsAbl, fluxTempVirtPot00, tempVirtPot00.
    #'    $veloScalCvct: Convective velocity scale in the atmospheric boundary layer (aka Deardorff velocity) in units [m s-1]. Requires the following numeric function arguments to be supplied for non-NaN result: distZaxsAbl, fluxTempVirtPot00, tempVirtPot00.
    

    #' @references
    #' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
    #' Stull, R. B.: An Introduction to Boundary Layer Meteorology, Kluwer Academic Publishers, Dordrecht, The Netherlands, 670 pp., 1988.
    #' Foken, T.: Micrometeorology, Springer, Berlin, Heidelberg, 306 pp., 2008.
    
    #' @keywords turbulence intensity, coefficient of variation, Obukhov length, atmospheric stability, convective velocity, Deardorff velocity, convective timescale, temperature scale, humidity scale
    
    #' @examples
    #' prepare input data
    #' 
    #'   create emptly list for inputs
    #'   inp <- base::list()
    #'   
    #'   dry air density
    #'   inp$densMoleAirDry <- 41.91337
    #'   base::attr(inp$densMoleAirDry,"unit") <- "mol m-3"
    #'   
    #'   atmospheric boundary layer height
    #'   inp$distZaxsAbl = 1000
    #'   base::attr(inp$distZaxsAbl,"unit") <- "m"
    #'   
    #'   effective measurement height
    #'   inp$distZaxsMeas = 8
    #'   base::attr(inp$distZaxsMeas,"unit") <- "m"
    #'   
    #'   latent heat flux in kinematic units
    #'   inp$fluxH2o = 9.019642e-06
    #'   base::attr(inp$fluxH2o,"unit") <- "mol m-2 s-1"
    #'   
    #'   sensible heat flux in kinematic units
    #'   inp$fluxTemp = 0.001527586
    #'   base::attr(inp$fluxTemp,"unit") <- "K m s-1"
    #'   
    #'   buoyancy flux in kinematic units
    #'   inp$fluxTempVirtPot00 = 0.001355592
    #'   base::attr(inp$fluxTempVirtPot00,"unit") <- "K m s-1"
    #'   
    #'   virtual potential temperature
    #'   inp$tempVirtPot00 = 274.1534
    #'   base::attr(inp$tempVirtPot00,"unit") <- "K"
    #'   
    #'   3-dimensional wind vector
    #'   inp$velo <- base::data.frame(
    #'     Xaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
    #'     Yaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
    #'     Zaxs = c(0.1766139, 0.1849477, 0.3443318, 0.1902303, 0.2391932)
    #'   )
    #'   base::attr(inp$velo$Xaxs,"unit") <- "m s-1"
    #'   base::attr(inp$velo$Yaxs,"unit") <- "m s-1"
    #'   base::attr(inp$velo$Zaxs,"unit") <- "m s-1"
    #'   
    #'   friction velocity
    #'   inp$veloFric = 0.15489
    #'   base::attr(inp$veloFric,"unit") <- "m s-1"
    #'   
    #' call function
    #' 
    #'   Example 1. All function argument values and units supplied - works.
    #'   out <- def.var.abl(
    #'     densMoleAirDry = inp$densMoleAirDry,
    #'     distZaxsAbl = inp$distZaxsAbl,
    #'     distZaxsMeas = inp$distZaxsMeas,
    #'     fluxH2o = inp$fluxH2o,
    #'     fluxTemp = inp$fluxTemp,
    #'     fluxTempVirtPot00 = inp$fluxTempVirtPot00,
    #'     tempVirtPot00 = inp$tempVirtPot00,
    #'     velo = inp$velo,
    #'     veloFric = inp$veloFric
    #'   )
    #'   utils::str(out)
    #'   base::rm(out)
    #'   
    #'   Example 2. fluxTemp is missing unit attribute - throws error.
    #'   base::attr(inp$fluxTemp,"unit") <- NULL
    #'   
    #'   out <- def.var.abl(
    #'     densMoleAirDry = inp$densMoleAirDry,
    #'     distZaxsAbl = inp$distZaxsAbl,
    #'     distZaxsMeas = inp$distZaxsMeas,
    #'     fluxH2o = inp$fluxH2o,
    #'     fluxTemp = inp$fluxTemp,
    #'     fluxTempVirtPot00 = inp$fluxTempVirtPot00,
    #'     tempVirtPot00 = inp$tempVirtPot00,
    #'     velo = inp$velo,
    #'     veloFric = inp$veloFric
    #'   )
    #'   
    #'   Example 3. fluxTemp is not supplied as function argument - tempScalAtmSurf reported as NaN.
    #'   out <- def.var.abl(
    #'     densMoleAirDry = inp$densMoleAirDry,
    #'     distZaxsAbl = inp$distZaxsAbl,
    #'     distZaxsMeas = inp$distZaxsMeas,
    #'     fluxH2o = inp$fluxH2o,
    #'     fluxTempVirtPot00 = inp$fluxTempVirtPot00,
    #'     tempVirtPot00 = inp$tempVirtPot00,
    #'     velo = inp$velo,
    #'     veloFric = inp$veloFric
    #'   )
    #'   utils::str(out)
    #'   base::rm(inp, out)

    #' @seealso Currently none.
    
    #' @export
    
    # changelog and author contributions / copyrights
    #   Stefan Metzger (2011-03-04)
    #     original creation
    #   Stefan Metzger (2022-12-23 - 2022-12-27)
    #     update to eddy4R terminology and modularize into definition function
    ###############################################################################################
    
    
    def.var.abl <- function(
      densMoleAirDry = NULL,
      distZaxsAbl = NULL,
      distZaxsMeas = NULL,
      fluxH2o = NULL,
      fluxTemp = NULL,
      fluxTempVirtPot00 = NULL,
      tempVirtPot00 = NULL,
      velo = NULL,
      veloFric = NULL
    ) {
      
      # check presence of input arguments and consistent units
      
        # list of expected units for function arguments
        UnitExpc <- base::list()
        UnitExpc$densMoleAirDry <- "mol m-3"  
        UnitExpc$distZaxsAbl <- "m"
        UnitExpc$distZaxsMeas <- "m"
        UnitExpc$fluxH2o <- "mol m-2 s-1"
        UnitExpc$fluxTemp <- "K m s-1"
        UnitExpc$fluxTempVirtPot00 <- "K m s-1"
        UnitExpc$tempVirtPot00 <- "K"
        UnitExpc$velo <- "m s-1"
        UnitExpc$veloFric <- "m s-1"
      
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
            
            # check that object is of class numeric vector
            if(base::class(velo[[idx]]) != "numeric") {
              stop(base::paste0("def.var.abl(): velo$", idx, " is not of class numeric vector, please check."))}
            
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
            
            # check that object is of length = 1
            if(base::length(tmp) != 1) {
              stop(base::paste0("def.var.abl(): ", idx, " is not of length = 1, please check."))}
            
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
        
      # turbulence intensity (aka coefficient of variation) of the  total wind vector (Stull, Eq. 1.4d)
        
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
        
      # sort results alphabetically and return
      rpt <- rpt[,base::order(base::names(rpt))]
      return(rpt)
      
    }
  }
    

  # actual function call
    
    # prepare input data
    velo <- data[c("veloXaxs", "veloYaxs", "veloZaxs")]
    base::dimnames(velo)[[2]] <- c("Xaxs", "Yaxs", "Zaxs")

    # call function
    varAblTmp <- def.var.abl(
      densMoleAirDry = statStaDiff$base$densMoleAirDry,
      distZaxsAbl = statStaDiff$mean$d_z_ABL,
      distZaxsMeas = statStaDiff$mean$d_z_m,
      fluxH2o = statStaDiff$mean$fluxH2o,
      fluxTemp = statStaDiff$mean$fluxTemp,
      fluxTempVirtPot00 = statStaDiff$mean$fluxTempVirtPot00,
      tempVirtPot00 = statStaDiff$mean$tempVirtPot00,
      velo = velo,
      veloFric = fluxVect$mean$veloFric
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