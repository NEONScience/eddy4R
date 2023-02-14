##############################################################################################
#' @title Wrappter function: Calculate turbulent vertical flux and auxiliary variables

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function wrapper. Calculate turbulent vertical flux and auxiliary variables.

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
#   Stefan Metzger (2023-02-11)
#     rename from REYNflux_P5.R to wrap.flux.R - integrate modularization w/ separated definition functions
##############################################################################################



# attributes(data)$names
# [1] "t_utc"        "d_x_utm"      "d_y_utm"      "d_z_m"        "d_xy_travel"  "d_xy_flow"    "PSI_aircraft" "uvw"         
# [9] "u_met"        "v_met"        "w_met"        "p_air"        "T_air"        "FD_mole_H2O"  "FD_mole_CH4"  "d_z_terrain" 
# [17] "d_z_ABL"      "T_surface"    "R_SW_down"


wrap.flux <- function(
  data,
  AlgBase = c("mean", "trnd", "ord03")[1],
  SlctPot = FALSE,
  PresPot = eddy4R.base::IntlNatu$Pres00,
  ...
)
{
  
  ### rename input data in preparation of terms update (replace with unit test when completing refactoring)
  # keep "data" data.frame object with minimum required variables that are tested for in the beginning of the wrapper
  # in that way additional variables can be supplied (e.g., UTM coordinates for aircraft) and stats calculated
  # algorithm / processing settings as separate, individual specifications of the function call
  
    # moved to main workflow
    # # def.stat.sta.diff
    # # independent variable, required only if AlgBase != mean
    # # can be assigned flexibly for tower, aircraft etc. with corresponding units
    #   # fixed platform, e.g. tower
    #   # data$idep <- data$t_utc
    #   #   data$t_utc <- NULL
    #   #   base::attr(x = data$idep, which = "unit") <- "h"
    #   #   base::attr(x = data$d_xy_travel, which = "unit") <- "s"
    #   # moving platform, e.g. aircraft
    #   data$idep <- data$d_xy_travel
    #     data$d_xy_travel <- NULL
    #     base::attr(x = data$idep, which = "unit") <- "s"
    #     base::attr(x = data$t_utc, which = "unit") <- "h"
    #   
    # # def.flux.vect
    # data$veloXaxs <- data$u_met
    #   data$u_met <- NULL
    #   base::attr(x = data$veloXaxs, which = "unit") <- "m s-1"
    # data$veloYaxs <- data$v_met
    #   data$v_met <- NULL
    #   base::attr(x = data$veloYaxs, which = "unit") <- "m s-1"
    # data$veloZaxs <- data$w_met
    #   data$w_met <- NULL
    #   base::attr(x = data$veloZaxs, which = "unit") <- "m s-1"
    # 
    # # def.flux.sclr
    # data$presAtm <- data$p_air
    #   data$p_air <- NULL
    #   base::attr(x = data$presAtm, which = "unit") <- "Pa"
    # data$tempAir <- data$T_air
    #   data$T_air <- NULL
    #   base::attr(x = data$tempAir, which = "unit") <- "K"
    # data$rtioMoleDryH2o <- data$FD_mole_H2O
    #   data$FD_mole_H2O <- NULL
    #   base::attr(x = data$rtioMoleDryH2o, which = "unit") <- "molH2o mol-1Dry"
    # data$rtioMoleDryCo2 <- data$FD_mole_CH4
    #   data$FD_mole_CH4 <- NULL
    #   base::attr(x = data$rtioMoleDryCo2, which = "unit") <- "molCo2 mol-1Dry"
    #   
    # # def.var.abl
    # data$distZaxsMeas <- data$d_z_m
    #   data$d_z_m <- NULL
    #   base::attr(x = data$distZaxsMeas, which = "unit") <- "m"
    # data$distZaxsAbl <- data$d_z_ABL
    #   data$d_z_ABL <- NULL
    #   base::attr(x = data$distZaxsAbl, which = "unit") <- "m"
    # 
    # # optional: pass-through
    # base::attr(x = data$t_doy_utc, which = "unit") <- "d"
    # base::attr(x = data$t_doy_local, which = "unit") <- "d"
    # base::attr(x = data$d_x_utm, which = "unit") <- "m"
    # base::attr(x = data$d_y_utm, which = "unit") <- "m"
    # base::attr(x = data$d_xy_flow, which = "unit") <- "s"
    # data$PSI_aircraft <- eddy4R.base::def.conv.poly(data = data$PSI_aircraft, coefPoly = eddy4R.base::IntlConv$DegRad)
    #   attributes(data$PSI_aircraft)$unit <- "rad"
    # base::attr(x = data$uvw_aircraft, which = "unit") <- "m s-1"
    # base::attr(x = data$uv_met, which = "unit") <- "m s-1"
    # base::attr(x = data$d_z_terrain, which = "unit") <- "m"

  
  
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

  # output data assignments
  
    # re-assign SDs for consistency among all shear-stress-related quantities, analogous to legacy workflow
    # context: SDs are already computed in def.stat.sta.diff with deviations < 2%
    for(idx in base::names(fluxVect$sd)) statStaDiff$sd[[idx]] <- fluxVect$sd[[idx]]
    base::rm(idx)
    
    # initiate dataframe to store correlations and assign fluxVect results
    statStaDiff$corr <- fluxVect$corr
    
    # also assign fluxVect diff and mean results
    for(idx in c("diff", "mean")) statStaDiff[[idx]] <- base::cbind(statStaDiff[[idx]], fluxVect[[idx]])
    base::rm(fluxVect, idx)
  
    
  
  ############################################################
  #SENSIBLE HEAT FLUX
  ############################################################

  # initiate dataframe to store conversion factors and correlations
  statStaDiff$conv <- base::data.frame(fluxTemp = base::rep(NaN, length.out = nrow(statStaDiff$diff)))
  # statStaDiff$corr <- base::data.frame(fluxTemp = NaN)

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

  # actual function call
    
    # prepare input data
    velo <- data[c("veloXaxs", "veloYaxs", "veloZaxs")]
    base::dimnames(velo)[[2]] <- c("Xaxs", "Yaxs", "Zaxs")

    # call function
    varAblTmp <- def.var.abl(
      densMoleAirDry = statStaDiff$base$densMoleAirDry,
      distZaxsAbl = statStaDiff$mean$distZaxsAbl,
      distZaxsMeas = statStaDiff$mean$distZaxsMeas,
      fluxH2o = statStaDiff$mean$fluxH2o,
      fluxTemp = statStaDiff$mean$fluxTemp,
      fluxTempVirtPot00 = statStaDiff$mean$fluxTempVirtPot00,
      tempVirtPot00 = statStaDiff$mean$tempVirtPot00,
      velo = velo,
      veloFric = statStaDiff$mean$veloFric
    )
    
    # assign outputs
    for(idx in base::names(varAblTmp)) statStaDiff$mean[[idx]] <- varAblTmp[[idx]]

    # clean up
    base::rm(idx, varAblTmp, velo)
    

    
    
############################################################
# RETURN RESULTS
############################################################

  # complete list to be returned
    # "min" # minimums
    # "max" # maximums
    # "mean" # means
    # "base" # base states
    # "diff" # instantaneous differences
    # "sd" # standard deviations
    # "corr" # correlations
    # "conv" # scalar flux conversion factors
    statStaDiff$data <- data #data including internal calculations
    statStaDiff$mtrxRot01 <- rot$mtrxRot01# transformation matrix for stress tensor
    statStaDiff <- statStaDiff[base::sort(base::names(statStaDiff))]

  #clean up
  rm(data, rot)
  
  #return result
  return(statStaDiff)

}
