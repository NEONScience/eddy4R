##############################################################################################
#' @title Wrapper function: Calculate turbulent vertical flux and auxiliary variables

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function wrapper. Calculate turbulent vertical flux and auxiliary variables.

#' @param data A data frame of class "numeric" and each variable with independent unit attribute assigned. The minimum requirements are: 1) In order to return shear stress, i.e. for the internal call to def.flux.vect() to succeed: the wind vector in meteorological ENU convention with the variables data$veloXaxs (latitudinal wind speed, positive from west), data$veloYaxs (longitudinal wind speed, positive from south), and data$veloZaxs (vertical wind speed, positive from below), each with unit attribute [m s-1]. 2) In order to return scalar fluxes of temperature, H2O and CO2, i.e. for the internal call to def.flux.sclr() to succeed: atmospheric pressure data$presAtm with unit attribute [Pa], air temperature data$tempAir with unit attribute [K], water vapor dry mole fraction data$rtioMoleDryH2o with unit attribute [molH2o mol-1Dry], and CO2 dry mole fraction data$rtioMoleDryCo2 with unit attribute [molCo2 mol-1Dry]. 3) In order to return boundary layer scaling variables, i.e. for the internal call to def.var.abl() to succeed: effective measurement height data$distZaxsMeas with unit attribute [m], and atmospheric boundary layer depth data$distZaxsAbl with unit attribute [m]. 4) Only if AlgBase != mean, for the internal call to def.stat.sta.diff() to succeed: the independent variable data$idep needs to be provided to perform detrending or polynomical regression. data$idep can be assigned flexibly, e.g. for tower measurements the passed time in units [s] or [h], or for aircraft measurements the stretch of air flown through in units [km] or [m]. 5) Optionally, additional variables data$ can be passed through, provided they have the same number of observations as the variables in 1) - 4), and each variable has an independent unit attribute assigned. The following outputs are reported for optional variables: $base (base states), $data (data including internal calculations), $diff (instantaneous differences), $max (maximums), $mean (means), $min (minimums), and $sd (standard deviations).
#' @param AlgBase A vector of length 1 that defines the base state with respect to which instantaneous differences and standard deviations are calculated, of class "character" and no unit attribute. Contains one of AlgBase <- c("mean", "trnd", "ord03")[1] and defaults to "mean", with the additional options detrending "trnd" and 3rd-order polynomial "ord03". See ?eddy4R.base::def.base.ec() for additional details. When AlgBase is set to "trnd" or "ord03", the variable inp$idep is required, which provides the independent variable for interpolation.
#' @param SlctPot A logical TRUE or FALSE: use potential temperature and densities? [-]
#' @param PresPot A vector of length 1 and of class "numeric". If is.true(SlctPot) it provides the reference pressure level for which potential temperature and densities are calculated. [-]
#' @param ListGasSclr A list of gas scalars in data (not including water vapor) to pass to def.flux.sclr() function that includes the Unit data frame. A data frame with the entries InpVect, InpSclr, Conv, Out, of class "character". To ensure consistent units of the returned object, Unit needs to be specified with the constraint that Unit$Out = Unit$InpVect * Unit$InpSclr * Unit$Conv. If the function call argument conv is not specified, then Unit$Conv should be supplied as = "-". The Conv list is character variable that is used to grab the conversion factor from variables in the statStaDiff$base data.frame. If desired conversion factor is not available in can be added directly to data. This defaults to "densMoleAirDry" which is calculated in the function to provide molar fluxes [mol m-2 s-1]. Lastly, NameOut is a character variable for the output name (e.g. "fluxCo2")
#' @param ... Additional arguments that can be passed to the wrapper function.

#' @return A list with the elements $base (base states), $conv (scalar flux conversion factors), $corr (correlations), $data (data including internal calculations), $diff (instantaneous differences), $max (maximums), $mean (means), $min (minimums), $mtrxRot01 (transformation matrix for stress tensor), and $sd (standard deviations).

#' @references Metzger, S., Durden, D., Sturtevant, C., Luo, H., Pingintha-Durden, N., Sachs, T., Serafimovich, A., Hartmann, J., Li, J., Xu, K., and Desai, A. R.: eddy4R 0.2.0: a DevOps model for community-extensible processing and analysis of eddy-covariance data based on R, Git, Docker, and HDF5, Geosci. Model Dev., 10, 3189-3206, doi:10.5194/gmd-10-3189-2017, 2017.

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
#David Durden(2023-12-10)
#     Adding input argument for scalar variables to allow additional calculations
##############################################################################################


wrap.flux <- function(
  data,
  AlgBase = c("mean", "trnd", "ord03")[1],
  SlctPot = FALSE,
  PresPot = eddy4R.base::IntlNatu$Pres00,
  ListGasSclr = list(rtioMoleDryCo2 = list(Conv = "densMoleAirDry", Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molCo2 mol-1Dry", Conv = "mol m-3", Out = "mol m-2 s-1"), NameOut = "fluxCo2")),
  ...
)
{

  library(eddy4R.base)
  rlog = Logger.Singleton$new() #class defined in eddy4R.base
  rlog$debug("in function wrap.flux(...)")
  
  
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
  
  
  # define conversion from kinematic units to units of energy
  # dry air density [mol m-3] x latent heat of vaporization [J kg-1] x molar mass [kg mol-1] = [J m-3] = [kg m-1 s-1]
  statStaDiff$base$convH2oEngy <- statStaDiff$base$densMoleAirDry * statStaDiff$base$heatH2oGas * eddy4R.base::IntlNatu$MolmH2o
  base::attr(statStaDiff$base$convH2oEngy, which = "unit") <- "kg m-1 s-1"
  
  # define conversion from kinematic units to units of evapotranspiration depth
  # dry air mole density [mol m-3] x (H2O molar mass [kg mol-1] / liquid H2O mass density at 4C and 1 Atm [kg m-3]) = [-]
  statStaDiff$base$convH2oVelo <- statStaDiff$base$densMoleAirDry * IntlNatu$MolmH2o / 1e3
  base::attr(statStaDiff$base$convH2oVelo, which = "unit") <- "-"

  
  
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
      # actual flux calculation
      fluxTmp <- def.flux.sclr(
        inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = statStaDiff$diff$rtioMoleDryH2o),
        conv = statStaDiff$base$convH2oEngy,
        Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molH2o mol-1Dry", Conv = "kg m-1 s-1", Out = "W m-2")
      )
      
      # transfer results
      for(idx in base::names(fluxTmp)) statStaDiff[[idx]]$fluxH2oEngy <- fluxTmp[[idx]]
      base::rm(fluxTmp, idx)
    
    # evapotranspiration depth [m s-1]
    # resources: https://www.fao.org/3/X0490E/x0490e04.htm
    # https://github.com/stefanmet/NEON-FIU-algorithm-stefanmet/commit/86c368cfe367aac6f5c90aa8704ed0caf6558e4b
    # https://chemistry.stackexchange.com/questions/23643/calculating-the-volume-of-1-mole-of-liquid-water

      # actual flux calculation
      fluxTmp <- def.flux.sclr(
        inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = statStaDiff$diff$rtioMoleDryH2o),
        conv = statStaDiff$base$convH2oVelo,
        Unit = base::data.frame(InpVect = "m s-1", InpSclr = "molH2o mol-1Dry", Conv = "-", Out = "m s-1")
      )

      # transfer results
      for(idx in base::names(fluxTmp)) statStaDiff[[idx]]$fluxH2oVelo <- fluxTmp[[idx]]
      base::rm(fluxTmp, idx)

#############################################################################          
  # OTHER SCALAR FLUXES INCL. CO2, CH4, NOx, VOCs ETC.
##########################################################################      

    #For loop for additional scalar fluxes
    for(idxGas in names(ListGasSclr)){
      #idxGas <- names(ListGasSclr)[1]
      
      #Check scalar name in input data and assign as input scalar
      if(!(idxGas %in% names(statStaDiff$diff))){
        stop(paste0(idxGas," not found in input data"))}
      inpSclr <- statStaDiff$diff[,idxGas]
      # test for presence of unit attribute
      if(!("unit" %in% names(attributes(inpSclr)))) {
        stop(paste0(idxGas," is missing unit attribute."))}
      
      #Check for presence of scalar conversion factor and assign
      if(!(ListGasSclr[[idxGas]]$Conv %in% names(statStaDiff$base))){
        stop(paste0(idxGas, "not found in input data"))}
        convSclr <- statStaDiff$base[,ListGasSclr[[idxGas]]$Conv]
      
    # CO2 flux in kinematic units [mol m-2 s-1]
    fluxTmp <- def.flux.sclr(
      inp = data.frame(vect = statStaDiff$diff$veloZaxsHor, sclr = inpSclr),
      conv = convSclr,
      Unit = ListGasSclr[[idxGas]]$Unit
    )
    
    for(idx in base::names(fluxTmp)) statStaDiff[[idx]][[ListGasSclr[[idxGas]]$NameOut]] <- fluxTmp[[idx]]
    base::rm(fluxTmp, idx)

    }#End loop around ListGasSclr
    
#############################################################################    

    
  ############################################################
  # BOUNDARY LAYER VARIABLES
  ############################################################

  # actual function call
    
    # prepare input data
    velo <- data[c("veloXaxs", "veloYaxs", "veloZaxs")]
    base::dimnames(velo)[[2]] <- c("Xaxs", "Yaxs", "Zaxs")

    # call function
    varAblTmp <- def.var.abl(
      densMoleAirDry = statStaDiff$mean$densMoleAirDry,
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
