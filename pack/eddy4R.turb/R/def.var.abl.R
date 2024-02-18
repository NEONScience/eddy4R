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
