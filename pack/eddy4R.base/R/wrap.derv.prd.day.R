##############################################################################################
#' @title Calculation of derived quantities (daily extent, native resolution)

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Wrapper function. Reads the list \code{data} in the format provided by function \code{eddy4R.base::wrap.read.hdf5.neon.eddy()}. For the list entries in \code{data} the following derived quantities are calculated, each through the call to a separate definition function: \cr
#' \code{data$time}: fractional UTC time, fractional day of year, local standard time;  \cr
#' \code{data$irga}: average signal strength, delta signal strength, total pressure, average temperature, water vapor partial pressure, water vapor saturation pressure, relative humidity, molar density of air (dry air and water vapor), molar density of dry air, wet mass fraction (specific humidity);  \cr
#' \code{data$soni}: sonic temperature.

#' @param \code{data} List consisting of \code{ff::ffdf} file-backed objects, in the format provided by function \code{eddy4R.base::wrap.read.hdf5.neon.eddy()}. Of types numeric and integer.
#' @param \code{SiteLoca} List consisting of site-specific parameters. Of types numeric, integer and character.

#' @return 
#' The returned object consistes of \code{data}, with the derived variables added to the respective list entry, and all list levels sorted alphabetically.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords derived, high-frequency, irga, post-processing, pre-processing, sonic, time

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
##############################################################################################
    
    
# derived quantities: daily basis upon import

wrap.derv.prd.day <- function(
  data,
  SiteInfoLoca
) {

# time

  # create temporary variable holding POSIXlt object
  tmp <- as.POSIXlt(data$time$UTC[])

  # fractional UTC time
  data$time$UTC_frac <- ff::as.ff(tmp$hour + tmp$min / 60 + tmp$sec / 3600)
  base::attr(x = data$time$UTC_frac, which = "unit") <- "h"

  # fractional day of year [DOY]
  data$time$DOY_frac <- ff::as.ff(tmp$yday + 1 + data$time$UTC_frac[] / 24)
  base::attr(x = data$time$DOY_frac, which = "unit") <- "d"
  
  # calculate local standard time
  tmp <- data$time$UTC[]
  attributes(tmp)$tzone <- SiteInfoLoca$Tz
  data$time$Loca <- ff::as.ff(tmp)
  base::attr(x = data$time$Loca, which = "unit") <- "YYYY-MM-DD hh:mm:ss.sss"

  # clean up
  rm(tmp)

#irga

  # average signal strength

  data$irga$ssiMean <- ff::as.ff(def.ssi.mean(ssiCo2 = data$irga$ssiCO2,
                                                     ssiH2o = data$irga$ssiH2O))
  base::attr(x = data$irga$ssiMean, which = "unit") <- base::attr(x = data$irga$ssiCO2, which = "unit")
  
  # delta signal strength
  data$irga$RSSI_delta_7200 <- def.ssi.diff(ssiCo2 = data$irga$ssiCO2, ssiH2o = data$irga$ssiH2O)
 
  # total pressure in irga cell
  data$irga$presSum <- def.pres.sum(presAtm = data$irga$presAtm,presDiff = data$irga$presDiff)
  

  # average temperature in irga cell 
  data$irga$tempMean <- def.temp.mean.7200(tempIn = data$irga$tempIn,
                                              tempOut = data$irga$tempOut)
 
  # RH in cell

    # water vapor partial pressure
    data$irga$presH2o <- ff::as.ff(def.pres.h2o.rtio.mole.h2o.dry.pres(rtioMoleDryH2o = data$irga$fdMoleH2O, pres = data$irga$presSum))
    base::attr(x = data$irga$presH2o, which = "unit") <- "Pa"

    # water vapor saturation pressure
    if(!is.na(mean(data$irga$tempMean, na.rm=TRUE))) {
      data$irga$presH2oSat <- ff::as.ff(as.vector(def.pres.h2o.sat.temp.mag(temp = data$irga$tempMean[])))
    } else {
      data$irga$presH2oSat <- ff::as.ff(rep(NaN, length(data$irga$tempMean)))
    }
    base::attr(x = data$irga$presH2oSat, which = "unit") <- "Pa"
    
    # calcuate RH
    data$irga$RH_7200 <- def.rh.pres.h2o.pres.sat.h2o(presH2o = data$irga$presH2o, presH2oSat = data$irga$presH2oSat)
    

  # molar density of dry air and water vapor
  data$irga$rho_mole_air_7200 <- def.dens.mole.air(presSum = data$irga$presSum,
                                                   tempMean = data$irga$tempMean)
  
  # molar density of dry air alone
  data$irga$rho_mole_dry_7200 <- def.dens.mole.air.dry(densMoleAir = data$irga$rho_mole_air_7200,
                                                       densMoleH2o = data$irga$rhoMoleH2O)
  

  # wet mass fraction (specific humidity)
  data$irga$FW_mass_H2O_7200 <- def.rtio.mass.h2o.dens.mole(densMoleH2o = data$irga$rhoMoleH2O,
                                                             densMoleAirDry = data$irga$rho_mole_dry_7200)

# soni
  
  # sonic temperature [K] from speed of sound [m s-1] (Campbell Scientific, Eq. (9))
  data$soni$T_SONIC <- def.temp.soni(veloSoni = data$soni$veloSoni)
  
# sort object levels alphabetically

  # data
  data <- data[names(data)[order(tolower(names(data)))]]

  # data$irga
  data$irga <- data$irga[names(data$irga)[order(tolower(names(data$irga)))]]
  
  # data$irga MFC
  data$irgaMfcSamp <- data$irgaMfcSamp[names(data$irgaMfcSamp)[order(tolower(names(data$irgaMfcSamp)))]]
  
  # data$soni
  data$soni <- data$soni[names(data$soni)[order(tolower(names(data$soni)))]]

# return results
return(data)
    
}
