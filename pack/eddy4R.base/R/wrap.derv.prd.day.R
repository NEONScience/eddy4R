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

  data$irga$RSSI_mean_7200 <- ff::as.ff(def.ssi.mean(ssiCo2 = data$irga$ssiCO2,
                                                     ssiH2o = data$irga$ssiH2O))
  base::attr(x = data$irga$RSSI_mean_7200, which = "unit") <- base::attr(x = data$irga$ssiCO2, which = "unit")
  
  # delta signal strength
  data$irga$RSSI_delta_7200 <- def.ssi.diff(ssiCo2 = data$irga$ssiCO2, ssiH2o = data$irga$ssiH2O)
 
  # total pressure in irga cell
  data$irga$p_cell_7200 <- ff::as.ff(data$irga$presAtmBox + data$irga$presGageCell)
  base::attr(x = data$irga$p_cell_7200, which = "unit") <- base::attr(x = data$irga$presAtmBox, which = "unit")

  # average temperature in irga cell 
  data$irga$T_cell_7200 <- ff::as.ff(0.2 * data$irga$tempCellIn + 0.8 * data$irga$tempCellOut)
  base::attr(x = data$irga$T_cell_7200, which = "unit") <- base::attr(x = data$irga$tempCellIn, which = "unit")

  # RH in cell

    # water vapor partial pressure
    data$irga$presH2o <- ff::as.ff(def.pres.h2o.rtio.mole(rtioMoleDryH2o = data$irga$fdMoleH2O, pres = data$irga$p_cell_7200))
    base::attr(x = data$irga$presH2o, which = "unit") <- "Pa"

    # water vapor saturation pressure
    if(!is.na(mean(data$irga$T_cell_7200, na.rm=TRUE))) {
      data$irga$presH2oSat <- ff::as.ff(as.vector(def.pres.h2o.sat(temp = data$irga$T_cell_7200[])))
    } else {
      data$irga$presH2oSat <- ff::as.ff(rep(NaN, length(data$irga$T_cell_7200)))
    }
    base::attr(x = data$irga$presH2oSat, which = "unit") <- "Pa"
    
    # calcuate RH
    data$irga$RH_7200 <- ff::as.ff(data$irga$presH2o / data$irga$presH2oSat * 100)
    base::attr(x = data$irga$RH_7200, which = "unit") <- "%"

  # molar density of dry air and water vapor
  data$irga$rho_mole_air_7200 <- ff::as.ff(data$irga$p_cell_7200 / eddy4R.base::Natu$Rg / data$irga$T_cell_7200)
  base::attr(x = data$irga$rho_mole_air_7200, which = "unit") <- "mol m-3"

  # molar density of dry air alone
  data$irga$rho_mole_dry_7200 <- ff::as.ff(data$irga$rho_mole_air_7200 - data$irga$rhoMoleH2O)
  base::attr(x = data$irga$rho_mole_dry_7200, which = "unit") <- "mol m-3"

  # wet mass fraction (specific humidity)
  data$irga$FW_mass_H2O_7200 <- ff::as.ff(data$irga$rhoMoleH2O * eddy4R.base::Natu$MolmH2o /
    (data$irga$rho_mole_dry_7200 * eddy4R.base::Natu$MolmDry + data$irga$rhoMoleH2O * eddy4R.base::Natu$MolmH2o))
  base::attr(x = data$irga$FW_mass_H2O_7200, which = "unit") <- "kg kg-1"

# soni
  
  # sonic temperature [K] from speed of sound [m s-1] (Campbell Scientific, Eq. (9))
  data$soni$T_SONIC <- ff::as.ff(data$soni$veloSoni^2 / eddy4R.base::Natu$GmmaDry / 
                                  (eddy4R.base::Natu$Rg / eddy4R.base::Natu$MolmDry))
  base::attr(x = data$soni$T_SONIC, which = "unit") <- "K"

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
