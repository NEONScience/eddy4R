##############################################################################################
#' @title Reading NEON HDF5 files

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Wrapper function. Reads an HDF5 input file in NEON standard format from \code{DirInpLoca}. Subsequently, (i) name and unit attributes are converted to eddy4R standard terms, (ii) variable units are converted accordingly, (iii) the data is regularized, (iv) sensor diagnostic tests are performed, (v) a range test is performed, and (vi) the data is de-spiked.

#' @param \code{DirInpLoca} Character: Input directory.
#' @param \code{SiteLoca} Character: Site location.
#' @param \code{DateLoca} Character: Date in ISO format "(2016-05-26").
#' @param \code{VarLoca} Character: Which instrument to read data from.
#' @param \code{FreqLoca} Integer: Measurement frequency.
#' @param \code{RngLoca} List of named ingegers: Thresholds for range test.
#' @param \code{DespLoca} List of integers: De-spiking parameters

#' @return 
#' Named list containing pre-processed time-series $time and $data.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords file, read, pre-processing, unit conversion, regularization, diagnostic, range, spike

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-08-07)
#     original creation
#   Stefan Metzger (2016-08-23)
#     use unit conversion with "internal" units
##############################################################################################
    
    
# derived quantities: daily basis upon import

wrap.derv.prd.day <- function(
  data = inp,
  SiteInfoLoca = SiteInfo
) {

# time

  # create temporary variable holding POSIXlt object
  tmp <- as.POSIXlt(data$time[])

  # fractional UTC time        
  data$time$UTC_frac <- ff::as.ff(tmp$hour + tmp$min / 60 + tmp$sec / 3600)
  
  #fractional day of year [DOY]
  data$time$DOY_frac <- ff::as.ff(tmp$yday + 1 + data$time$UTC_frac[] / 24)
  
  #calculate local standard time
  tmp <- data$time$UTC[]
  attributes(tmp)$tzone <- SiteInfoLoca$Tz
  data$time$Loca <- ff::as.ff(tmp)

  # clean up
  rm(tmp)

#irga

  # average signal strength
  data$irga$RSSI_mean_7200 <- ff::as.ff((data$irga$ssiCO2 + data$irga$ssiH2O) / 2)
  attributes(data$irga)$unit["RSSI_mean_7200"] <- attributes(data$irga)$unit["ssiCO2"]
  
  # delta signal strength
  data$irga$RSSI_delta_7200 <- ff::as.ff(data$irga$ssiCO2 - data$irga$ssiH2O)
  attributes(data$irga)$unit["RSSI_delta_7200"] <- attributes(data$irga)$unit["ssiCO2"]

  # total pressure in irga cell
  data$irga$p_cell_7200 <- ff::as.ff(data$irga$presAtmBox + data$irga$presGageCell)
  attributes(data$irga)$unit["p_cell_7200"] <- attributes(data$irga)$unit["presAtmBox"]
  
  # average temperature in irga cell 
  data$irga$T_cell_7200 <- ff::as.ff(0.2 * data$irga$tempCellIn + 0.8 * data$irga$tempCellOut)
  attributes(data$irga)$unit["T_cell_7200"] <- attributes(data$irga)$unit["tempCellIn"]

  # RH in cell

    # water vapor partial pressure
    data$irga$presH2o <- ff::as.ff(def.pres.h2o.rtio.mole(rtioMoleDryH2o = data$irga$fdMoleH2O, pres = data$irga$p_cell_7200))
    attributes(data$irga)$unit["presH2o"] <- "Pa"

    # water vapor saturation pressure
    if(!is.na(mean(data$irga$T_cell_7200, na.rm=TRUE))) {
      data$irga$presH2oSat <- ff::as.ff(as.vector(def.pres.h2o.sat(temp = data$irga$T_cell_7200[])))
    } else {
      data$irga$presH2oSat <- ff::as.ff(rep(NaN, length(data$irga$T_cell_7200)))
    }
    attributes(data$irga)$unit["presH2oSat"] <- "Pa"
    
    # calcuate RH
    data$irga$RH_7200 <- ff::as.ff(data$irga$presH2o / data$irga$presH2oSat * 100)
    attributes(data$irga)$unit["RH_7200"] <- "%"

  # molar density of dry air and water vapor
  data$irga$rho_mole_air_7200 <- ff::as.ff(data$irga$p_cell_7200 / eddy4R.base::Natu$Rg / data$irga$T_cell_7200)
  attributes(data$irga)$unit["rho_mole_air_7200"] <- "mol m-3"
  
  # molar density of dry air alone
  data$irga$rho_mole_dry_7200 <- ff::as.ff(data$irga$rho_mole_air_7200 - data$irga$rhoMoleH2O)
  attributes(data$irga)$unit["rho_mole_dry_7200"] <- "mol m-3"
  
  # wet mass fraction (specific humidity)
  data$irga$FW_mass_H2O_7200 <- ff::as.ff(data$irga$rhoMoleH2O * eddy4R.base::Natu$MolmH2o /
    (data$irga$rho_mole_dry_7200 * eddy4R.base::Natu$MolmDry + data$irga$rhoMoleH2O * eddy4R.base::Natu$MolmH2o))
  attributes(data$irga)$unit["FW_mass_H2O_7200"] <- "kg kg-1"

# soni
  
  # sonic temperature [K] from speed of sound [m s-1] (Campbell Scientific, Eq. (9))
  data$soni$T_SONIC <- ff::as.ff(data$soni$veloSoni^2 / eddy4R.base::Natu$GmmaDry / 
                                  (eddy4R.base::Natu$Rg / eddy4R.base::Natu$MolmDry))
  attributes(data$soni)$unit["T_SONIC"] <- "K"

# sort object levels alphabetically

  # data
  whr <- names(data)[order(tolower(names(data)))]
  data <- data[whr]
  rm(whr)

  # data$irga
  whr <- names(data$irga)[order(tolower(names(data$irga)))]
  tmp <- attributes(data$irga)$unit[whr]
  data$irga <- data$irga[whr]
  attributes(data$irga)$unit <- tmp
  rm(tmp, whr)

  # data$irga MFC
  whr <- names(data$irgaMfcSamp)[order(tolower(names(data$irgaMfcSamp)))]
  tmp <- attributes(data$irgaMfcSamp)$unit[whr]
  data$irgaMfcSamp <- data$irgaMfcSamp[whr]
  attributes(data$irgaMfcSamp)$unit <- tmp
  rm(tmp, whr)
  
  # data$soni
  whr <- names(data$soni)[order(tolower(names(data$soni)))]
  tmp <- attributes(data$soni)$unit[whr]
  data$soni <- data$soni[whr]
  attributes(data$soni)$unit <- tmp
  rm(tmp, whr)

# print message to screen
print(paste0(format(Sys.time(), "%F %T"), ": dataset ", date, ": derived high-frequency quantities calculated"))
  
# return results
return(data)
    
}
