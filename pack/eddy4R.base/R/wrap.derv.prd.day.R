##############################################################################################
#' @title Wrapper function: Calculation of derived quantities (daily extent, native resolution)

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Wrapper function. Reads the list \code{inpList} in the format provided by function \code{eddy4R.base::wrap.neon.read.hdf5.eddy()}. For the list entries in \code{inpList} the following derived quantities are calculated, each through the call to a separate definition function: \cr
#' \code{inpList$data$time}: fractional UTC time, fractional day of year, local standard time;  \cr
#' \code{inpList$data$irga}: average signal strength, delta signal strength, total pressure, average temperature, water vapor partial pressure, water vapor saturation pressure, relative humidity, molar density of air (dry air and water vapor), molar density of dry air, wet mass fraction (specific humidity);  \cr
#' \code{inpList$data$soni}: sonic temperature.

#' @param inpList List consisting of \code{ff::ffdf} file-backed objects, in the format provided by function \code{eddy4R.base::wrap.neon.read.hdf5.eddy()}. Of types numeric and integer.
#' @param SiteLoca List consisting of site-specific parameters. Of types numeric, integer and character.
#' @param AngZaxsSoniInst  Parameter of class numeric. Azimuth (angle around z axis) direction against true north in which sonic anemometer installation (transducer array) is pointing [deg]
#' 
#' @return 
#' The returned object consistes of \code{inpList}, with the derived variables added to the respective list entry, and all list levels sorted alphabetically.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords derived, high-frequency, irga, post-processing, pre-processing, sonic, time, qfqm

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
#   David Durden (2017-05-14)
#     adapting to include derived flags
#   David Durden (2017-11-08)
#     removing high frequency data that have failed high frequency quality flags
##############################################################################################
    
    
# derived quantities: daily basis upon import

wrap.derv.prd.day <- function(
  inpList,
  ZoneTime,
  AngZaxsSoniInst
) {

# time

  # create temporary variable holding POSIXlt object
  tmp <- as.POSIXlt(inpList$time$UTC[])

  # fractional UTC time
  inpList$time$UTC_frac <- ff::as.ff(tmp$hour + tmp$min / 60 + tmp$sec / 3600)
  base::attr(x = inpList$time$UTC_frac, which = "unit") <- "h"

  # fractional day of year [DOY]
  inpList$time$DOY_frac <- ff::as.ff(tmp$yday + 1 + inpList$time$UTC_frac[] / 24)
  base::attr(x = inpList$time$DOY_frac, which = "unit") <- "d"
  
  # calculate local standard time
  tmp <- inpList$time$UTC[]
  attributes(tmp)$tzone <- ZoneTime
  inpList$time$Loca <- ff::as.ff(tmp)
  base::attr(x = inpList$time$Loca, which = "unit") <- "YYYY-MM-DD hh:mm:ss.sss"

  # clean up
  rm(tmp)
  
  #Additional QAQC tests and removing high frequency flagged data
  ###############################################################
  #Run the test to determine the irgaAgc flag
  inpList$qfqm$irga$qfIrgaAgc <- ff::as.ff(eddy4R.qaqc::def.qf.irga.agc(qfIrgaAgc = inpList$qfqm$irga$qfIrgaAgc))
  
  #Applying the bad quality flags to the reported output data
  inpList <- wrap.qf.rmv.data(inpList = inpList, Vrbs = FALSE)
  
  #Run the test to output Validation flag
  inpList$qfqm$irga$qfIrgaVali <- ff::as.ff(eddy4R.qaqc::def.qf.irga.vali(data = inpList$data$irgaMfcSamp))#Use this one for MFC set point
  
  #  inpList$qfqm$irga$qfIrgaVali <- eddy4R.qaqc::def.qf.irga.vali(data = inpList$data$irgaSndValiNema, Sens = "irgaSndValiNema")
  
  ###############################################################     

#irga

  # average signal strength
  inpList$data$irga$ssiMean <- ff::as.ff(def.ssi.mean(ssiCo2 = inpList$data$irga$ssiCo2, ssiH2o = inpList$data$irga$ssiH2o))

  # delta signal strength
  inpList$data$irga$ssiDiff <- def.ssi.diff(ssiCo2 = inpList$data$irga$ssiCo2, ssiH2o = inpList$data$irga$ssiH2o)
 
  # total pressure in irga cell
  inpList$data$irga$presSum <- def.pres.sum(presAtm = inpList$data$irga$presAtm, presDiff = inpList$data$irga$presDiff)

  # average temperature in irga cell 
  inpList$data$irga$tempMean <- def.temp.mean.7200(tempIn = inpList$data$irga$tempIn, tempOut = inpList$data$irga$tempOut)
 
  # RH in cell

    # water vapor partial pressure
    inpList$data$irga$presH2o <- ff::as.ff(def.pres.h2o.rtio.mole.h2o.dry.pres(rtioMoleDryH2o = inpList$data$irga$rtioMoleDryH2o, pres = inpList$data$irga$presSum))
    base::attr(x = inpList$data$irga$presH2o, which = "unit") <- "Pa"

    # water vapor saturation pressure
    if(!is.na(mean(inpList$data$irga$tempMean, na.rm=TRUE))) {
      inpList$data$irga$presH2oSat <- ff::as.ff(as.vector(def.pres.h2o.sat.temp.mag(temp = inpList$data$irga$tempMean[])))
    } else {
      inpList$data$irga$presH2oSat <- ff::as.ff(rep(NaN, length(inpList$data$irga$tempMean)))
    }
    base::attr(x = inpList$data$irga$presH2oSat, which = "unit") <- "Pa"
    
    # calcuate RH
    inpList$data$irga$rh <- def.rh.pres.h2o.pres.sat.h2o(presH2o = inpList$data$irga$presH2o, presH2oSat = inpList$data$irga$presH2oSat)
    

  # molar density of dry air and water vapor
  inpList$data$irga$densMoleAir <- def.dens.mole.air(presSum = inpList$data$irga$presSum, tempMean = inpList$data$irga$tempMean)
  
  # molar density of dry air alone
  inpList$data$irga$densMoleAirDry <- def.dens.mole.air.dry(densMoleAir = inpList$data$irga$densMoleAir, densMoleH2o = inpList$data$irga$densMoleH2o)

  # wet mass fraction (specific humidity)
  inpList$data$irga$rtioMassH2o <- def.rtio.mass.h2o.dens.mole(densMoleH2o = inpList$data$irga$densMoleH2o, densMoleAirDry = inpList$data$irga$densMoleAirDry)

# soni
  
  # correction for attitude and motion via AMRS
  
  #Convert the angle of installation to radians
  AngZaxsSoniInst <- eddy4R.base::def.unit.conv(data=AngZaxsSoniInst,unitFrom="deg",unitTo="rad")
  # rotate wind vector into meteorological coordinate system (positive from west, south and below)
  inpList$data$soni <- def.met.body(AngZaxsSoniInst = AngZaxsSoniInst, veloBody = inpList$data$soni)
  
  # magnitude of horizontal wind speed
  inpList$data$soni$veloXaxsYaxsErth <- sqrt(inpList$data$soni$veloXaxs^2 + inpList$data$soni$veloYaxs^2)
  # Assign unit to horizontal wind speed
  base::attr(x = inpList$data$soni$veloXaxsYaxsErth, which = "unit") <- "m s-1"
  
  # wind direction
  # need to redo for vector averaging, see REYNflux_P5.R line 139
  inpList$data$soni$angZaxsErth <- ff::as.ff((2*pi + atan2(-inpList$data$soni$veloYaxs[], -inpList$data$soni$veloXaxs[]))%%(2*pi))
  # Assign unit to wind direction
  base::attr(x = inpList$data$soni$angZaxsErth, which = "unit") <- "rad"

  # sonic temperature [K] from speed of sound [m s-1] (Campbell Scientific, Eq. (9))
  inpList$data$soni$tempSoni <- eddy4R.base::def.temp.soni(veloSoni = inpList$data$soni$veloSoni)
  
  
  
  # sort object levels alphabetically

  #inpList
  inpList <- inpList[names(inpList)[order(tolower(names(inpList)))]]
  
  # inpList$data
  inpList$data <- inpList$data[names(inpList$data)[order(tolower(names(inpList$data)))]]

  # inpList$data$irga
  inpList$data$irga <- inpList$data$irga[names(inpList$data$irga)[order(tolower(names(inpList$data$irga)))]]
  
  # inpList$data$irga MFC
  inpList$data$irgaMfcSamp <- inpList$data$irgaMfcSamp[names(inpList$data$irgaMfcSamp)[order(tolower(names(inpList$data$irgaMfcSamp)))]]
  
  # inpList$data$irga Solenoids
  inpList$data$irgaSndValiNema <- inpList$data$irgaSndValiNema[names(inpList$data$irgaSndValiNema)[order(tolower(names(inpList$data$irgaSndValiNema)))]]
  
  # inpList$data$soni
  inpList$data$soni <- inpList$data$soni[names(inpList$data$soni)[order(tolower(names(inpList$data$soni)))]]
  
  # inpList$data$soniAmrs
  inpList$data$soniAmrs <- inpList$data$soniAmrs[names(inpList$data$soniAmrs)[order(tolower(names(inpList$data$soniAmrs)))]]

# return results
return(inpList)
    
}
