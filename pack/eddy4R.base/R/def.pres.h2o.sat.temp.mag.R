##############################################################################################
#' @title Calculate saturated water vapor pressure from temperature using Magnus equation

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya P-Durden
 
#' @description Calculate saturated water vapor pressure from temperature using Magnus equation.

#' @param \code{temp} Either a vector or an object of class numeric of measured air temperature. [K]

#' @return Saturated water vapor pressure and of the same length as \code{temp}. [Pa]

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16 \cr
#' Gueymard, C.: Assessment of the accuracy and computing speed of simplified saturation vapor equations using a new reference dataset, J. Appl. Meteorol., 32, 1294-1300, doi:10.1175/1520-0450(1993)0321294:aotaac2.0.co;2, 1993. \cr

#' @keywords humidity conversion, Magnus equation

#' @examples
#' def.pres.h2o.sat.temp.mag(temp = 265.15)
#' def.pres.h2o.sat.temp.mag(temp = c(265.15, 285.0, 290.2))

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-11)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-04-08)
#     Initail naming convention for eddy4R
#   Natchaya P-Durden (2015-11-30)
#     rename function to def.pres.h2o.sat.temp.mag()
##############################################################################################

def.pres.h2o.sat.temp.mag <- function(temp) {

  #defined local constants for Magnus formula
  CnstLoc <- list(Cnst01 = 6.11, Cnst02 = 17.08, Cnst03 = 234.18, Cnst04 = 22.44, Cnst05 = 272.44)
 
  #mean temperature
    tempMean <- mean(temp, na.rm=TRUE)

    if(!is.na(tempMean)) {
    
        if (tempMean >= eddy4R.base::IntlConv$CelsKelv[1]) {
          #temp >= 273.15 K (0 degC):
          #Saturation water vapor pressure:
          presH2oSat <- (CnstLoc$Cnst01 * exp((CnstLoc$Cnst02 * eddy4R.base::def.unit.conv(data=temp,unitFrom="K",unitTo="C")) / (CnstLoc$Cnst03 + eddy4R.base::def.unit.conv(data=temp,unitFrom="K",unitTo="C"))))*100

          } else {
            #temp < 273.15 K (0 degC):
          presH2oSat <- (CnstLoc$Cnst01 * exp((CnstLoc$Cnst04 * eddy4R.base::def.unit.conv(data=temp,unitFrom="K",unitTo="C")) / (CnstLoc$Cnst05 + eddy4R.base::def.unit.conv(data=temp,unitFrom="K",unitTo="C"))))*100
          }
    
    
    } else {
      
      presH2oSat <- rep(NA, length(temp))
      
    }

  #return reported object
    return(presH2oSat) 
  
  # end function def.pres.h2o.sat.temp.mag()
}
