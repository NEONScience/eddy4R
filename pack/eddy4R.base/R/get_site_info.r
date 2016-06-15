##############################################################################################
#' @title Load pre-defined site location info into global environment

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}\n
#' Dave Durden

#' @description Load pre-defined site location info into global environment.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-10-02)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Dave Durden (2016-02-11)
#     Applying standard style following Wiki
#   Dave Durden (2016-05-04)
#     Adding site information for SERC
##############################################################################################


def.get.site.info <- function(
  #location
  loc=c("IM", "LOS", "NR", "NS", "PF" , "SERC")[6]
) {
  
  #allocate empty list for site information
  siteInfo <- list()
  
  #get site info
  #Xilingol, Inner Mongolia, China
  if(loc == "IM") {
    #UTC to local time difference (China standard time, CST)
    siteInfo$diffUtcLt <- +8	
    siteInfo$tz <- "CST"
    #UTM zone for conversion lat / lon to UTM
    siteInfo$utm <- data.frame(zone=50)
  }
  
  #Lindenberg, Brandenburg, Germany
  if(loc == "LOS") {
    #UTC to local time difference (Central European summer time, CEST -> 26.10)
    siteInfo$diffUtcLt <- +2
    siteInfo$tz <- "CEST"
    #UTM zone for conversion lat / lon to UTM
    siteInfo$utm <- data.frame(zone=33)	
  }
  
  #Niwot Ridge, CO, U.S.A.
  if(loc == "NR") {
    #UTC to local time difference (Mountain standard time, MST)
    siteInfo$diffUtcLt <- -7
    siteInfo$tz <- "MST"
    #UTC to local time difference (Mountain daylight saving time, MDT)
    #      UTC2LT <- -6  
    #coordinates in UTM [m]
    siteInfo$utm <- data.frame(zone=13, estg=453382, nthg=4431552)
    #height of tower base above sea level [m]
    siteInfo$towElevAsl <- 3023
    #measurement height [m]
    siteInfo$distZaxsMeasHght <- 21.5
    #displacement height 7.8+/-1.1[m]
    site_info$distZaxsDispHght <- 7.8
    #canopy height 11 - 13[m]
    siteInfo$distZaxsCnpyHght <- 12 
    #CSAT3 boom angle [degrees from true North]
    siteInfo$azSoni <- 204
    #planar fit coefficients (pfCoef) [degree] [degree] [m s-1] alpha (coefPtchAng), beta (CoefRollAng),
    #and the regression offset (coefRegOfst)       
    siteInfo$pfCoef <- data.frame(coefPtchAng = 2.29, coefRollAng = 5.62, coefRegOfst = -0.0185)
  }
  
  #Northslope, AK, U.S.A.
  if(loc == "NS") {  
    #UTC to local time difference (Alaska standard time)
    siteInfo$diffUtcLt <- -9
    siteInfo$tz <- "AKST"
    #UTC to local time difference (Alaska daylight time)
    #       site_info$UTC2LT <- -8  
    #		site_info$tz <- "AKDT"
    #UTM zone for conversion lat / lon to UTM
    siteInfo$utm<- data.frame(zone=4)
  }
  
  #Park Falls, Wi, U.S.A.
  if(loc == "PF"){
    #UTC to local time difference (Central standard time)
    siteInfo$diffUtcLt <- -6
    siteInfo$tz <- "CST"
    #coordinates in UTM zone [m]
    siteInfo$utm <- data.frame(zone=15, estg=711415, nthg=5091655) 
    #height of tower base above sea level [m]
    siteInfo$towElevAsl <- 470 
  }
  
  #Smithsonian Environmental Research Center, MD, U.S.A.
  if(location == "SERC") {
    #UTC to local time difference (Eastern standard time, EST)
    siteInfo$diffUtcLt <- -5
    siteInfo$tz <- "EST"
    #UTC to local time difference (Eastern daylight saving time, EDT)
    #      UTC2LT <- -4  
    #coordinates in UTM [m]
    siteInfo$utm <- data.frame(zone = 18, estg = 364703.4, nthg = 4305740.9)
    #height of tower base above sea level [m]
    siteInfo$towElevAsl  <- 33.23
    #measurement height [m]
    siteInfo$distZaxsMeasHght <- 61.94
    #displacement height 7.8+/-1.1[m]
    site_info$distZaxsDispHght <- 32
    #canopy height 11 - 13[m]
    siteInfo$distZaxsCnpyHght <- 38
    #CSAT3 boom angle [degrees from true North]
    siteInfo$azSoni <- 230
    #planar fit coefficients (April 22 to May 3, 2016) [degree] [degree] [m s-1]
    siteInfo$pfCoef <- data.frame(coefPtchAng = -0.7056, coefRollAng = -1.8718, coefRegOfst = 0.0549)
  }
  #return site info
  return(siteInfo)
  
}
