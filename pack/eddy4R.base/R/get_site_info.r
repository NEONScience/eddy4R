##############################################################################################
#' @title Load pre-defined site location info into global environment

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com}\cr
#' David Durden \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-10-02)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Dave Durden (2016-05-04)
#     Adding site information for SERC

#' @description Load pre-defined site location info into global environment.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

get_site_info <- function(
  location=c("IM", "LOS", "NR", "NS", "PF", "SERC")[6]
  ) {

  #allocate empty list
    site_info <- list()

  #get site info
    #Xilingol, Inner Mongolia, China
      if(location == "IM") {
      #UTC to local time difference (China standard time, CST)
        site_info$UTC2LT <- +8	
		site_info$tz <- "CST"
      #UTM zone for conversion lat / lon to UTM
        site_info$UTM <- data.frame(zone=50)
      }
    
    #Lindenberg, Brandenburg, Germany
      if(location == "LOS") {
      #UTC to local time difference (Central European summer time, CEST -> 26.10)
        site_info$UTC2LT <- +2
		site_info$tz <- "CEST"
      #UTM zone for conversion lat / lon to UTM
        site_info$UTM <- data.frame(zone=33)	
      }
    
    #Niwot Ridge, CO, U.S.A.
      if(location == "NR") {
        #UTC to local time difference (Mountain standard time, MST)
          site_info$UTC2LT <- -7
		  site_info$tz <- "MST"
        #UTC to local time difference (Mountain daylight saving time, MDT)
  #      UTC2LT <- -6  
        #coordinates in UTM [m]
          site_info$UTM <- data.frame(zone=13, easting=453382, northing=4431552)
        #height of tower base above sea level [m]
          site_info$d_z_base <- 3023
        #measurement height [m]
          site_info$d_z_m <- 21.5
        #displacement height 7.8+/-1.1[m]
          site_info$d_z_d <- 7.8
        #canopy height 11 - 13[m]
          site_info$d_z_c <- 12 
          #CSAT3 boom angle [degrees from true North]
          site_info$PSI_SONIC <- 204
        #planar fit coefficients [degree] [degree] [m s-1]        
          site_info$PF_coeff <- data.frame(alpha = 2.29, beta = 5.62, offset = -0.0185)
      }
    
    #Northslope, AK, U.S.A.
      if(location == "NS") {  
	  #UTC to local time difference (Alaska standard time)
	    site_info$UTC2LT <- -9
		site_info$tz <- "AKST"
      #UTC to local time difference (Alaska daylight time)
#       site_info$UTC2LT <- -8  
#		site_info$tz <- "AKDT"
      #UTM zone for conversion lat / lon to UTM
        site_info$UTM <- data.frame(zone=4)
      }
    
    #Park Falls, Wi, U.S.A.
      if(location=="PF"){
      #UTC to local time difference (Central standard time)
        site_info$UTC2LT <- -6
		site_info$tz <- "CST"
      #coordinates in UTM zone [m]
        site_info$UTM <- data.frame(zone=15, easting=711415, northing=5091655) 
      #height of tower base above sea level [m]
        site_info$d_z_tower_base <- 470 
      }
    
    #Smithsonian Environmental Research Center, MD, U.S.A.
    if(location == "SERC") {
        #UTC to local time difference (Eastern standard time, EST)
        site_info$UTC2LT <- -5
        site_info$tz <- "EST"
        #UTC to local time difference (Eastern daylight saving time, EDT)
        #      UTC2LT <- -4  
        #coordinates in UTM [m]
        site_info$UTM <- data.frame(zone=18, easting=364703.4, northing=4305740.9)
        #height of tower base above sea level [m]
        site_info$d_z_base <- 33.23
        #measurement height [m]
        site_info$d_z_m <- 61.94
        #displacement height 7.8+/-1.1[m]
        site_info$d_z_d <- 32
        #canopy height 11 - 13[m]
        site_info$d_z_c <- 38
        #CSAT3 boom angle [degrees from true North]
        site_info$PSI_SONIC <- 230
        #planar fit coefficients (April 22 to May 3, 2016) [degree] [degree] [m s-1]
        site_info$PF_coeff <- data.frame(alpha = -0.7056, beta = -1.8718, offset = 0.0549)
    }

 #return site info
  return(site_info)
  
}
