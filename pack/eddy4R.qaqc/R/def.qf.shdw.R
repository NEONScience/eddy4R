##############################################################################################
#' @title Dummy Definition function: Tower shadowing flags for CSAT3

#' @author 
#' Sreenath Paleri \email{paleri@battelleecology.org}

#' @description 
#' Definition function to diagnose shadow effects due to tower structure on the sonic anemometer data
#' @param diag16 The 16-bit diagnostic stream that is output from the Soni (CSAT3). 

#' @return A dataframe (\code{qfSoni}) of sensor specific AMRS quality flags as described in NEON.DOC.000807.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0' data product conversions and calculations (NEON.DOC.000807) \cr
#' Campbell Scientific CSAT3 reference manual

#' @keywords NEON, soni, sonic anemometer, CSAT3, qfqm

#' @examples 
#' diag16 <- as.integer(rep(135, 36000))

#' set <- runif(20,1, 36000) # inserting error positions for other flags
#' diag16[set] <- as.integer(c(32768,16384,8192,4096,61442, 61441,61440,61503, -99999, NaN)) # filling with numbers that would indicate flags soni flags
#' 
#' eddy4R.qaqc::def.qf.soni(diag16 = diag16)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Sreenath Paleri (2025-07-02)
#     original creation
##############################################################################################
def.qf.shdw <- function (
mean_angle,
site
) {
  
  # Create a dataframe matching the provided data
  
  # Create a dataframe matching the provided data
  
  tower_shadow_angles <- data.frame(
    SiteID = c("BART","HARV","SERC","BLAN","SCBI","JERC","DSNY","OSBS","GUAN","LAJA",
               "UNDE","STEI","TREE","UKFS","KONZ","KONA","MLBS","GRSM","ORNL","DELA",
               "TALL","LENO","NOGP","DCFS","WOOD","CPER","STER","RMNP","OAES","CLBJ",
               "YELL","MOAB","NIWO","SRER","JORN","ONAQ","ABBY","WREF","SOAP","SJER",
               "TEAK","BARR","TOOL","BONA","HEAL","DEJU","PUUM"),
    min_angle = c(100.0,100.0,60.0,70.0,130.0,205.0,250.0,295.0,250.0,250.0,
                  30.0,55.0,55.0,60.0,120.0,123.0,160.0,210.0,341.0,20.0,
                  70.0,145.0,50.0,75.0,75.0,100.0,100.0,133.0,250.0,100.0,
                  10.0,339.5,55.0,360.0,50.0,10.0,340.0,55.0,160.0,250.0,
                  100.0,325.0,100.0,336.7,340.0,360.0,18.0),
    max_angle = c(130.0,130.0,90.0,100.0,160.0,235.0,280.0,325.0,280.0,280.0,
                  60.0,85.0,85.0,90.0,150.0,153.0,190.0,240.0,11.0,50.0,
                  100.0,175.0,80.0,105.0,105.0,130.0,130.0,163.0,280.0,130.0,
                  40.0,9.5,85.0,30.0,80.0,40.0,10.0,85.0,190.0,280.0,
                  130.0,355.0,130.0,6.7,10.0,30.0,48.0)
  )
  
  #convert input wind angle from radians to degrees
  mean_angle <- mean_angle*(180.)/pi
  
  #here, from the x and y velocities, it looks like the angle is cartesian and 
  #needs to be converted to degrees north, can comment out the following section if that is not needed anymore
  
  #convert from cartesian degrees to degrees north
  #right now, start by checking which (cartesian) quadrant the wind angle is in
  
  #cartesian quadrants go 1,2,3,4, counterclockwise
  
  if(mean_angle >= 0 & mean_angle < 90){quadrant <- 1
  } else if(mean_angle >= 90 & mean_angle < 180){quadrant <- 2
  } else if(mean_angle >= 180 & mean_angle < 270){quadrant <- 3
  } else if(mean_angle >= 270 & mean_angle < 360){quadrant <- 4
  } else if(mean_angle==360){quadrant <- 1
  } else(print("Error! check mean wind angles. Not in range"))
  
  
  if(quadrant==1){mean_angle = 90-mean_angle
  } else if(quadrant==2){mean_angle = 270 + (180 - mean_angle)
  } else if(quadrant==3){mean_angle = 180 + (270 - mean_angle)
  } else if(quadrant==4){mean_angle = 90 + (360 - mean_angle)
  }
  
  #now check shadowing for the current site
  
  #pull in the shadow angles
  min_angle <- tower_shadow_angles[tower_shadow_angles$SiteID==site,]$min_angle
  max_angle <- tower_shadow_angles[tower_shadow_angles$SiteID==site,]$max_angle
  
  #add a flag for data within the shadow zone = 1, outside of it = 0
  
  #consider cases when the shadow zone crosses through zero degree north
  #then set flag based on mean wind angle and shadow angles
  if (min_angle < max_angle){
    if(mean_angle >= min_angle & mean_angle <= max_angle){out <- 1}
    else{out <- 0}
  }
  else{
    if(mean_angle >= min_angle | mean_angle <= max_angle){out <- 1}
    else{out <- 0}
  }
  
  return(out)
}
  