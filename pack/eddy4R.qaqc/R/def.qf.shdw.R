##############################################################################################
#' @title Definition function: Tower shadowing flags for CSAT3

#' @author 
#' Sreenath Paleri \email{paleri@battelleecology.org}

#' @description 
#' Definition function to diagnose shadow effects due to tower structure on the sonic anemometer data
#' @param angWindMean The mean wind direction from the Soni (AngZaxsErth) in radians. 
#' @param Site The NEON 4-letter site code.
#' 
#' @return A dataframe (\code{qfShdw}) of sonic anemometer qfShdw quality flag.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0' data product conversions and calculations (NEON.DOC.000807) \cr
#' Campbell Scientific CSAT3 reference manual

#' @keywords NEON, soni, sonic anemometer, CSAT3, qfqm, tower shadow

#' @examples Currently none


#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Sreenath Paleri (2025-07-02)
#     original creation
#   David Durden (2025-08-02)
#     updating terms and documentation
##############################################################################################
def.qf.shdw <- function (
angWindMean,
Site,
Crs = c("NED","ENU")[1]
) {

  
  # Create a dataframe matching the provided data
dfAngShdw <- data.frame(
    SiteID = c("BART","HARV","SERC","BLAN","SCBI","JERC","DSNY","OSBS","GUAN","LAJA",
               "UNDE","STEI","TREE","UKFS","KONZ","KONA","MLBS","GRSM","ORNL","DELA",
               "TALL","LENO","NOGP","DCFS","WOOD","CPER","STER","RMNP","OAES","CLBJ",
               "YELL","MOAB","NIWO","SRER","JORN","ONAQ","ABBY","WREF","SOAP","SJER",
               "TEAK","BARR","TOOL","BONA","HEAL","DEJU","PUUM"),
    ThshAngMin = c(100.0,100.0,60.0,70.0,130.0,205.0,250.0,295.0,250.0,250.0,
                  30.0,55.0,55.0,60.0,120.0,123.0,160.0,210.0,341.0,20.0,
                  70.0,145.0,50.0,75.0,75.0,100.0,100.0,133.0,250.0,100.0,
                  10.0,339.5,55.0,360.0,50.0,10.0,340.0,55.0,160.0,250.0,
                  100.0,325.0,100.0,336.7,340.0,360.0,18.0),
    ThshAngMax = c(130.0,130.0,90.0,100.0,160.0,235.0,280.0,325.0,280.0,280.0,
                  60.0,85.0,85.0,90.0,150.0,153.0,190.0,240.0,11.0,50.0,
                  100.0,175.0,80.0,105.0,105.0,130.0,130.0,163.0,280.0,130.0,
                  40.0,9.5,85.0,30.0,80.0,40.0,10.0,85.0,190.0,280.0,
                  130.0,355.0,130.0,6.7,10.0,30.0,48.0)
  )
  
  #convert input wind angle from radians to degrees
  angWindMean <- eddy4R.base::def.unit.conv(data = angWindMean, unitFrom = attributes(angWindMean)$unit, unitTo = "deg", MethGc = FALSE)
  
  #here, from the x and y velocities, it looks like the angle is cartesian and 
  #needs to be converted to degrees north, can comment out the following section if that is not needed anymore
  
  #convert from cartesian degrees to degrees north
  #right now, start by checking which (cartesian) quadrant the wind angle is in
  
  #cartesian quadrants go 1,2,3,4, counterclockwise
  
  #if statement for coordinate references system (crs) check
  if(Crs == "ENU"){
    
  angWindMean <- eddy4R.base::def.rot.enu.ned(angEnu = angWindMean, unitOut = "deg")  
#  if(angWindMean >= 0 & angWindMean < 90){quad <- 1
#  } else if(angWindMean >= 90 & angWindMean < 180){quad <- 2
#  } else if(angWindMean >= 180 & angWindMean < 270){quad <- 3
#  } else if(angWindMean >= 270 & angWindMean < 360){quad <- 4
#  } else if(angWindMean==360){quad <- 1
#  } else(print("Error! check mean wind angles. Not in range"))
  
  
#  if(quad==1){angWindMean = 90-angWindMean
#  } else if(quad==2){angWindMean = 270 + (180 - angWindMean)
#  } else if(quad==3){angWindMean = 180 + (270 - angWindMean)
#  } else if(quad==4){angWindMean = 90 + (360 - angWindMean)
#  }
  }#End if statement for coordinate references system (crs) check
  
  #now check shadowing for the current site
  
  #pull in the shadow angles
  ThshAngMin <- dfAngShdw[dfAngShdw$SiteID==Site,]$ThshAngMin
  ThshAngMax <- dfAngShdw[dfAngShdw$SiteID==Site,]$ThshAngMax
  
  #add a flag for data within the shadow zone = 1, outside of it = 0
  
  #consider cases when the shadow zone crosses through zero degree north
  #then set flag based on mean wind angle and shadow angles
  if (ThshAngMin < ThshAngMax){
    out <- as.integer(ifelse(angWindMean >= ThshAngMin & angWindMean <= ThshAngMax, 1, 0))
     #if(angWindMean >= ThshAngMin & angWindMean <= ThshAngMax){out <- 1}
    #else{out <- 0}
  }else{
    out <- as.integer(ifelse(angWindMean >= ThshAngMin | angWindMean <= ThshAngMax, 1, 0))
    #if(angWindMean >= ThshAngMin | angWindMean <= ThshAngMax){out <- 1}
    #else{out <- 0}
  }
  
  #Set unit attribute
  base::attr(out, "unit") <- "NA"
  
  #Return output flag
  return(out)
}
  