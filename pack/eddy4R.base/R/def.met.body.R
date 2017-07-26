##############################################################################################
#' @title Definition function: Coordinate transformation from CSAT3 body coordinate system to meteorological coordinate system

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Hongyan Luo \email{hluo@battelleecology.org} \cr
#' David Durden \email{ddurden@battelleecology.org}

#' @description Function defintion. Coordinate transformation from CSAT3 body coordinate system to meteorological coordiante system.

#' @param AngZaxsSoniInst  Parameter of class numeric. Azimuth (angle around z axis) direction against true north in which sonic anemometer installation (transducer array) is pointing [rad]
#' @param AngZaxsSoniOfst Parameter of class integer or numeric.  Azimuth Offset of meteorological x-axis against true north. That is, angle by which sonic data has to be clockwise azimuth-rotated when sonic anemometer body x-axis points perfectly north (azSonic = 0) [rad]
#' @param veloBody  Variable of class numeric. Data frame containing wind speeds along x-axis (veloXaxs), y-axis (veloYaxs),and z-axis (veloZaxs) in sonic anemometer body coordinate system. For example: \code{veloBody <- data.frame(veloXaxs=rnorm(20), veloYaxs=rnorm(20), veloZaxs=rnorm(20))} [m s-1]


#' @return Wind speed in meteorological coordinate system [m s-1]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. 

#' @keywords coordinate, sonic anemometer, transformation

#' @examples 
#' veloIn01 <- data.frame(veloXaxs=rnorm(20), veloYaxs=rnorm(20), veloZaxs=rnorm(20)) 
#' def.met.body(AzSoniInst=60, veloBody=veloIn01)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2013-06-27)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Hongyan Luo (2016-05-02)
#     adjust to eddy4R terms
#   Natchaya P-Durden (2016-11-26)
#     rename function to def.met.body()
#   David Durden (2017-06-10)
#    Adapt function to eddy4R terms
##############################################################################################

def.met.body <- function(
    AngZaxsSoniInst,
    AngZaxsSoniOfst =  eddy4R.base::def.unit.conv(data=90,unitFrom="deg",unitTo="rad"),
    veloBody 
  ) {

#body coordinate system (BCS)
#geodetic coordinate system (GCS)
#meteorological coordiante system (MCS)

  #Signal   CSAT3 BCS           standard BCS        GCS                 MCS
  #u+       wind from front     wind from front     wind from south     wind from west
  #v+       wind from left      wind from right     wind from west      wind from south
  #w+       wind from below     wind from above     wind from above     wind from below

  #draw axes arrows in direction into which wind is blowing!

#direct transformation from CSAT3 BCS into MCS
  #-> same order of axes, no permutation required
  #-> axes permutation from arbitrary BCS to CSAT3 BCS could be added as a pre-step;
  #-> simple azimuth rotation
  
  
  #determine "body angle" of the sonic
  #the direction against true north in which the sonic x-axis is pointing [radians]
  AngZaxsSoni <- AngZaxsSoniInst - pi
    if(AngZaxsSoni < 0)  AngZaxsSoni <- AngZaxsSoni + 2 * pi  

  #resulting clockwise azimuth rotation angle from BCS to MCS  [radians]
    AngZaxsMet <- AngZaxsSoniOfst - AngZaxsSoni
    if(AngZaxsMet < 0)  AngZaxsMet <- AngZaxsMet + 2 * pi

  #prepare data.frame for output
    veloMet <- veloBody
   # veloMet$veloXaxs <- NA
  #  veloMet$veloYaxs <- NA

  #perform actual rotation
    veloMet$veloXaxs <- veloBody$veloXaxs * cos(AngZaxsMet) - veloBody$veloYaxs * sin(AngZaxsMet)  
    veloMet$veloYaxs <- veloBody$veloXaxs * sin(AngZaxsMet) + veloBody$veloYaxs * cos(AngZaxsMet)

  #return results
    return(veloMet)
    
}



# #test example
#veloIn01 <- data.frame(xaxs=rnorm(20), yaxs=rnorm(20), zaxs=rnorm(20))
#veloIn02 <- veloIn01
#dimnames(veloIn02)[[2]] <- c("u", "v", "w")

##test new function
#def.met.body(AzSoniInst=60, veloBody=veloIn01)

##test old function
#rot_B2M(Psi_boom = 60, BCS_uvw = veloIn02)


