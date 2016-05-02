##############################################################################################
#' @title Coordinate transformation from CSAT3 body coordinate system to meteorological coordinate system

#' @author Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' @author Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function defintion. Coordinate transformation from CSAT3 body coordinate system to meteorological coordiante system.

#' @param \code{AzSoniInst}  Parameter of class numeric. Azimuth direction against true north in which sonic anemometer installation (transducer array) is pointing [rad]
#' @param \code{AzSoniOfst} Parameter of class integer or numeric.  Azimuth Offset of meteorological x-axis against true north. That is, angle by which sonic data has to be clockwise azimuth-rotated when sonic anemometer body x-axis points perfectly north (az_body = 0) [rad]



#' @return Currently none

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. 

#' @keywords coordinate, sonic anemometer, transformation

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2013-06-27)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Hongyan Luo (2016-05-02)
#     adjust to eddy4R terms
##############################################################################################

def.conv.body.met <- function(
    AzSoniInst,
    AzSoniOfst =  eddy4R.base::def.conv.unit(data=90,unitFrom="deg",unitTo="rad")$dataConv[[1]],
  #data.frame with wind speeds u, v, w in BCS
    veloBody = data.frame(xaxs=0, yaxs=0, zaxs=0)
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
    az_body <- AzSoniInst - pi
    if(az_body < 0)  az_body <- az_body + 2 * pi  

  #resulting clockwise azimuth rotation angle from BCS to MCS  [radians]
    az_B2M <- AzSoniOfst - az_body
    if(az_B2M < 0)  az_B2M <- az_B2M + 2 * pi

  #prepare data.frame for output
    MCS_uvw <- veloBody
    MCS_uvw$u <- NA
    MCS_uvw$v <- NA

  #perform actual rotation
    MCS_uvw$u <- veloBody$xaxs * cos(az_B2M) - veloBody$yaxs * sin(az_B2M)  
    MCS_uvw$v <- veloBody$xaxs * sin(az_B2M) + veloBody$yaxs * cos(az_B2M)

  #return results
    return(MCS_uvw)
    
}


# #test example
# test <- def.conv.body.met(
#   #"boom angle" of sonic
#   #direction against true north in which the transducer array is pointing [radians]
#     AzSoniInst = eddy4R.base::def.conv.unit(data=189.28,unitFrom="deg",unitTo="rad")$dataConv[[1]],
#   #Offset of MCS x-axis against north
#   #That is, angle by which sonic data has to be clockwise azimuth-rotated 
#   #when BCS x-axis points perfectly north (az_body == 0) [radians]
#     AzSoniOfst = eddy4R.base::def.conv.unit(data=90,unitFrom="deg",unitTo="rad")$dataConv[[1]],
#   #data.frame with wind speeds u, v, w in BCS
#     veloBody = data.frame(u=ns.data$fst_u, v=ns.data$fst_v, w=ns.data$fst_w)
# )
