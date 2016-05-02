##############################################################################################
#' @title Coordinate transformation from CSAT3 body coordinate system to meteorological coordinate system

#' @author Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' @author Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function defintion. Coordinate transformation from CSAT3 body coordinate system to meteorological coordiante system.

#' @param Currently none

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
  #"boom angle" of sonic
  #direction against true north in which the transducer array is pointing [radians]
    az_boom = eddy4R.base::def.conv.unit(data=189.28,unitFrom="deg",unitTo="rad")$dataConv[[1]],
  #Offset of MCS x-axis against north
  #That is, angle by which sonic data has to be clockwise azimuth-rotated 
  #when BCS x-axis points perfectly north (az_body == 0) [radians]
    az_B2M_0 =  eddy4R.base::def.conv.unit(data=90,unitFrom="deg",unitTo="rad")$dataConv[[1]],
  #data.frame with wind speeds u, v, w in BCS
    BCS_uvw = data.frame(u=0, v=0, w=0)
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
    az_body <- az_boom - pi
    if(az_body < 0)  az_body <- az_body + 2 * pi  

  #resulting clockwise azimuth rotation angle from BCS to MCS  [radians]
    az_B2M <- az_B2M_0 - az_body
    if(az_B2M < 0)  az_B2M <- az_B2M + 2 * pi

  #prepare data.frame for output
    MCS_uvw <- BCS_uvw
    MCS_uvw$u <- NA
    MCS_uvw$v <- NA

  #perform actual rotation
    MCS_uvw$u <- BCS_uvw$u * cos(az_B2M) - BCS_uvw$v * sin(az_B2M)  
    MCS_uvw$v <- BCS_uvw$u * sin(az_B2M) + BCS_uvw$v * cos(az_B2M)

  #return results
    return(MCS_uvw)
    
}


# #test example
# test <- def.conv.body.met(
#   #"boom angle" of sonic
#   #direction against true north in which the transducer array is pointing [radians]
#     az_boom = eddy4R.base::def.conv.unit(data=189.28,unitFrom="deg",unitTo="rad")$dataConv[[1]],
#   #Offset of MCS x-axis against north
#   #That is, angle by which sonic data has to be clockwise azimuth-rotated 
#   #when BCS x-axis points perfectly north (az_body == 0) [radians]
#     az_B2M_0 = eddy4R.base::def.conv.unit(data=90,unitFrom="deg",unitTo="rad")$dataConv[[1]],
#   #data.frame with wind speeds u, v, w in BCS
#     BCS_uvw = data.frame(u=ns.data$fst_u, v=ns.data$fst_v, w=ns.data$fst_w)
# )
