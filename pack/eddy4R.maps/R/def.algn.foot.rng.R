#' @title Align Footprint Map with Remote Sensing Data
#'
#' @author
#' Stefan Metzger \email{smetzger@atmofacts.com}
#' Andrei Serafimovich
#' 
#' @description Function definition. This function adjusts the dimensions of a footprint map
#' (PHIcr_pad) to match and align with the dimensions of remote sensing (RS) data. It handles both cases
#' where the footprint map is smaller or larger than the RS data extent in all directions.
#'
#' @param PHIcr_pad Matrix representing the footprint map.
#' @param nortF Numeric vector representing the north extent of the footprint.
#' @param eastF Numeric vector representing the east extent of the footprint.
#' @param RS List containing 'nort' and 'east' vectors for remote sensing data extents.
#' 
#' @return List containing the adjusted footprint map matrix and the updated extents (nortF and eastF).
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#' 
#' @keywords visualization, flux mapping
#' 
#' @importFrom matlab padarray
#' 
#' @examples
#' # Example usage:
#' # result <- alignFootprintMap(PHIcr_pad, nortF, eastF, RS)
#' # aligned_PHIcr_pad <- result$PHIcr_pad
#' # updated_nortF <- result$nortF
#' # updated_eastF <- result$eastF
#' 
#' @seealso Currently none.
#' 
#' @export
#
# changelog and author contributions / copyrights
#   Stefan Metzger (2013-01-01) original creation
#   Andrei Serafimovich (2015-06-15) fixed padding for footprint matrices
#   Stefan Metzger (2023-01-31) add Roxygen header, AGPL3 publication
# 
###############################################################################################

def.algn.foot.rng <- function(
  PHIcr_pad,
  nortF,
  eastF,
  RS) {
  
  # Pad or trim the footprint matrix to get same dimension as and alignment with RS data

  # Extent in south
  if(min(nortF, na.rm=TRUE) > 1) {
    #case1: footprint matrix smaller than RS extent
    PHIcr_pad <- matlab::padarray(PHIcr_pad, range(nortF)[1]-1,0,"post")
    nortF <- c(1:(range(nortF)[1]-1), nortF)
  } else if (min(nortF, na.rm=TRUE) < 1) {
    #case2: footprint matrix larger than RS extent
    #            PHIcr_pad <- PHIcr[-which(nortF < 0),]
    PHIcr_pad <- PHIcr_pad[-which(nortF <= 0),]
    #            nortF <- nortF[-which(nortF < 0)]
    nortF <- nortF[-which(nortF <= 0)]
  }
  
  # Extent in north
  if( max(nortF, na.rm=TRUE) < length(RS$nort) ) {
    #case1: footprint matrix smaller than RS extent
    PHIcr_pad <- matlab::padarray(PHIcr_pad,length(RS$nort)-range(nortF)[2],0,"pre")
    nortF <- c(nortF, (range(nortF)[2]+1):length(RS$nort))
  } else if ( max(nortF, na.rm=TRUE) > length(RS$nort) ) {
    #case2: footprint matrix larger than RS extent
    PHIcr_pad <- PHIcr_pad[-which(nortF > length(RS$nort)),]
    nortF <- nortF[-which(nortF > length(RS$nort))]
  }
  
  # Extent in west
  if(min(eastF, na.rm=TRUE) > 1) {
    #case1: footprint matrix smaller than RS extent
    PHIcr_pad <- cbind(matrix(nrow=nrow(PHIcr_pad), ncol=range(eastF)[1]-1, 0), PHIcr_pad)
    eastF <- c(1:(range(eastF)[1]-1), eastF)
  } else if (min(eastF, na.rm=TRUE) < 1) {
    #case2: footprint matrix larger than RS extent
    #              PHIcr_pad <- PHIcr_pad[,-which(eastF < 0)]
    PHIcr_pad <- PHIcr_pad[,-which(eastF <= 0)]
    #              eastF <- eastF[-which(eastF < 0)]
    eastF <- eastF[-which(eastF <= 0)]
  }
  
  # Extent in east
  if( max(eastF, na.rm=TRUE) < length(RS$east) ) {
    #case1: footprint matrix smaller than RS extent
    PHIcr_pad <- cbind(PHIcr_pad, matrix(nrow=nrow(PHIcr_pad),
                                         ncol=length(RS$east)-range(eastF)[2], 0))
    eastF <- c(eastF, (range(eastF)[2]+1):length(RS$east))
  } else if (max(eastF, na.rm=TRUE) > length(RS$east)) {
    #case2: footprint matrix larger than RS extent
    PHIcr_pad <- PHIcr_pad[,-which(eastF > length(RS$east))]
    #              eastF <- eastF[-which(nortF > length(RS$east))]
    eastF <- eastF[-which(eastF > length(RS$east))]
  }

  # Return the adjusted matrix and extents
  return(list(PHIcr_pad = PHIcr_pad, nortF = nortF, eastF = eastF))
}
