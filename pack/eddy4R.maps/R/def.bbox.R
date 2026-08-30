##############################################################################################
#' @title definition function to create bounding box

#' @author 
#' David Durden
#' Stefan Metzger \email{smetzger@atmofacts.com}

#' @description Function definition. Read in shapefile or create a bounding box
#'  by adding an extent to Lat/Lon for grabbing STAC data with a padding option.

#' @param site the site to use to grab the bounding box
#' @param crs coordinate reference system (Defaults to "EPSG:4326")
#' @param pad_box logical to determine if padding should be applied to bounding box (defaults to FALSE)
#' @param pad_degree decimal degree of latitude and longitude to pad the bounding box (defaults to 0.1) 

#' @return list containing shapefile data and bbox

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
#' 
# changelog and author contributions
#   David Durden (2024-01-23)
#     original creation 
#   Stefan Metzger (2024-03-01)
#     workaround for native pipe operator to support both  R < 4.1 and R >= 4.1
##############################################################################################
def.bbox <- function(
    site = c("CPER", "CHEESEHEAD")[2], 
    crs = "EPSG:4326",
    pad_box = FALSE,
    pad_degree = 0.5,
    lat = NULL,
    lon = NULL
    ){
 
  #Initialize list
  out <- list()
  
  ## read shape file
  if(!is.null(site)) {
    if(site == "CHEESEHEAD"){
    
    out$shp <-eddy4R.maps::CHEESEHEAD
    }else{
    
    out$shp <- dplyr::filter(eddy4R.maps::NEON$siteID == site)
    }
      #sf::read_sf(dsn = DirInp, "mask")
    #generate bounding box
    # native pipe operator |>; supported from R >= 4.1
    # out$bbox <- out$shp |> sf::st_transform(crs=crs) |> sf::st_bbox()
    # workaround: sequentially chain functions to support both  R < 4.1 and R >= 4.1
    tmp01 <- sf::st_transform(out$shp, crs=crs)
    out$bbox <- sf::st_bbox(tmp01)
    base::rm(tmp01)
    
    #pad bounding box
    if(pad_box == TRUE){
      out$bbox[1] <- out$bbox$xmin - pad_degree #Padding in degrees
      out$bbox[2] <- out$bbox$ymin - pad_degree
      out$bbox[3] <- out$bbox$xmax + pad_degree
      out$bbox[4] <- out$bbox$ymax + pad_degree
    }
    
    #Create a mask layer
    # with pipe operator (R >= 4.1 only)
    # out$maskLayer <- sf::st_geometry(out$shp) |> sf::st_transform(crs=crs) 
    # w/o pipe operator

      # First, extract the geometry from out$shp
      tmp01 <- sf::st_geometry(out$shp)
      
      # Then, transform the geometry with the specified CRS
      out$maskLayer <- sf::st_transform(tmp01, crs=crs)
      
      # clean up
      base::rm(tmp01)
    
    
  } else 
  {
    out$bbox <- as(raster::extent(lon - pad_degree, lon + pad_degree, lat - pad_degree, lat + pad_degree), "SpatialPolygons")
    proj4string(out$bbox) <- crs
    }
  

  
  #Return output list with shp and bbox
  return(out)
}
