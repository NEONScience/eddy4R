##############################################################################################
#' @title definition function to download data from Microsoft planetary computer

#' @author 
#' David Durden
#' Stefan Metzger \email{smetzger@atmofacts.com}

#' @description Function definition. Download data from Microsoft planetary computer

#' @param start_date start date as character format yyyy-mm-dd
#' @param end_date end date as character format yyyy-mm-dd
#' @param box numberic vector in the format of (xmin, ymin, xmax, ymax)
#' @param collection name of planetary collection
#' @param asset_name mame of asset 
#' @param srs target spatial reference system as a string; can be a proj4 definition, WKT, or in the form "EPSG:XXXX"
#' @param dx size of pixels in x-direction (longitude / easting)
#' @param dy size of pixels in y-direction (latitude / northing)
#' @param dt size of pixels in time-direction, expressed as ISO8601 period string (only 1 number and unit is allowed) such as "P16D"
#' @param aggregation aggregation method as string, defining how to deal with pixels containing data from multiple images, can be "min", "max", "mean", "median", or "first"
#' @param resampling resampling method used in gdalwarp when images are read, can be "near", "bilinear", "bicubic" or others as supported by gdalwarp (see https://gdal.org/programs/gdalwarp.html)

#' @return A data cube proxy object

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords Currently none

#' @examples 
#' ingest_planetary_data(start_date = "2022-01-01", end_date = "2023-07-01", box =  c("xmin" = -123, "ymin" = 39, "xmax" = -122, "ymax" = 40))

#' @seealso Currently none

#' @export
#' 
# changelog and author contributions
#   David Durden (2024-01-23)
#     original creation 
#   Stefan Metzger (2024-03-01)
#     complete Roxygen header
#     workaround for native pipe operator to support both  R < 4.1 and R >= 4.1
##############################################################################################
def.spat.data.cube <- function(start_date,
                                  end_date,
                                  box,
                                  collection = c("modis-15A2H-061", "sentinel-3-slstr-lst-l2-netcdf")[2],
                                  asset_name = c("Lai_500m", "lst-in")[2],
                                  srs = "EPSG:4326",
                                  dx = 0.1, 
                                  dy = 0.1, 
                                  dt = "P1M",
                                  aggregation = "mean",
                                  resampling = "near"){
  
  # check box
  assertthat::are_equal(length(box), 4)
  
  # get STACItemCollection
    # Original pipe-based code (R > 4.1 only)
    #   matches <-
    # rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") |>
    # rstac::stac_search(collections = collection,
    #                    datetime = paste(start_date, end_date, sep = "/"),
    #                    bbox = c(box)) |>
    # get_request() |>
    # items_fetch() |>
    # items_sign(sign_fn = sign_planetary_computer())

    # workaround without pipe statements and using temporary objects:
      # Step 1: Initialize STAC client
        tmp01 <- rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1")
        
      # Step 2: Search STAC
        tmp02 <- stac_search(tmp01, collections = collection,
                             datetime = paste(start_date, end_date, sep = "/"),
                             bbox = c(box))
        
      # Step 3: Send request
        tmp03 <- get_request(tmp02)
        
      # Step 4: Fetch items
        tmp04 <- items_fetch(tmp03)
        
      # Step 5: Sign items
        matches <- items_sign(tmp04, sign_fn = sign_planetary_computer())
  
      # Cleanup of temporary objects at the end:
        base::rm(tmp01, tmp02, tmp03, tmp04)
  
  # get image collection object
  cube <- gdalcubes::stac_image_collection(matches$features,
                                           asset_names = asset_name,
                                           duration = "start")
  
  # set dimensions of the cube
  v <- gdalcubes::cube_view(srs = srs, #lat/lon
                            extent = list(t0 = as.character(start_date), t1 = as.character(end_date),
                                          left = box[1], right = box[3],
                                          top = box[4], bottom = box[2]),
                            dx = dx, dy = dy, dt= dt,
                            aggregation = aggregation, resampling = resampling)
  
  # create proxy data cube
  proxy_cube <- gdalcubes::raster_cube(cube, v)
  return(proxy_cube)
}
