#' Download data from Microsoft planetary comuputer 
#'
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
#' @examples 
#' ingest_planetary_data(start_date = "2022-01-01", end_date = "2023-07-01", box =  c("xmin" = -123, "ymin" = 39, "xmax" = -122, "ymax" = 40))
#' @export
#' 
def.spat.data.cube <- function(start_date,
                                  end_date,
                                  box,
                                  collection = "modis-15A2H-061",
                                  asset_name = "Lai_500m",
                                  srs = "EPSG:4326",
                                  dx = 0.1, 
                                  dy = 0.1, 
                                  dt = "P30D",
                                  aggregation = "mean",
                                  resampling = "near"){
  
  # check box
  assertthat::are_equal(length(box), 4)
  
  # get STACItemCollection
  matches <-
    rstac::stac("https://planetarycomputer.microsoft.com/api/stac/v1") |>
    stac_search(collections = collection,
                datetime = paste(start_date, end_date, sep = "/"),
                bbox = c(box)) |>
    get_request() |>
    items_fetch() |>
    items_sign(sign_fn = sign_planetary_computer())
  
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
