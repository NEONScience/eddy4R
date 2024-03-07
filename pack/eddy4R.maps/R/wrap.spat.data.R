#' Generate spatial targets
#'
#' @param dir A directory of .tif files for scoring
#' @param site_id The side ID of the area being forecasted. Currently "august_complex" is the only option.
#'
#' @return message from minio submission
#' @export
#'
#' @examples spat4cast_submit(dir = "targets", site_id = "august_complex")



wrap.spat.data.grab <- function(
    dir = tempdir(), 
    dateBgn = "2019-05-01",
    dateEnd = "2019-10-31",
    site = c("CPER", "CHEESEHEAD")[2],
    dt = "P1M",
    dx = 0.1,
    dy = 0.1
)
{
  
  print(paste0("Grabbing spatial data at ", Sys.time(), " for ", site))
  
  library(here)
  library(sf)
  library(lubridate)
  library(gdalcubes)
  library(rstac)
  library(stars)
  
  
  
  #Source functions
 # for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

  #Create fire bounding box
  bbox <- def.bbox(site = site, pad_box = TRUE)
  
  #Target date
  dateBgn <- lubridate::floor_date(as.Date(dateBgn), "month") #first day of the month
  dateEnd <- lubridate::ceiling_date(as.Date(dateEnd), "month")
  
  # Ingest data ------------------------------------------------------------
  gdalcubes::gdalcubes_options(parallel=TRUE)
  
  # use ingest_planetary_data function to extract raster cube for fire bounding box between Jan 1 2002 and July 1 2023.
  raster_cube <- def.spat.data.cube(start_date = dateBgn, 
                                       end_date = dateEnd, 
                                       box = bbox$bbox,
                                       srs = "EPSG:4326",
                                       dx = dx, 
                                       dy = dy, 
                                       dt = dt,
                                       collection = "modis-15A2H-061",
                                       asset_name = "Lai_500m")
  
  
  
  # create target file
  # target <- create_target_file(cuberast = raster_cube,
  #                              site_id = site_id,
  #                              date = as.character(date),
  #                              dir = tempdir(),
  #                              bucket = "efi/spat4cast-targets",
  #                              mask = fire_box$maskLayer)
  # 
  
} #End of function