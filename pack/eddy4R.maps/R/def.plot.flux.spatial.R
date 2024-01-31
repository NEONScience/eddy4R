##############################################################################################
#' @title Definition function: Plot fluxes on a basemap

#' @author
#' Sam Bower \email{sbower@atmofacts.com}

#' @description Function definition. This function saves a GeoTiff flux map onto a static basemap or an interactive html map.
#' 
#' @param input_path A spatial file or folder of spatial files to plot on the basemap.
#' @param output_path A string for the folder location to save output maps.
#' @param interactive If FALSE - plots the flux(es) onto a static basemap. If TRUE, plots the flux(es) onto an interactive basemap.
#' @param basemap_style For a list of basemap styles use names(providers)

#' @return 
#' 
#' 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords visualization, flux mapping
#' 
#' @examples
#' 
#' path_in = "/path/to/folder/of/tif/files"
#' path_out = "/path/to/desired/output/location"
#' plot.flux.spatial(path_in, path_out, 'viridis', opacity = 0.5,'OpenStreetMap', TRUE)
#' 



#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Sam Bower (2023-01-30)
#     original creation
#
# 
###############################################################################################
# Define the function with improved interactive HTML handling
def.plot.flux.spatial <- function(input_path, output_path, colormap = 'viridis', opacity = 0.5, basemap_style = 'OpenStreetMap', interactive = FALSE) {
  # Load necessary libraries
  library(raster)
  library(leaflet)
  library(htmlwidgets)
  library(mapview)
  
  # Check if input_path is a directory or a single file
  if (base::dir.exists(input_path)) {
    # Handle directory of TIFF files
    tiff_files <- base::list.files(input_path, pattern = '\\.tif$', full.names = TRUE)
    if (interactive) {
      # Create an interactive map with layer selection for each TIFF file
      map <- leaflet::leaflet()
      map <- map %>% leaflet::addProviderTiles(leaflet::providers[[basemap_style]])
      
      for (file in tiff_files) {
        raster_layer <- raster::raster(file)
        map <- map %>% leaflet::addRasterImage(
          raster_layer,
          group = base::basename(file),
          colors = colormap,
          opacity = opacity)
      }
      
      map <- map %>% leaflet::addLayersControl(
        overlayGroups = base::basename(tiff_files),
        options = leaflet::layersControlOptions(collapsed = FALSE))
      
      output_html_path <- base::paste0(output_path, tools::file_path_sans_ext(base::basename(file)), '.html')
      htmlwidgets::saveWidget(map, file = output_html_path, selfcontained = TRUE)
      base::cat('Interactive map saved to:', output_html_path, '\n')
    } else {
      # Create a PNG map for each TIFF file
      for (file in tiff_files) {
        raster_layer <- raster::raster(file)
        map <- leaflet::leaflet() %>%
          leaflet::addProviderTiles(leaflet::providers[[basemap_style]]) %>%
          leaflet::addRasterImage(raster_layer, colors = colormap, opacity = opacity)
        mapview::mapshot(map, file = base::paste0(output_path, tools::file_path_sans_ext(base::basename(file)), '.png'))
      }
    }
  } else {
    # Handle single TIFF file
    raster_file <- raster::raster(input_path)
    map <- leaflet::leaflet() %>%
      leaflet::addProviderTiles(leaflet::providers[[basemap_style]]) %>%
      leaflet::addRasterImage(raster_file, colors = colormap, opacity = opacity)
    if (interactive) {
      output_html_path <- base::paste0(output_path, tools::file_path_sans_ext(base::basename(input_path)), '.html')
      htmlwidgets::saveWidget(map, file = output_html_path, selfcontained = TRUE)
      base::cat('Interactive map saved to:', output_html_path, '\n')
    } else {
      mapview::mapshot(map, file = base::paste0(output_path,tools::file_path_sans_ext(base::basename(input_path)), '.png'))
    }
  }
  return(map)
}
