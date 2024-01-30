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
#   Stefan Metzger (2011-03-04)
#     original creation
#   Stefan Metzger (2022-02-08)
#     update to eddy4R terminology and modularize into definition function
###############################################################################################
# Load necessary libraries
#library(ggplot2)
#library(ggmap)
#library(raster)
#library(leaflet)
#library(mapview)

# Define the function with improved interactive HTML handling
plot.flux.spatial <- function(input_path, output_path, colormap = 'viridis', opacity = 0.5, basemap_style = 'OpenStreetMap', interactive = FALSE) {
  # Check if input_path is a directory or a single file
  if (dir.exists(input_path)) {
    # Handle directory of TIFF files
    tiff_files <- list.files(input_path, pattern = '\\.tif$', full.names = TRUE)
    if (interactive) {
      # Create an interactive map with layer selection for each TIFF file
      map <- leaflet() %>%
        #add a basemap based on the parameter basemap_style
        addProviderTiles(leaflet::providers[[basemap_style]])
      for (file in tiff_files) {
        #add the raster layers iteratively
        raster_layer <- raster(file)
        map <- map %>%
          addRasterImage(
            raster_layer,
            group = basename(file),
            colors = colormap,
            opacity = opacity)
      }
      
      map <- map %>%
      addLayersControl(
        overlayGroups = basename(tiff_files),
        options = layersControlOptions(collapsed = FALSE))
      output_html_path <- paste0(output_path, tools::file_path_sans_ext(basename(file)), '.html')
      saveWidget(map, file = output_html_path, selfcontained = TRUE)
      cat('Interactive map saved to:', output_html_path, '
')
    } else {
      # Create a PNG map for each TIFF file
      for (file in tiff_files) {
        raster_layer <- raster(file)
        map <- leaflet() %>%
          addProviderTiles(leaflet::providers[[basemap_style]]) %>%
          addRasterImage(raster_layer, colors = colormap, opacity = opacity)
        mapshot(map, file = paste0(output_path, tools::file_path_sans_ext(basename(file)), '.png'))
      }
    }
  } else {
    # Handle single TIFF file
    raster_file <- raster(input_path)
    map <- leaflet() %>%
      addProviderTiles(leaflet::providers[[basemap_style]]) %>%
      addRasterImage(raster_file, colors = colormap, opacity = opacity)
    if (!interactive) {
      mapshot(map, file = paste0(output_path,tools::file_path_sans_ext(basename(input_path)), '.png'))
    } else {
      output_html_path <- paste0(output_path, tools::file_path_sans_ext(basename(input_path)), '.html')
      saveWidget(map, file = output_html_path, selfcontained = TRUE)
      cat('Interactive map saved to:', output_html_path, '
')
    }
  }
}
