
# Load necessary libraries
#library(ggplot2)
#library(ggmap)
#library(raster)
#library(leaflet)
#library(mapview)

# Define the function with improved interactive HTML handling
plot_geotiff_on_basemap <- function(input_path, output_path, colormap = 'viridis', opacity = 0.5, basemap_style = 'OpenStreetMap', interactive = FALSE) {
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

# Example usage:
path_in = "/home/sbower/eddy/sambower/Documents/AtmoFacts/maps_package/test_data/random_geotiff/footprints/"
path_out = "/home/sbower/eddy/sambower/Documents/AtmoFacts/maps_package/test_data/random_geotiff/outputs/"
plot_geotiff_on_basemap(path_in, path_out, 'viridis', opacity = 0.5,'OpenStreetMap', TRUE)
