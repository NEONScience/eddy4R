def.plot.flux.animate <- function(
  input_folder,
  output_file,
  nodata_value = 0,
  colormap = 'YlOrRd',
  basemap_style = 'osm',
  palette_style = 'equal',
  alpha = 0.4,
  legend = TRUE
  ) {
  
  # Load necessary libraries
  library(raster)
  library(tmap)
  library(tmaptools)
  library(gifski)
  
  # Ensure tmap is in plot mode
  tmap_mode("plot")
  
  # Create a temporary directory for storing map images
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # List all raster files in the directory
  raster_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Initialize a list to store file paths of individual map images
  temp_files <- vector("character", length(raster_files))
  
  
}