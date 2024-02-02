##############################################################################################
#' @title Definition function: Plot fluxes on an animated gif.

#' @author
#' Sam Bower \email{sbower@atmofacts.com}

#' @description Function definition. This function creates an animated flux map visualization for temporal flux map data contained in a folder.
#' 
#' @param input_folder A folder of geotiff files.
#' @param output_file The path of the output file (.gif)
#' @param nodata_value Nodata value from flux data
#' @param alpha Raster opacity
#' @param colormap palette_explorer() for palette options
#' @param palette_style quantile, equal, cont, cat
#' @param legend Boolean. If legend plots on the map
#' @param delay The delay time in the animation.
#' 

#' @return 
#' 
#' 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords visualization, flux mapping, animation
#' 
#' @examples 
#' 
#' def.plot.flux.animation(
#' input_folder = "/path/to/input/folder",  # Replace with your input folder path
#' output_file = "/path/to/output/animation.gif",  # Replace with your desired output GIF path
#' nodata_value = 0,
#' colormap = 'YlOrRd',
#' basemap_style = 'osm',
#' palette_style = 'equal',
#' alpha = 0.4,
#' legend = TRUE
#' )


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Sam Bower (2023-02-02)
#     original creation
#
# 
###############################################################################################

def.plot.flux.animate <- function(
  input_folder,
  output_file,
  nodata_value = 0,
  colormap = 'YlOrRd',
  basemap_style = 'osm',
  palette_style = 'equal',
  alpha = 0.4,
  legend = TRUE,
  delay = 1
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
  
  # Loop through each raster file to create a static map
  for (i in seq_along(raster_files)) {
    flux <- raster::raster(raster_files[i])
    flux[flux == nodata_value] <- NA  # Apply nodata value
    
    osm_map <- tmaptools::read_osm(flux, type = basemap_style)
    
    map <- tm_shape(osm_map) +
      tm_rgb() +
      tm_shape(flux) +
      tm_raster(style = palette_style, alpha = alpha, palette = colormap) +
      tm_layout(legend.outside = legend)
    
    # Generate a temporary file path for the static map image
    temp_file <- file.path(temp_dir, paste0("map_", i, ".png"))
    temp_files[i] <- temp_file  # Store the file path
    
    # Save the map as an image
    tmap_save(map, filename = temp_file, width = 800, height = 600, units = "px")
  }
  
  # Use gifski to create the GIF from the PNG files
  gifski(png_files = temp_files, gif_file = output_file, width = 800, height = 600, delay = delay, progress = TRUE)
  
  # After creating the GIF, clean up the temporary directory and its contents
  unlink(temp_dir, recursive = TRUE)
  
  cat("Animation created at:", output_file, "\n")
  
}