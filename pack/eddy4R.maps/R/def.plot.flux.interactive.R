##############################################################################################
#' @title Definition function: Plot fluxes on an interactive map.

#' @author
#' Sam Bower \email{sbower@atmofacts.com}

#' @description Function definition. This function creates an interactive flux map visualization that can be used in R viewer or in a web browser.
#' 
#' @param input_path A geotiff file or folder of geotiff files to plot on the basemap.
#' @param nodata_value Nodata value from flux data
#' @param alpha Raster opacity
#' @param colormap palette_explorer() for palette options

#' @return 
#' 
#' 

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords visualization, flux mapping
#' 


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Sam Bower (2023-02-02)
#     original creation
#
# 
###############################################################################################


def.plot.flux.interactive <- function(
  input_path,
  nodata_value = 0,
  alpha = 0.7,
  colormap = 'YlOrRd',
  save_path = NULL
) {
  
  library(leaflet)
  library(raster)
  library(RColorBrewer)
  
  # Initialize Leaflet map
  map <- leaflet() %>%
    addProviderTiles("OpenStreetMap", group = "Street") %>%
    addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
    addProviderTiles("CartoDB.DarkMatter", group = "Dark")
  
  # Function to process and add a raster object to the map
  processAndAddRaster <- function(flux, map, layerName) {
    # Replace nodata_value with NA
    flux[flux == nodata_value] <- NA
    
    # Extract values from the raster while maintaining NA for nodata
    rasterValues <- getValues(flux)  # This will return a vector of all raster values
    validValues <- rasterValues[!is.na(rasterValues)]  # Filter out NA values
    
    if (length(validValues) == 0) {
      validValues <- c(0)  # Fallback to avoid issues with empty data
    }
    
    # Define the color palette function
    colorPal <- colorNumeric(palette = colormap, domain = range(validValues, na.rm = TRUE), na.color = "transparent")
    
    # Add raster to the map with the correct color mapping
    map <- map %>% addRasterImage(flux, group = layerName, colors = colorPal, opacity = alpha, layerId = layerName)
    return(map)
  }
  
  # Check if input_path is a raster object
  if (inherits(input_path, "RasterLayer") || inherits(input_path, "RasterStack") || inherits(input_path, "RasterBrick")) {
    layerName <- "CustomRasterLayer" # Modify as needed or generate dynamically
    map <- processAndAddRaster(input_path, map, layerName)
    rasterGroups <- c(layerName)
  } else {
    # Determine if input_path is a directory or a single file
    if (dir.exists(input_path)) {
      rasterFiles <- list.files(input_path, pattern = "\\.tif$", full.names = TRUE)
      if (length(rasterFiles) == 0) {  # Check if no TIFF files found
        stop("No TIFF files found in the directory.")
      }
    } else if (file.exists(input_path) && grepl("\\.tif$", input_path)) {
      rasterFiles <- list(input_path)  # Ensure rasterFiles is a list
    } else {
      stop("Input path is neither a valid raster object, folder, nor a TIFF file.")
    }
    
    # Apply the function to each raster file
    rasterGroups <- NULL
    for(filePath in rasterFiles) {
      layerName <- base::basename(filePath)
      flux <- raster(filePath)
      map <- processAndAddRaster(flux, map, layerName)
      rasterGroups <- c(rasterGroups, layerName)
    }
  }
  
  # Add layers control to the map
  map <- map %>% addLayersControl(overlayGroups = rasterGroups, baseGroups = c("Street", "Imagery", "Dark"))
  
  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(map, file = save_path, selfcontained = TRUE)
  } else {
    # Print the map to display it interactively
    print(map)
  }
}
