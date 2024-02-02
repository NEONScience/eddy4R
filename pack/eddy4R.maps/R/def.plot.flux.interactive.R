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
  
  # Determine if input_path is a directory or a single file
  if (dir.exists(input_path)) {
    rasterFiles <- list.files(input_path, pattern = "\\.tif$", full.names = TRUE)
    if (length(rasterFiles) == 0) {  # Check if no TIFF files found
      stop("No TIFF files found in the directory.")
    }
  } else if (file.exists(input_path) && grepl("\\.tif$", input_path)) {
    rasterFiles <- list(input_path)  # Ensure rasterFiles is a list
  } else {
    stop("Input path is neither a valid folder nor a TIFF file.")
  }
  
  # Initialize Leaflet map
  map <- leaflet() %>%
    addProviderTiles("OpenStreetMap", group = "Street") %>%
    addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
    addProviderTiles("CartoDB.DarkMatter", group = "Dark")
  
  # Function to process and add each raster file to the map
  processAndAddRaster <- function(filePath, map) {
    flux <- raster(filePath)
    # Replace nodata_value with NA
    flux[flux == nodata_value] <- NA
    # Add raster to the map using specified colormap and alpha
    map <- map %>% addRasterImage(flux, group = base::basename(filePath), colors = colormap, opacity = alpha, layerId = base::basename(filePath))
    return(map)
  }
  
  # Apply the function to each raster file
  for(filePath in rasterFiles) {
    map <- processAndAddRaster(filePath, map)
  }
  
  # Dynamically create a list of groups for the layers control based on the raster files
  rasterGroups <- base::basename(rasterFiles)
  
  # Add layers control to the map
  map <- map %>% addLayersControl(overlayGroups = rasterGroups, baseGroups = c("Street", "Imagery", "Dark"))
  
  if (!is.null(save_path)) {
    htmlwidgets::saveWidget(map, file = save_path, selfcontained = TRUE)
  } else {
    # Print the map to display it interactively
    print(map)
  }
}
