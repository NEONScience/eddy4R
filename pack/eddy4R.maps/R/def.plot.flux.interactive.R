

def.plot.flux.interactive <- function(
  input_path,
  output_path,
  nodata_value = 0,
  alpha = 0.5,
  colormap = 'YlOrRd'
  ) {
  
  # List all TIFF files in the folder
  rasterFiles <- list.files(input_path, pattern = "\\.tif$", full.names = TRUE)
  
  # Initialize Leaflet map
  map <- leaflet() %>%
    addProviderTiles("OpenStreetMap", group = "Street") %>%
    addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
    addProviderTiles("CartoDB.DarkMatter", group= "Dark")
  
  # Function to process and add each raster file to the map
  processAndAddRaster <- function(filePath, map) {
    flux <- raster(filePath)
    # Replace zeros with NA
    flux[flux == 0] <- NA
    # Add raster to the map
    map <- map %>% addRasterImage(flux, group = base::basename(filePath), colors = 'YlOrRd', opacity = 0.7, layerId = base::basename(filePath))
    return(map)
  }
  
  
  
  
  
  
  
  
}