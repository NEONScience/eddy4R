def.plot.flux.static <- function(inputPath, basemapStyle = 'osm', nodataValue = 0, outputPath = 'output') {

# Change java system parameters to allow for headless operation (to disable under-the-hood java GUI which will not work in this Docker container) . 
  options(java.parameters = "-Djava.awt.headless=true")

# Plotting function with tmap
  
  processRaster <- function(rasterFile, basemapStyle, nodataValue, outputPath) {
    rasterLayer <- raster::raster(rasterFile)
    rasterLayer[rasterLayer == nodataValue] <- NA
    
    osm_map <- tmaptools::read_osm(rasterLayer, type = basemapStyle)
    map <- tm_shape(osm_map) +
      tm_rgb() +
      tm_shape(rasterLayer) +
      tm_raster(style = "equal", alpha = 0.4) +
      tm_layout(legend.outside = FALSE)
    
    tmap_save(map, file = outputPath)
  }
  
# Read in a raster file or folder of raster files

  if (dir.exists(inputPath)) {
    rasterFiles <- list.files(inputPath, pattern = "\\.tif$", full.names = TRUE)
    if (length(rasterFiles) == 0) {
      stop("No raster files found in the directory.")
    }
    for (rasterFile in rasterFiles) {
      fileName <- basename(rasterFile)
      fileOutputPath <- paste0(outputPath, "/", sub("\\.tif$", ".png", fileName))
      processRaster(rasterFile, basemapStyle, nodataValue, fileOutputPath)}
  else if (file.exists(inputPath) && grepl("\\.tif$", inputPath)) {
    processRaster(inputPath, basemapStyle, nodataValue, paste0(outputPath, ".png"))
    }
  }
  else {
    stop("Input path is neither a valid raster file nor a directory containing raster files.")
  }
}

# set nodata value.
  
# create basemap layer with OpenStreetMap and tmaptools.
  
# superimpose raster layer with given color scheme.
  
# save
  
# Read in folder of raster files
  
# Plot iteratively 
  
# Save


