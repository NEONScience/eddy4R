def.plot.flux.static <- function(
  inputPath,
  outputPath,
  nodataValue = 0,
  basemapStyle = 'bing',
  alpha = 0.5,
  colormap = 'YlOrRd',
  style = 'equal',
  color_n = 7) { #palette_explorer()) {
  
  # Change java system parameters to allow for headless operation (to disable under-the-hood java GUI which will not work in this Docker container).
  options(java.parameters = "-Djava.awt.headless=true")
  
  # Function to load and process a single raster file and plot it
  plotRaster <- function(rasterFile, outputPath) {
    rasterLayer <- raster::raster(rasterFile)
    rasterLayer[rasterLayer == nodataValue] <- NA
    
    osm_map <- tmaptools::read_osm(rasterLayer, type = basemapStyle)
    map <- tm_shape(osm_map) +
      tm_rgb() +
      tm_shape(rasterLayer) +
      tm_raster(style = style, alpha = alpha, palette = get_brewer_pal(palette = colormap, n = color_n, plot = FALSE)) +
      tm_layout(legend.outside = FALSE)
    
    tmap_save(map, file = outputPath)
  }
  
  # Check if inputPath is a directory
  if (dir.exists(inputPath)) {
    rasterFiles <- list.files(inputPath, pattern = "\\.tif$", full.names = TRUE)
    if (length(rasterFiles) == 0) {
      stop("No raster files found in the directory.")
    }
    for (rasterFile in rasterFiles) {
      fileName <- basename(rasterFile)
      fileOutputPath <- paste0(outputPath, "/", sub("\\.tif$", ".png", fileName))
      plotRaster(rasterFile, fileOutputPath)
    }
  } else if (file.exists(inputPath) && grepl("\\.tif$", inputPath)) {
    # InputPath is a single file
    fileOutputPath <- paste0(outputPath, "/", basename(inputPath), ".png")
    plotRaster(inputPath, fileOutputPath)
  } else {
    stop("Input path is neither a valid raster file nor a directory containing raster files.")
  }
}
