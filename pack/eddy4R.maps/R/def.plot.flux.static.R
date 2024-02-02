##############################################################################################
#' @title Definition function: Plot fluxes on a static basemap

#' @author
#' Sam Bower \email{sbower@atmofacts.com}

#' @description Function definition. This function creates a flux map visualization on a user defined basemap.
#' 
#' @param inputPath A spatial file or folder of spatial files to plot on the basemap.
#' @param outputPath A string for the folder location to save output maps.
#' @param nodata_value The nodata value specified from the flux data.
#' @param basemap_style Basemap styles in OpenStreetMap (more in openmap())
#' @param alpha Raster opacity
#' @param colormap palette_explorer() for palette options
#' @param style quantile, equal, cont, cat
#' @param color_n color categories in palette

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
#   Sam Bower (2023-02-01)
#     original creation
#
# 
###############################################################################################
def.plot.flux.static <- function(
  inputPath,
  outputPath,
  nodata_value = 0,
  basemap_style = 'bing',
  alpha = 0.5,
  colormap = 'YlOrRd',
  style = 'equal',
  color_n = 7) { #palette_explorer()) {
  
  # Change java system parameters to allow for headless operation (to disable under-the-hood java GUI which will not work in this Docker container).
  options(java.parameters = "-Djava.awt.headless=true")
  
  # Function to load and process a single raster file and plot it with tmap
  plotRaster <- function(rasterFile, outputPath) {
    
    #read in raster with raster library
    rasterLayer <- raster::raster(rasterFile)
    rasterLayer[rasterLayer == nodata_value] <- NA
  
    #grab baselayer from OpenStreetMap  
    osm_map <- tmaptools::read_osm(rasterLayer, type = basemap_style)
    
    #create tmap object with raster superimposed on basemap 
    map <- tm_shape(osm_map) +
      tm_rgb() +
      tm_shape(rasterLayer) +
      tm_raster(style = style, alpha = alpha, palette = get_brewer_pal(palette = colormap, n = color_n, plot = FALSE)) +
      tm_layout(legend.outside = FALSE)
    
    #save tmap object
    tmap_save(map, file = outputPath)
  }
  
  # Check if inputPath is a directory and make a list of the files.
  if (dir.exists(inputPath)) {
    rasterFiles <- list.files(inputPath, pattern = "\\.tif$", full.names = TRUE)
    if (length(rasterFiles) == 0) {
      stop("No raster files found in the directory.")
    }
    #iteratively create and save the maps
    for (rasterFile in rasterFiles) {
      fileName <- basename(rasterFile)
      fileOutputPath <- paste0(outputPath, "/", sub("\\.tif$", ".png", fileName))
      plotRaster(rasterFile, fileOutputPath)
    }
    #Save a single file if the input is a single tiff
  } else if (file.exists(inputPath) && grepl("\\.tif$", inputPath)) {
    # InputPath is a single file
    fileOutputPath <- paste0(outputPath, "/", tools::file_path_sans_ext(base::basename(inputPath)), ".png")
    plotRaster(inputPath, fileOutputPath)
  } else {
    stop("Input path is neither a valid raster file nor a directory containing raster files.")
  }
}
