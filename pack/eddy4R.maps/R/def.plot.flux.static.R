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
  basemap_style = 'bing',
  alpha = 0.5,
  nodata_value = 0,
  colormap = 'YlOrRd',
  style = 'equal',
  color_n = 7,
  use_basemap = FALSE, 
  background_color = "white", 
  units = "Units",
  legend_bg_alpha = 1
) {
  
  options(java.parameters = "-Djava.awt.headless=true")
  library(tmap)
  library(tmaptools)
  library(OpenStreetMap)
  library(RColorBrewer)
  
  plotRaster <- function(rasterFile, outputPath) {
    
    rasterFile[rasterFile == nodata_value] <- NA
    map <- NULL
    
    if (use_basemap && basemap_style != 'none') {
      osm_map <- tmaptools::read_osm(rasterFile, type = basemap_style)
      map <- tm_shape(osm_map) +
        tm_rgb() +
        tm_shape(rasterFile) +
        tm_raster(style = style, alpha = alpha, palette = brewer.pal(n = color_n, name = colormap), title = units) +
        tm_layout(
          legend.bg.alpha = legend_bg_alpha,
          legend.bg.color = 'white',
          legend.outside = FALSE
        )
    } else {
      map <- tm_shape(rasterFile) +
        tm_raster(style = style, alpha = alpha, palette = brewer.pal(n = color_n, name = colormap), title = units) +
        tm_layout(
          bg.color = background_color,
          legend.bg.alpha = legend_bg_alpha,
          legend.bg.color = 'white',
          frame = FALSE
        )
    }
    
    tmap_save(map, file = outputPath)
  }
  
  if (inherits(inputPath, "RasterLayer") || inherits(inputPath, "RasterStack") || inherits(inputPath, "RasterBrick")) {
    rasterLayer <- inputPath
    fileOutputPath <- paste0(outputPath) 
    plotRaster(rasterLayer, fileOutputPath)
  } else if (dir.exists(inputPath)) {
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
    fileOutputPath <- paste0(outputPath, "/", tools::file_path_sans_ext(basename(inputPath)), ".png")
    plotRaster(inputPath, fileOutputPath)
  } else {
    stop("Input path is neither a valid raster object, file, nor a directory containing raster files.")
  }
}
