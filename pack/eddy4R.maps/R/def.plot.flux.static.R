

def.plot.flux.static <- function(inputPath, basemapStyle = 'osm', nodataValue = 0, outputPath = 'output') {

# Change java system parameters to allow for headless operation (to disable under-the-hood java GUI which will not work in this Docker container) . 
  options(java.parameters = "-Djava.awt.headless=true")
  
# read in single raster file.
  
  if (dir.exists(inputPath)) {
    rasterFiles <- list.files(inputPath, pattern = "\\.tif$", full.names = TRUE)
    if (length(rasterFiles) == 0) {
      stop("No raster files found in the directory.")
    }
    
# set nodata value.
  
# create basemap layer with OpenStreetMap and tmaptools.
  
# superimpose raster layer with given color scheme.
  
# save
  
# Read in folder of raster files
  
# Plot iteratively 
  
# Save
}}

