##############################################################################################
#' Spatialize Flux Footprint Data into Raster Format
#'
#' @description This function processes flux footprint data, spatializing it into a raster format
#' suitable for GIS analysis. It supports reading flux data from various formats, setting the
#' spatial extent, and defining the coordinate reference system (CRS). The output can optionally
#' be written to a file in GeoTiff format.
#'
#' @param flux_input The input flux data, which can be a path to an ASCII grid or CSV file, a matrix,
#' or a data frame.
#' @param lower_left_X The X coordinate of the lower left corner of the output raster.
#' @param lower_left_Y The Y coordinate of the lower left corner of the output raster.
#' @param nodata_value The value to be considered as 'no data' in the input flux data. Defaults to 0.
#' @param cell_size The size of each cell in the output raster, assumed to be square. Defaults to 10.
#' @param crs The coordinate reference system for the output raster, specified in PROJ.4 format.
#' Defaults to "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs".
#' @param write Logical, whether to write the output raster to a file. Defaults to FALSE.
#' @param output_filename The filename (including path) for the output file if write is TRUE. 
#' If NULL and write is TRUE, an error is thrown.
#' @param file_format The format of the output file, defaults to 'GTiff'.
#'
#' @return A raster object representing the spatialized flux data.
#'
#' @examples
#' # Example usage:
#' raster <- def.spatialize.flux.foot(flux_input = "path/to/data.csv",
#'                                    lower_left_X = 100000,
#'                                    lower_left_Y = 400000,
#'                                    cell_size = 10,
#'                                    crs = "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs",
#'                                    write = TRUE,
#'                                    output_filename = "output.tif",
#'                                    file_format = 'GTiff')
#'
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
#'
#' @author Sam Bower \email{sbower@atmofacts.com}
#' @keywords spatial, GIS, raster, flux
#'
#' @seealso \code{\link[raster]{writeRaster}}, \code{\link[raster]{raster}}, \code{\link[sp]{CRS}}
#'
#' @export
#'
#' @changelog
#' - Sam Bower (2023-02-02): Original creation.
###############################################################################################

# Define the function with an additional CRS argument
def.spatialize.flux.foot <- function(
  flux_input,
  lower_left_X,
  lower_left_Y,
  nodata_value = 0,
  cell_size = 10,
  crs = "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs",
  write = FALSE,
  output_filename = NULL,
  file_format = 'GTiff'
) {
  
  # Check if input is a path to a file, a data frame, or a matrix
  if (is.character(flux_input)) {
    # Assuming flux_input is a filepath to either a .asc or .csv file
    file_extension <- tools::file_ext(flux_input)
    if (file_extension == "asc") {
      flux_matrix <- as.matrix(read.table(flux_input, header = FALSE))
    } else if (file_extension == "csv") {
      flux_matrix <- as.matrix(read.csv(flux_input, header = TRUE))
    } else {
      stop("Unsupported file format. Please provide a .asc or .csv file.")
    }
  } else if (is.data.frame(flux_input)) {
    flux_matrix <- as.matrix(flux_input)
  } else if (is.matrix(flux_input)) {
    flux_matrix <- flux_input
  } else {
    stop("The data input must be a file path, matrix, or a data frame.")
  }
  
  #initialize raster object from flux data input
  flux_raster <- raster::raster(flux_matrix)
  flux_raster[flux_raster==nodata_value] <- NA
  
  # Calculate and set the extent based on lower left corner and cell size
  cellSizeX <- cell_size
  cellSizeY <- cell_size
  ncols <- ncol(flux_matrix)
  nrows <- nrow(flux_matrix)
  extentXmin <- lower_left_X
  extentXmax <- lower_left_X + (ncols * cellSizeX)
  extentYmin <- lower_left_Y
  extentYmax <- lower_left_Y + (nrows * cellSizeY)
  raster::extent(flux_raster) <- c(extentXmin, extentXmax, extentYmin, extentYmax)
  
  
  # Set the CRS of the raster object
  raster::crs(flux_raster) <- sp::CRS(crs)
  
  #Write raster to file if write is true. The default file format is a GeoTiff
  if (write == TRUE){
    writeRaster(
      flux_raster,
      filename = output_filename,
      format = file_format,
      overwrite = TRUE
    )
  }
  
  return(flux_raster)
  
}