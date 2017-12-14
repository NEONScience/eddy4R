##############################################################################################
#' @title Definition function: Function to read in dp0p files 

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' David Durden \email{ddurden@battelleecology.org} \cr
#' Hongyan Luo \email{hluo@battelleecology.org}

#' @description Definition function. A function to read in dp0p files and extact relevant attributes as parameters for future use.

#' @param \code{DirFileParaLoca} Absolute path to h5 file from which workflow and scientific parameters are to be used, of class "character". [-]
#' @param \code{GrpName} Absolute path to h5 file group and variable name from which parameters are to be read, of class "character". [-]
#' @param \code{PosPara} Parameters can be subset by PosPara; defaults to NULL which imports all parameters. A vector of characters. [-]

#' @return 
#' The returned object is the parameters extracted from dp0p file for future use.  

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr

#' @keywords parameter, hdf5, dp0p

#' @examples Currently none

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-09-06)
#     original creation
##   David Durden (2016-05-04)
#     added HDF5 ingest
#   Stefan Metzger (2016-11-16)
#     initial transition to external parameter repo
#   Hongyan Luo (2017-03-17)
#     add header to the function
###############################################################################################

# definition function for parameter extraction
def.neon.read.hdf5.para <- function(
  # absolute path to h5 file from which workflow and scientific parameters are to be used
  DirFileParaLoca,
  # absolute path to h5 file group and variable name from which parameters are to be read
  # Grp and Name correspond to the group and name variables returned by h5ls(), so that GrpName <- paste0(group, "/", name)
  # GrpName = "site" reports the parameters at h5 file root level
  GrpName,
  # parameters can be subset by PosPara; defaults to NULL which imports all parameters
  PosPara = NULL
) {
  
  # open first dp0p h5 file to explore structure
  fileHdf5 <- rhdf5::H5Fopen(DirFileParaLoca)
  
  # c2r: location; to get site information and define output directory
  # first entry (hightest level) in name field
  if(GrpName == "site") GrpName <- h5ls(fileHdf5)$name[1]
  # Para$Flow$Loc <- c("IM", "LOS", "NR", "NS", "PF", "SERC")[6]
  
  # read all relevant attributes at the corresponding level from h5 file
  rpt <- rhdf5::h5readAttributes(file = fileHdf5, name = GrpName)
  
  # subset selected parameters
  if(!is.null(PosPara)) rpt <- rpt[PosPara]
  
  # sort attributes alphabetically
  rpt <- rpt[base::sort(base::names(rpt))]
  
  # which attributes are of type character?
  Pos01 <- base::names(rpt)[base::sapply(base::names(rpt), function(x) base::is.character(rpt[[x]]))]
  
  # split characters by comma separator and trim white spaces
  if(base::length(Pos01) > 0) {
    
    tmp <- base::sapply(Pos01, function(x) base::trimws(base::unlist(base::strsplit(x = rpt[[x]], split = ","))))
    if(is.vector(tmp)) tmp <- as.list(tmp)
    if(is.matrix(tmp)) tmp <- as.list(as.data.frame(tmp, stringsAsFactors = FALSE))
    rpt[Pos01] <- tmp
    rm(tmp)
    
  }
  
  # convert character to numeric provided the result does not yield NA, and there are no leading zeroes in any entry
  rpt <- lapply(rpt, function(x) {
    if(suppressWarnings(all(!is.na(as.numeric(as.character(x))))) && all(substr(as.character(x), 1, 1) != 0)) {
      as.numeric(as.character(x))
    } else {
      x
    }
  })
  
  # convert character NA to logical numeric provided the result does not yield NA, and there are no leading zeroes in any entry
  rpt <- lapply(rpt, function(x) {
    
    if(length(which(as.character(x) %in% c("-Inf", "Inf", "NA"))) > 0){
      as.logical(as.character(x))
    } else {
      x
    }
  })
  
  # close h5 file and clean up
  rhdf5::H5close()
  rm(Pos01)
  
  # return result
  base::return(rpt)
  
}