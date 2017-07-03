##############################################################################################
#' @title Wrapper function: Reading quality flags from NEON HDF5 files

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description definition function. Reads an HDF5 input file in NEON standard format from \code{DirInpLoca}. 

#' @param DirInpLoca Character: Input directory.
#' @param SiteLoca Character: Site location.
#' @param DateLoca Character: Date in ISO format "(2016-05-26").
#' @param VarLoca Character: Which instrument to read data from.
#' @param LevlTowr The tower level that the sensor data is being collected in NEON data product convention (HOR_VER)
#' @param FreqLoca Integer: Measurement frequency.

#' @return 
#' Named list \code{qfqm} containing time-series of quality flags.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords file, read, pre-processing, diagnostic flags, QAQC, quality flags

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   David Durden (2017-04-21)
#     Original creation
##############################################################################################

def.neon.read.hdf5.qfqm <- function(
  DirInpLoca,
  SiteLoca,
  DateLoca,
  VarLoca,
  LevlTowr = c("000_040", "000_050", "000_060")[3],
  FreqLoca
){
  
#Read in the flags from the HDF5 file  
qfqm <- rhdf5::h5read(file = base::paste0(DirInpLoca, "/ECTE_dp0p_", SiteLoca, "_", DateLoca, ".h5"),
                      name = base::paste0("/", SiteLoca, "/dp0p/qfqm/", VarLoca, "_001/",LevlTowr), read.attributes = TRUE)
 
#Convert each flag to a vector from a 1D array                     
for(idx in base::names(qfqm)) qfqm[[idx]] <- base::as.vector(qfqm[[idx]]); base::rm(idx)

#Apply units to each flag
lapply(seq_len(length(qfqm)), function(x){ 
  print(x)
  attributes(qfqm[[x]])$Unit <<- attributes(qfqm)$Unit[[x]]
  })

# convert list to data.frame
qfqm <- base::as.data.frame(qfqm, stringsAsFactors = FALSE)

return(qfqm)
}

