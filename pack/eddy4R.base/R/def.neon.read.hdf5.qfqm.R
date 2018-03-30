##############################################################################################
#' @title Wrapper function: Reading quality flags from NEON HDF5 files

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description definition function. Reads an HDF5 input file in NEON standard format from \code{DirInpLoca}. 

#' @param DirInpLoca Character: Input directory.
#' @param SiteLoca Character: Site location.
#' @param DateLoca Character: Date in ISO format "(2016-05-26").
#' @param VarLoca Character: Which instrument to read data from.
#' @param LvlTowr The tower level that the sensor data is being collected in NEON data product convention (HOR_VER)
#' @param FreqLoca Integer: Measurement frequency.
#' @param MethMeas A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecte".

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
#   Natchaya Pingintha-Durden (2017-06-21)
#     adding parameter MethMeas to distinguish different cases for ecte and ecse
#   David Durden (2017-12-12)
#     Removing rev number from dp0p data product HDF5 group levels 
#   Natchaya P-Durden (2018-01-19)
#     Updating to remove rev numbers from ECSE dp0p HDF5 data product group level 
#   Natchaya P-Durden (2018-03-30)
#     applied term name convention; replaced Levl by Lvl
##############################################################################################

def.neon.read.hdf5.qfqm <- function(
  DirInpLoca,
  SiteLoca,
  DateLoca,
  VarLoca,
  LvlTowr = c("000_040", "000_050", "000_060")[3],
  FreqLoca,
  MethMeas = c("ecte", "ecse")[1]
){
  
#Read in the flags from the HDF5 file 
if (MethMeas == "ecte") {
qfqm <- rhdf5::h5read(file = base::paste0(DirInpLoca, "/ECTE_dp0p_", SiteLoca, "_", DateLoca, ".h5"),
                      name = base::paste0("/", SiteLoca, "/dp0p/qfqm/", VarLoca, "/",LvlTowr), read.attributes = TRUE)
}

if (MethMeas == "ecse") {
qfqm <- rhdf5::h5read(file = base::paste0(DirInpLoca, "/ECSE_dp0p_", SiteLoca, "_", DateLoca, ".h5"),
                      name = base::paste0("/", SiteLoca, "/dp0p/qfqm/", VarLoca, "/",LvlTowr), read.attributes = TRUE)
}
  
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

