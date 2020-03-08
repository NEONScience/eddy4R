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
#   Natchaya P-Durden (2018-05-22)
#     rename function from def.neon.read.hdf5.qfqm() to def.hdf5.read.qfqm()
##############################################################################################

def.hdf5.read.qfqm <- function(
  DirInpLoca,
  SiteLoca,
  DateLoca,
  VarLoca,
  LvlTowr = c("000_040", "000_050", "000_060")[3],
  FreqLoca,
  DataType = c(data,qfqm)[1],
  MethMeas = c("ecte", "ecse")[1]
){

#Read in the flags from the HDF5 file
if (MethMeas == "ecte") {
rpt <- rhdf5::h5read(file = base::paste0(DirInpLoca, "/ECTE_dp0p_", SiteLoca, "_", DateLoca, ".h5"),
                      name = base::paste0("/", SiteLoca, "/dp0p/",DataType,"/", VarLoca, "/",LvlTowr), read.attributes = TRUE, drop = TRUE) #drop converts to vector
}

if (MethMeas == "ecse") {
rpt <- rhdf5::h5read(file = base::paste0(DirInpLoca, "/ECSE_dp0p_", SiteLoca, "_", DateLoca, ".h5"),
                      name = base::paste0("/", SiteLoca, "/dp0p/",DataType,"/", VarLoca, "/",LvlTowr), read.attributes = TRUE, drop = TRUE) #drop converts to vector
}

# print message to screen
msg <- paste0("dataset ", DateLoca, ": ", VarLoca, " hdf5 read-in complete")
tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})  
  
#Convert each flag to a vector from a 1D array
#for(idx in base::names(rpt)) rpt[[idx]] <- base::as.vector(rpt[[idx]]); base::rm(idx)

#Cache attributes before coverting to data.frame
attr <- attributes(rpt)

# convert list to data.frame
rpt <- base::as.data.frame(rpt, stringsAsFactors = FALSE)

# convert type of variable time
if("time" %in% colnames(rpt)){
rpt$time <- base::as.POSIXct(rpt$time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC") + 0.0001
}

# perform unit conversion
rpt <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = rpt,
                                                          unitFrom = attributes(rpt)$Unit,
                                                          unitTo = "intl"))

# sd assign attribute to gasRefe
if (VarLoca == "gasRefe"){
  names(attr(rpt,"Sd")) <-  attr(rpt,"Name")
  names(attr(rpt,"Sd")) <-  attr(rpt,"Name")
  base::names(rpt$Sd) <- rpt$Name
  base::names(attr$DfSd) <- attr$Name
  base::attributes(data)$sd <- attr$Sd[base::names(data)]
  base::attributes(data)$DfSd <- attr$DfSd[base::names(data)]
}

#Apply units to each flag
lapply(seq_len(length(rpt)), function(x){
  tryCatch({rlog$debug(x)}, error=function(cond){print(x)})
  attributes(rpt[[x]])$unit <<- attributes(rpt)$Unit[[x]]
  attributes(rpt[[x]])$`Dspk$Br86$MaxReso` <<- attributes(rpt)$`Dspk$Br86$MaxReso`[[x]]
  attributes(rpt[[x]])$`Dspk$Br86$NumBin` <<- attributes(rpt)$`Dspk$Br86$NumBin`[[x]]
  attributes(rpt[[x]])$`Dspk$Br86$NumWndw` <<- attributes(rpt)$`Dspk$Br86$NumWndw`[[x]]
  })


return(rpt)
}

