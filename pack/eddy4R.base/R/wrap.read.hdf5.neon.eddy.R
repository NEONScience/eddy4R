##############################################################################################
#' @title Reading NEON HDF5 files

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Wrapper function. Reads an HDF5 input file in NEON standard format from \code{DirInpLoca}. Subsequently, (i) name and unit attributes are converted to eddy4R standard terms, (ii) variable units are converted accordingly, (iii) the data is regularized, (iv) sensor diagnostic tests are performed, (v) a range test is performed, and (vi) the data is de-spiked.

#' @param \code{DirInpLoca} Character: Input directory.
#' @param \code{SiteLoca} Character: Site location.
#' @param \code{DateLoca} Character: Date in ISO format "(2016-05-26").
#' @param \code{VarLoca} Character: Which instrument to read data from.
#' @param \code{FreqLoca} Integer: Measurement frequency.
#' @param \code{TermLoca} Named character: Lookup table for variable term substitution.
#' @param \code{UnitLoca} List of named characters: For unit term substitution and definition of eddy4R-internally used units.
#' @param \code{RngLoca} List of named ingegers: Thresholds for range test.
#' @param \code{DespLoca} List of integers: De-spiking parameters

#' @return 
#' Named list containing pre-processed time-series $time and $data.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @keywords file, read, pre-processing, unit conversion, regularization, diagnostic, range, spike

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-08-07)
#     original creation
##############################################################################################
        
wrap.read.hdf5.neon.eddy <- function(
  DirInpLoca = DirInp,
  SiteLoca = "SERC",
  DateLoca = date,
  VarLoca = c("irga", "irgaMfcSamp", "soni")[2],
  FreqLoca = freq_res,
  TermLoca = Term,
  UnitLoca = Unit,
  RngLoca = Rng,
  DespLoca = Desp
) {
  
# create regular time dimension
# POSIX is rounding down, add time increment after last significant digit
time <- seq.POSIXt(
  from = base::as.POSIXlt(paste(DateLoca, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
  to = base::as.POSIXlt(paste(DateLoca, " ", "23:59:59.9502", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
  by = 1/FreqLoca
)

# get available dates from directory structure

  # get available files from directory structure
  file <- list.files(DirInpLoca)
  
  # exclude files and subfolder with formats different from target file format
  file <- file[which(sapply(1:length(file), function(x) strsplit(file[x], "_")[[1]][3] == SiteLoca))]
  
  # extract dates in ISO format
  
    # first split file name from extension
    file <- sapply(1:length(file), function(x) strsplit(file[x], "[.]")[[1]][1])
    
    # then split date from remainder of filename
    file <- sapply(1:length(file), function(x) strsplit(file[x], "_")[[1]][4])

# check if dataset is available

###
# start data availability
# case "dataset is not available": create empty data.frame
if(!(DateLoca %in% file)) {
###

  # irga
  if(VarLoca == "irga") {
    
    data <- data.frame(matrix(data = NaN, ncol = 20, nrow = length(time)))
    names(data) <- c("asrpCO2", "asrpH2O", "diag", "diag02", "fdMoleCO2", "fdMoleH2O", "poteCool", "presAtmBox",
                     "presGageCell", "pwrCO2Ref", "pwrCO2Samp", "pwrH2ORef", "pwrH2OSamp", "rhoMoleCO2", "rhoMoleH2O",
                     "ssiCO2", "ssiH2O", "tempBloc", "tempCellIn", "tempCellOut")
    attributes(data)$unit <- c("-", "-", "NA", "NA", "mol mol-1", "mol mol-1", "V", "Pa", "Pa", "W", "W", "W", "W",
                               "mol m-3", "mol m-3", "%", "%", "K", "K", "K")
    names(attributes(data)$unit) <- names(data)
  
  }

  # irga MFC
  if(VarLoca == "irgaMfcSamp") {
    
    data <- data.frame(matrix(data = NaN, ncol = 5, nrow = length(time)))
    names(data) <- c("frt", "frt00", "frtSet00", "presAtm", "temp")
    attributes(data)$unit <- c("litersPerMinute", "litersPerMinute", "litersPerMinute", "Pa", "K")
    names(attributes(data)$unit) <- names(data)
    
  }

  # soni
  if(VarLoca == "soni") {
    
    data <- data.frame(matrix(data = NaN, ncol = 6, nrow = length(time)))
    names(data) <- c("diag", "idx", "veloSoni", "veloXaxs", "veloYaxs", "veloZaxs")
    attributes(data)$unit <- c("NA", "NA", "m s-1", "m s-1", "m s-1", "m s-1")
    names(attributes(data)$unit) <- names(data)
    
  }

  # print message to screen
  print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " hdf5 file not available, NaNs substituted"))
  
###
# mid data availability
# case "dataset is available": start read data from hdf5 file
} else {
###

  # read-in data
    
    # read-in hdf5 data from the specified sensor
    # options via open connection: fid <- H5Fopen(paste0(DirInpLoca, "/ECTE_L0_", SiteLoca, "_", DateLoca, ".h5")); h5ls(fid)
    data <- rhdf5::h5read(file = base::paste0(DirInpLoca, "/ECTE_L0_", SiteLoca, "_", DateLoca, ".h5"),
                          name = base::paste0("/", SiteLoca, "/DP0_", VarLoca, "_001/000_000"),
                          read.attributes = TRUE)
    
    # convert 1-d array list-elements to vector list-elements
    # can be omitted once Dave figures out to store h5 data tables as vector list-elements
    for(idx in base::names(data)) data[[idx]] <- base::as.vector(data[[idx]]); base::rm(idx)
    
    # convert list to data.frame
    data <- base::as.data.frame(data, stringsAsFactors = FALSE)
    # base::attributes(data)$row.names <- NULL
    
    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " hdf5 read-in complete"))

  # assign hdf5 attributes
    
    # read attributes
    attr <- rhdf5::h5readAttributes(file = base::paste0(DirInpLoca, "/ECTE_L0_", SiteLoca, "_", DateLoca, ".h5"),
                                    name = base::paste0("/", SiteLoca, "/DP0_", VarLoca, "_001"))
    
    # convert imported variable names to eddy4R variable names
    base::names(data) <- def.repl.char(
      data = structure(.Data = base::names(data), names = base::names(data)),
      ReplFrom = base::names(TermLoca),
      ReplTo = TermLoca
    )

    # sort attributes in same order as data.frame
    attr$units <- base::as.vector(attr$units)
    base::names(attr$units) <- def.repl.char(
      data = structure(.Data = attr$names, names = attr$names),
      ReplFrom = base::names(TermLoca),
      ReplTo = TermLoca
      )
    attr$units <- attr$units[base::names(data)]

    # assign unit descriptions (DPS terms)
    base::attributes(data)$unit <- attr$units
    rm(attr)

    # replace unit descriptions (DPS terms) with unit descriptions (eddy4R terms)
    attributes(data)$unit <- def.repl.char(
      data = base::attributes(data)$unit,
      ReplFrom = base::names(UnitLoca$NameConv),
      ReplTo = base::unlist(UnitLoca$NameConv)
    )
    
    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " hdf5 attributes complete"))

  # unit conversion
    
    # perform unit conversion
    data <- eddy4R.base::def.conv.unit(data = data,
                                       unitFrom = attributes(data)$unit,
                                       unitTo = unlist(UnitLoca$Refe)[names(attributes(data)$unit)])
    
    # conversion to "intl" units can be used once unknown units propagate frm unitFrom to unitTo
    # data <- base::suppressWarnings(eddy4R.base::def.conv.unit(data = data,
    #                                                           unitFrom = attributes(data)$unit,
    #                                                           unitTo = "intl"))

    # store target unit names for future use
    names(attributes(data)$unit) <- names(data)
      
    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " unit conversion complete"))

  # regularization
    
    # perform regularization
    data <- def.rglr(
      timeMeas = base::as.POSIXlt(data$time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
      dataMeas = data,
      unitMeas = attributes(data)$unit,
      BgnRglr = as.POSIXlt(min(time)),
      EndRglr = as.POSIXlt(max(time)),
      FreqRglr = FreqLoca,
      MethRglr = "zoo"
    )$dataRglr

    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " regularization complete"))

  # discard data with bad sensor diagnostic flag
    
    # for selected sensors
    if(VarLoca %in% base::names(RngLoca)) {

      # irga
      if(VarLoca == "irga") {

        # simple implementation based on decimal representation
        whr <- which(data$dataRglr$diag != 8191)
        if(length(whr) > 0) data$dataRglr[whr, c("asrpCO2", "asrpH2O", "rhoMoleCO2", "rhoMoleH2O", "fdMoleCO2", "fdMoleH2O")] <- NaN
        rm(whr)
        
        # #determine binary representations for integers up to 2^32-1
        # #https://stat.ethz.ch/pipermail/r-help/2000-February/010141.html
        # #truncate leading zeroes in strings
        # #http://r.789695.n4.nabble.com/Truncating-leading-zeros-in-strings-td2966968.html
        # #60 s for one day of 20 Hz data (1.7e6 data points)
        # deci2base <- function(deci, base = 2) {
        #   a <- base ^ (31:0)
        #   b <- base * a
        #   sapply(deci, function(x) paste(as.integer((x %% b)>=a), collapse=""))
        # }
        # #determine decimal value of binary string
        # #http://stackoverflow.com/questions/12892348/in-r-how-to-convert-binary-string-to-binary-or-decimal-value
        # base2deci <- function(base_number, base = 2) {
        #   sapply(1:length(base_number), function(x) 
        #     sum(base^(which(rev(unlist(strsplit(as.character(base_number[x]), "")) == 1))-1))
        #   )
        # }
        # #determine binary representation of QF_IRGA bit 12 to bit 0
        # DATA$Hz20$QF_7200 <- substr(deci2base(DATA$Hz20$QF_7200), 20, 32)
        # DATA$Hz20$QF_7200[which(DATA$Hz20$QF_7200 == "ANANANANANANA")] <- NA
        # #discard irga data with invalid quality flag
        # whr_not <- which(substr(DATA$Hz20$QF_7200, 1, 9) != "111111111" | is.na(DATA$Hz20$QF_7200))
        # if(length(whr_not) > 0) DATA$Hz20[whr_not, !names(DATA$Hz20) %in% c("QF_7200", "UTC_frac", "DOY_frac", "UTC", "MST")] <- NA
        # rm(whr_not)

      }
      
      # soni
      if(VarLoca == "soni") {
        
        # discard sonic data with invalid quality flag
        idx <- which(data$diag != 0)
        if(length(idx) > 0) data[idx, names(RngLoca$soni)] <- NaN
        rm(idx)

      }

      # print message to screen
      print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " sensor diagnostics complete"))
      
    } else {
      
      # print message to screen
      print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " sensor diagnostics not performed"))
      
    }

  # range test
    
    # perform range test
    if(VarLoca %in% base::names(RngLoca)) {

      # loop around variables
      for(idx in base::names(RngLoca[[VarLoca]])) {
        
        whr <- which(data[,idx] < RngLoca[[VarLoca]][1,idx] |
                     data[,idx] > RngLoca[[VarLoca]][2,idx])
        if(length(whr > 0)) data[whr,idx] <- NaN
        
      }; rm(idx, whr)

      # print message to screen
      print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " range test complete"))
      
    } else {
      
      # print message to screen
      print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " range test not performed"))
      
    }
  
  # de-spiking
    
    # perform range test
    if(VarLoca %in% base::names(RngLoca)) {
  
      # loop around variables
      for(idx in base::names(RngLoca[[VarLoca]])) {

        #execute de-spiking algorithm
        data[,idx] <- spike.medfilt(DESP = list(
          # input data, univariate vector of integers or numerics
          dati = as.vector(data[,idx]),
          # filter width
          widt = DespLoca$widt * FreqLoca + 1,
          # initial number/step size of histogram bins
          nbin = DespLoca$nbin,
          # resolution threshold
          rest = DespLoca$rest
        ))$dato
        
      }
      
      # clean up
      rm(idx)
      
      # print message to screen
      print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " de-spiking complete"))
      
    } else {
      
      # print message to screen
      print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " de-spiking not performed"))
      
    }

###
}
# end data availability  
###
    
# return data
return(list(time = time, data = data))

}

