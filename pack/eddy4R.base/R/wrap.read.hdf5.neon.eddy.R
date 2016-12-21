##############################################################################################
#' @title Reading NEON HDF5 files

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Wrapper function. Reads an HDF5 input file in NEON standard format from \code{DirInpLoca}. Subsequently, (i) name and unit attributes are converted to eddy4R standard terms, (ii) variable units are converted accordingly, (iii) the data is regularized, (iv) sensor diagnostic tests are performed, (v) a range test is performed, and (vi) the data is de-spiked.

#' @param \code{DirInpLoca} Character: Input directory.
#' @param \code{SiteLoca} Character: Site location.
#' @param \code{DateLoca} Character: Date in ISO format "(2016-05-26").
#' @param \code{VarLoca} Character: Which instrument to read data from.
#' @param \code{FreqLoca} Integer: Measurement frequency.
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
#   Stefan Metzger (2016-08-23)
#     use unit conversion with "internal" units
#   Stefan Metzger (2016-10-04)
#     added replacement statements for unit attributes on individual variables in data.frame
# 		full implementation requires updating unit-specific behavior of eddy4R.base::def.rglr()
##############################################################################################
        
wrap.read.hdf5.neon.eddy <- function(
  DirInpLoca,
  SiteLoca,
  DateLoca,
  VarLoca,
  FreqLoca,
  Rglr = FALSE,
  Diag = FALSE,
  Rng = FALSE,
  RngLoca,
  DespLoca
) {
  
# create regular time dimension
# POSIX is rounding down, add time increment after last significant digit
time <- seq.POSIXt(
  from = base::as.POSIXlt(paste(DateLoca, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
  to = base::as.POSIXlt(paste(DateLoca, " ", "00:00:00.0002", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC") + 86400 - 1/FreqLoca,
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
    
    data <- data.frame(matrix(data = NaN, ncol = 19, nrow = length(time)))
    names(data) <- c("asrpCo2", "asrpH2o", "densMoleCo2", "densMoleH2o", "potCool", "powrCo2Refe", "powrCo2Samp", "powrH2oRefe",
                     "powrH2oSamp", "presAtm", "presDiff", "rtioMoleDryCo2", "rtioMoleDryH2o", "ssiCo2", "ssiH2o", "tempIn",
                     "tempOut", "tempRefe", "time")
    data$time <- time
    attributes(data)$unit <- c("-", "-", "molCo2 m-3", "molH2o m-3", "V", "W", "W", "W", "W", "Pa", "Pa", "molCo2 mol-1Dry",
                               "molH2o mol-1Dry", "-", "-", "K", "K", "K", "YYYY-MM-DD hh:mm:ss.sss")
    names(attributes(data)$unit) <- names(data)
  
  }

  # irga MFC
  if(VarLoca == "irgaMfcSamp") {
    
    data <- data.frame(matrix(data = NaN, ncol = 6, nrow = length(time)))
    names(data) <- c("frt", "frt00", "frtSet00", "presAtm", "temp", "time")
    data$time <- time
    attributes(data)$unit <- c("litersPerMinute", "litersPerMinute", "litersPerMinute", "Pa", "K", "YYYY-MM-DD hh:mm:ss.sss")
    names(attributes(data)$unit) <- names(data)
    
  }

  # soni
  if(VarLoca == "soni") {
    
    data <- data.frame(matrix(data = NaN, ncol = 6, nrow = length(time)))
    names(data) <- c("idx", "time", "veloSoni", "veloXaxs", "veloYaxs", "veloZaxs")
    data$time <- time
    attributes(data)$unit <- c("NA", "YYYY-MM-DD hh:mm:ss.sss", "m s-1", "m s-1", "m s-1", "m s-1")
    names(attributes(data)$unit) <- names(data)
    
  }

  # soniAmrs
  if(VarLoca == "soniAmrs") {
    
    data <- data.frame(matrix(data = NaN, ncol = 14, nrow = length(time)))
    names(data) <- c("accXaxs", "accXaxsDiff", "accYaxs", "accYaxsDiff", "accZaxs", "accZaxsDiff", "angXaxs", "angYaxs", "angZaxs", 
                     "avelXaxs", "avelYaxs", "avelZaxs", "idx", "time" )
    data$time <- time
    attributes(data)$unit <- c("m s-2", "m s-2", "m s-2", "m s-2", "m s-2", "m s-2", "rad", "rad", "rad", "rad s-1", "rad s-1", 
                               "rad s-1", "NA", "YYYY-MM-DD hh:mm:ss.sss")
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
                          name = base::paste0("/", SiteLoca, "/dp0p/data/", VarLoca, "_001/000_060"),
                          read.attributes = TRUE)

    # convert 1-d array list-elements to vector list-elements
    # can be omitted once Dave figures out to store h5 data tables as vector list-elements
    for(idx in base::names(data)) data[[idx]] <- base::as.vector(data[[idx]]); base::rm(idx)
    
    # convert list to data.frame
    data <- base::as.data.frame(data, stringsAsFactors = FALSE)
    # base::attributes(data)$row.names <- NULL
    
    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " hdf5 read-in complete"))

  # convert type of variable time
  data$time <- base::as.POSIXct(data$time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC") + 0.0001

  # assign hdf5 attributes
    
    # read attributes
    attr <- rhdf5::h5readAttributes(file = base::paste0(DirInpLoca, "/ECTE_L0_", SiteLoca, "_", DateLoca, ".h5"),
                                    name = base::paste0("/", SiteLoca, "/dp0p/data/", VarLoca, "_001"))
    
    # which attributes are of type character?
    Pos01 <- base::names(attr)[base::sapply(base::names(attr), function(x) base::is.character(attr[[x]]))]
    
    # split characters by comma separator and trim white spaces
    if(base::length(Pos01) > 0) attr[Pos01] <- base::sapply(Pos01, function(x) base::trimws(base::unlist(base::strsplit(x = attr[[x]], split = ","))))

    # assign variable names to units to enable sorting
    base::names(attr$Unit) <- attr$Name

    # sort and assign unit descriptions in same order as data
    base::attributes(data)$unit <- attr$Unit[base::names(data)]
    rm(attr, Pos01)
    
      # replacement statement for assigning units to individual variables in data
      # # assign units to variables in data
      # for(idx in base::names(data)) base::attr(x = data[[idx]], which = "unit") <- attr$Unit[[idx]]
      # rm(idx)

    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " hdf5 attributes complete"))

  # unit conversion
    
    # perform unit conversion
    data <- base::suppressWarnings(eddy4R.base::def.unit.conv(data = data,
                                                              unitFrom = attributes(data)$unit,
                                                              unitTo = "intl"))

      # replacement statement for performing unit conversion over individual variables in data
      # # perform unit conversion: loop around variables in data
      # for(idx in base::names(data)) {
      #   
      #   data[[idx]] <- base::suppressWarnings(
      #     eddy4R.base::def.unit.conv(data = base::as.vector(data[[idx]]),
      #                                unitFrom = attributes(data[[idx]])$unit,
      #                                unitTo = "intl")
      #   )
      #   
      # }; rm(idx)

    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " unit conversion complete"))

  # regularization
  if(Rglr) {
    
    # perform regularization
    data <- eddy4R.base::def.rglr(
      timeMeas = base::as.POSIXlt(data$time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"),
      dataMeas = data,
      unitMeas = attributes(data)$unit,
      BgnRglr = as.POSIXlt(min(time)),
      EndRglr = as.POSIXlt(max(time)),
      FreqRglr = FreqLoca,
      MethRglr = "CybiEc",
      WndwRglr = "Cntr",
      PosWndw = "Clst"
    )$dataRglr

    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " regularization complete"))

  } else {
    
    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " regularization not selected"))
    
  }
    
  # discard data with bad sensor diagnostic flag
  if(Diag) {
    
    # for selected sensors
    if(VarLoca %in% base::names(RngLoca)) {

      # irga
      if(VarLoca == "irga") {

        # simple implementation based on decimal representation
        whr <- which(data$dataRglr$diag != 8191)
        if(length(whr) > 0) data$dataRglr[whr, c("asrpCO2", "asrpH2O", "rhoMoleCO2", "densMoleH2o", "fdMoleCO2", "rtioMoleDryH2o")] <- NaN
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
    
  } else {
    
    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " sensor diagnostics not selected"))
    
  }

  # range test
  if(Rng) {
    
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
    
  } else {
    
    # print message to screen
    print(paste0(format(Sys.time(), "%F %T"), ": dataset ", DateLoca, ": ", VarLoca, " range test not selected"))
    
  }
  
  # de-spiking

    # perform range test
    if(VarLoca %in% base::names(RngLoca)) {
  
      # loop around variables
      for(idx in base::names(RngLoca[[VarLoca]])) {

        #execute de-spiking algorithm
        data[,idx] <- eddy4R.qaqc::def.dspk.br86(
          # input data, univariate vector of integers or numerics
          dataIn = as.vector(data[,idx]),
          # filter width
          WndwFilt = DespLoca$widt,
          # initial number/step size of histogram bins
          NumBin = DespLoca$nbin,
          # resolution threshold
          ThshReso = DespLoca$rest
        )$dataOut
        
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

