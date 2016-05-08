

#function for regularization
def.rglr <- function(
  timeMeas = base::as.POSIXlt(paste(DATA$irga$date, " ", DATA$irga$time, sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
  dataMeas = DATA$soni,
  unitMeas = attributes(DATA$soni)$units,
  BgnRglr = base::as.POSIXlt(paste(date, " ", "00:00:00.00", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
  EndRglr = base::as.POSIXlt(paste(date, " ", "23:59:59.95", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
  TzRglr = attributes(BgnRglr)$tzone,
  FreqRglr = freq_res,
  MethRglr = "zoo"
){
  
  # assign list for storing the results
  rpt <- list()
  rpt$TzRglr <- TzRglr
  rpt$FreqRglr <- FreqRglr
  rpt$MethRglr <- MethRglr
  
  # default: using the zoo::na.approx() function
  # takes 3 s for 1,728,000 observations, i.e. one day of one 20 Hz variable
  # tested to work with types "double" and "integer"; definitly does not work with type "character"
  if(MethRglr == "zoo") {
    
    # add a small amount of time to avoid "down-rounding" by R-internal POSIX
    timeMeas$sec <- timeMeas$sec + 0.0001
    BgnRglr$sec <- BgnRglr$sec + 0.0001
    EndRglr$sec <- EndRglr$sec + 0.0002
    
    # create equidistant reference time vector
    rpt$timeRglr <- base::as.POSIXlt(seq.POSIXt(from = BgnRglr, to = EndRglr, by = 1/FreqRglr), tz=TzRglr)
    
    # delete rows with times that are duplicates of rows with smaller indices
    whr01 <- which(base::duplicated(timeMeas))
    if(base::length(whr01) != 0) {
      dataMeas <- dataMeas[-whr01,]
      timeMeas <- timeMeas[-whr01]
    }; base::rm(whr01)
    
    # reduce dataMeas to variables that are of type double or integer (not character!)
    whr02 <- base::sapply(1:base::ncol(dataMeas), function(x) base::typeof(dataMeas[[x]]))
    whr02 <- which((whr02 %in% c("double", "integer")))
    dataMeas <- base::subset(dataMeas, select = whr02)
    unitMeas <- unitMeas[whr02]
    base::rm(whr02)
    
    # start loop around variables
    rpt$dataRglr <- base::data.frame(tmp = rpt$timeRglr, stringsAsFactors = FALSE)
    for(idx in base::names(dataMeas)) {
      
      # determine number of non-NAs in averaging period
      whr03 <- base::length(base::which(!base::is.na(dataMeas[,idx])))
      
      # if less than 2 values (minimum required by na.approx() function)
      if(whr03 < 2) {
        
        rpt$dataRglr[,idx] <- base::rep(NaN, base::length(rpt$timeRglr))
        
        #else interpolate dataMeas
      } else {
        
        rpt$dataRglr[,idx] <- zoo::na.approx(object = dataMeas[,idx], x = timeMeas, xout = rpt$timeRglr,
                                             method = "constant", maxgap = 0, na.rm = FALSE, rule = 1, f = 0)
        
      }
      
      # end loop around variables
    }; base::rm(idx, whr03)
    
    #remove temporary variable from data.frame
    rpt$dataRglr <- subset(rpt$dataRglr, select = -tmp)
    
    # assign unit attributes
    attributes(rpt$dataRglr)$unit <- unitMeas
    
    # end MethRglr == zoo
  }
  
  # return results
  return(rpt)
  
  # end function 
}
