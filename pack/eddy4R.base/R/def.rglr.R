##############################################################################################
#' @title Definition function: Regularizing irregular data to strictly regular / equidistant data

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Cove Sturtevant \email{eddy4R.info@gmail.com} \cr
#' David Durden \email{ddurden@battelleecology.org}

#' @description Function defintion. 
#' Takes a (potentially) irregularly spaced timeseries \code{timeMeas} of data \code{dataMeas} and returns a strictuly regularly spaced timeseries \code{timeRglr} of data \code{dataRglr}. \strong{ATTENTION}: \code{MethRglr = "zoo"} uses the zoo:na.approx() function, which does not currently abide by its \code{maxgap} argument version 1.7-13. In result, where gaps exist currently the last known value is repeated instead of NAs being inserted. An Email with a request for bugfixing has been sent to \email{Achim.Zeileis@R-project.org} (2016-05-08).  

#' @param timeMeas A vector containing the observation times. Of class "POSIXlt" including timezone attribute, and of the same row length as \code{dataMeas}. [-]
#' @param dataMeas A named data.frame containing the observations, with row length matching that of \code{timeMeas}. Not that if the zoo method is chosen in \code{MethRglr} or input \code{DropNotNumc} is TRUE, any columns with class other than "numeric" or "integer" are removed and not included in the returned \code{dataRegl}. [user-defined]
#' @param unitMeas A vector containing the unit of each column in \code{dataMeas}. Of class "character". It is recommended to conform to the "unit representation" guidelines documented in the eddy4R.base package. 
#' @param BgnRglr Desired begin time for the regularized dataset. Of class "POSIXlt" including timezone attribute, and \code{length(BgnRglr) = 1}. [-]
#' @param EndRglr Desired end time for the regularized dataset. Of class "POSIXlt" including timezone attribute, and \code{length(EndRglr) = 1}. [-]
#' @param TzRglr Desired timezone for the regularized dataset. Of class "character" and \code{length(TzRglr) = 1}, defaults to the same timezone as \code{BgnRglr}. [-]
#' @param FreqRglr Desired frequency of  the regularized dataset. Of class "numeric" or "integer" and \code{length(FreqRglr) = 1}. [Hz]
#' @param MethRglr Switch for different regularization methods. Of class "character", currently defaults to "CybiEc". [-] \cr
#' Method "CybiEc" implements the default regularization method for eddy-covariance processing utilized CI. The procedure 
#' is documented in NEON.DOC.001069.\cr
#' Method "CybiEcTimeMeas" is a modification of CybiEc that replaces the regularized timestamp with the actual value of \code{timeMeas} for the observation (if any) that was selected following the binning options specified in \code{WndwRglr} and \code{idxWndw}. \cr
#' Method "zoo" implements the regularization method using the zoo::na.approx function. This method can only handle up to millisecond precision (PrcsSec=3)
#' @param WndwRglr Position of the window for binning in the "CybiEc" method. \code{WndwRglr} can be centered [Cntr], leading [Lead], or trailing [Trlg] (defaults to centered).\cr
#' @param IdxWndw Determines which observation to allocate to a bin if multiple observations fall into a single bin when using the "CybiEc" method.. \code{IdxWndw} can be set to closest [Clst], first [IdxWndwMin], or last [IdxWndwMax] (defaults to closest).\cr
#' @param DropNotNumc Logical. TRUE (default) for removing any non-numeric data columns prior to regularization (this is done automatically for zoo method). FALSE to attempt to regularize all data columns.
#' @param RptTimeWndw Logical. TRUE for including the start and end time of each bin with the output, in list element timeWndw. Defaults to FALSE. Not available as TRUE for zoo method.
#' @param PrcsSec A single numeric (integer) value indicating the operational precision of the seconds field of time vectors. Defaults to 6 (microsecond-precision). Values higher than 6 cannot be guaranteed to produce desired results.


#' @return Returns a list with elements \code{TzRglr}, \code{FreqRglr}, \code{MethRglr}, \code{timeRglr}, and \code{dataRglr}. 
#' An additional list element \code{timeWndw} will be included if input \code{RptTimeWndw=TRUE}

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON.DOC.001069 Preprocessing ATBD: The ATBD that describes the CybiEc regularization method. \cr
#' 
#' @keywords regularization, equidistant, preprocessing

#' @examples
#' # make sure that fractional seconds can be seen from the console
#' options(digits.secs=3)
#' # assign measured time vector
#' timeMeas <- base::as.POSIXlt(seq.POSIXt(
#'   from = base::as.POSIXlt("2016-01-01 00:00:00.001", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   to = base::as.POSIXlt("2016-01-01 00:00:01.002", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   by = 1/10), tz = "UTC")[-c(5,6,8)]
#' # assign fake observations
#' dataMeas <- base::data.frame("wind01" = rnorm(base::length(timeMeas)), "wind02" = rnorm(base::length(timeMeas)))
#' # regularize
#' def.rglr(
#'   timeMeas = timeMeas,
#'   dataMeas = dataMeas,
#'   unitMeas = c("metersPerSecond", "metersPerSecond"),
#'   BgnRglr = base::as.POSIXlt("2016-01-01 00:00:00.000", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   EndRglr = base::as.POSIXlt("2016-01-01 00:00:01.000", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   FreqRglr = 10,
#'   MethRglr = "zoo"
#' )
#' 
#' #"CybiEc" example with multiple observations in a single bin
#' timeMeas[3] <- timeMeas[3] - 0.03
#' timeMeas[2] <- timeMeas[2] - 0.03
#' timeMeas[1] <- timeMeas[1] + 0.05
#' timeMeas[7] <- timeMeas[7] - 0.031
#' timeMeas[8] <- timeMeas[8] - 0.081
#' 
#' #Regularize with a centered window and chosing the closest value to the regularized timestamp. 
#' def.rglr(
#'   timeMeas = timeMeas,
#'   dataMeas = dataMeas,
#'   unitMeas = c("metersPerSecond", "metersPerSecond"),
#'   BgnRglr = base::as.POSIXlt("2016-01-01 00:00:00.000", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   EndRglr = base::as.POSIXlt("2016-01-01 00:00:01.000", format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
#'   FreqRglr = 10,
#'   MethRglr = "CybiEc",
#'   WndwRglr = "Cntr",
#'   IdxWndw = "Clst"
#' )  

#' @seealso ?zoo:na.approx, ?stats::approx

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2016-05-08)
#     original creation
#   Cove Sturtevant (2016-05-19)
#     Addition of NEON CI default regularization procedure
#     Added Null defaults for unitMeas, BgnRglr, and EndRglr 
#     Added checks on inputs specific to "zoo" method
#   Cove Sturtevant (2016-07-15)
#     Drastically improved computational time for cybiDflt
#        by switching to .bincode function for determining 
#   Dave Durden (2016-10-21)
#     Addition of the new NEON CI regulariztion method "CybiEc" outlined
#       in the preprocessing ATBD (NEON.DOC.001069); 75% faster compared to method "zoo"
#   Stefan Metzger (2016-11-02)
#     method "CybiEc": fixed equidistant time vector, dropped character variables
#   Cove Sturtevant (2017-09-27)
#     code overhaul to use numeric-based time representation within the code to allow micro-second time precision
#     this allowed removal of the small fraction of a second added to time vectors in the CybiEc and zoo regularization methods
#     added more complete error checking
#   Cove Sturtevant (2017-12-22)
#     Added error checking for empty data when using cybiDflt method
#   Cove Sturtevant (2018-05-23)
#     Changed term 'pos' to 'idx' for single indices, to 'set' for multiple indices
#   Cove Sturtevant (2020-02-18)
#     Removed MethRglr "cybiDflt", as it is no longer used by NEON CI (CybiEc is used)
#     Added MethRglr "CybiEcTimeMeas"
#   Cove Sturtevant (2021-02-02)
#     Added option to retain non-numeric columns for everything but zoo method
#     Added option to output the time bins (start and end times) in a new list element in the output
#   Cove Sturtevant (2021-02-15)
#     bug fix. Sometimes class returns 'array', which was causing an error when forcing the type of 
#     each regularized variable to the same as that in the input data frame. 
#     Replaced 'class' with 'typeof' to fix. 
##############################################################################################

def.rglr <- function(
  timeMeas,
  dataMeas,
  unitMeas=base::attributes(dataMeas)$unit,
  BgnRglr=NULL,
  EndRglr=NULL,
  TzRglr = base::attributes(BgnRglr)$tzone,
  FreqRglr,
  MethRglr= c("CybiEc", "CybiEcTimeMeas", "zoo")[1],
  WndwRglr = c("Cntr", "Lead", "Trlg")[1],
  IdxWndw = c("Clst","IdxWndwMin","IdxWndwMax")[1],
  DropNotNumc = TRUE,
  RptTimeWndw = FALSE,
  PrcsSec = 6
){
  
  
  # Error-check
  if(!(WndwRglr %in% c("Cntr","Lead","Trlg"))){
    stop(base::paste0('Unrecognized value for input WndwRglr. Options are "Cntr","Lead","Trlg" (case-sensitive). This parameter is used only for MethRglr=CybiEc.'))
  }

  # Error-check
  if(!(IdxWndw %in% c("Clst","IdxWndwMin","IdxWndwMax"))){
    stop(base::paste0('Unrecognized value for input IdxWndw. Options are "Clst","IdxWndwMin","IdxWndwMax" (case-sensitive). This parameter is used only for MethRglr=CybiEc.'))
  }
  
  # Error-check
  if(!(MethRglr %in% c("zoo","CybiEcTimeMeas","CybiEc"))){
    stop(base::paste0('Unrecognized value for input MethRglr. Options are "zoo","CybiEc", and "CybiEcTimeMeas" (case-sensitive)'))
  }
  
  # Error-check
  if(MethRglr == "zoo" && DropNotNumc == FALSE){
    warning('Input parameter DropNotNumc is always set to TRUE for MethRglr=zoo. User selected input has been overwritten.')
  }

  # Error-check
  if(MethRglr == "zoo" && RptTimeWndw == TRUE){
    warning('Input parameter RptTimeWndw = TRUE is not currently an option for MethRglr=zoo. User selected input has been overwritten.')
  }

  
  if(MethRglr %in% c("zoo","CybiEcTimeMeas","CybiEc")){
    if(!("POSIXlt" %in% base::class(timeMeas))){
      stop("Input 'timeMeas' must be in POSIXlt")
    }
    if(!("POSIXlt" %in% base::class(BgnRglr))){
      stop("Input 'BgnRglr' must be in POSIXlt")
    }
    if(!("POSIXlt" %in% base::class(EndRglr))){
      stop("Input 'EndRglr' must be in POSIXlt")
    }    
  }

  if(MethRglr == "zoo") {
    
    if(base::is.null(BgnRglr)) {
      stop("Input 'BgnRglr' is required for the 'zoo' method")
    }
    if(base::is.null(TzRglr)) {
      stop("Input 'TzRglr' is required for the 'zoo' method")
    }
  }
  
  if(MethRglr == "CybiEc") {
    
    #Check that BgnRglr is initialized; otherwise return error  
    if(base::is.null(BgnRglr)) {
      stop("Input 'BgnRglr' is required for the 'CybiEc' method")
    }
    #Check that EndRglr is initialized; otherwise return error  
    if(base::is.null(EndRglr)) {
      stop("Input 'EndRglr' is required for the 'CybiEc' method")
    }
    #Check that TzRglr is initialized; otherwise return error 
    if(base::is.null(TzRglr)) {
      stop("Input 'TzRglr' is required for the 'CybiEc' method")
    }
    
    #Check if IdxWndw is set to "closest" the WndwRglr must be set to "centered"
    if(IdxWndw == "Clst" & !(WndwRglr == "Cntr")){
      stop("If IdxWndw is set to closest the WndwRglr must be set to centered")
    }
  }
 
  numData <- base::length(dataMeas[,1])
  numVar <- base::length(dataMeas[1,])
  nameVar <- base::names(dataMeas)
  
  if (base::length(timeMeas) != numData) {
    stop("Length of input variable timeMeas must be equal to the sample size of dataMeas.")
  } 
  
  # Check FreqRglr
  if(!base::is.numeric(FreqRglr) || (base::length(FreqRglr) != 1)) {
    stop("Input parameter FreqRglr must be single number.")
  }
  
  # Check PrcsSec
  if(!(base::class(PrcsSec) %in% c('integer','numeric'))){
    PrcsSec <- 6
  }
  PrcsSec <- round(PrcsSec) # seconds precision, ensure integer value
  
  # Zoo method cannot handle seconds precision greater than 3 (returns NA for all values...)
  if(MethRglr == 'zoo' && PrcsSec > 3){
    PrcsSec <- 3
  }

  # POSIX time has some issues with sub-second precision, often rounding down to a lower value without an 
  # obvious reason. As a result, use numeric representation of time and round to a specified precision. 
  # When returning to POSIX time, ensure use of POSIXlt so that down-rounding does not occur.
  ParaPrcs <- base::options('digits.secs') # Discover what it is set to, so we can return it to this later
  base::options(digits.secs = PrcsSec)
  epoc <- as.POSIXct(base::trunc.POSIXt(BgnRglr,units="secs")) # Set the epoc to the start time
  epocNumc <- base::as.double(epoc)
  timeBgnNumc <- base::round(base::as.double(BgnRglr)-epocNumc,digits=PrcsSec) # Seconds since start time
  timeEndNumc <- base::round(as.double(EndRglr)-epocNumc,digits=PrcsSec) # Seconds since start time
  timeRglrNumc <- base::round(base::seq(from=timeBgnNumc,to=timeEndNumc,by=1/FreqRglr),digits=PrcsSec)
  secRtio <- base::round(timeRglrNumc-base::floor(timeRglrNumc),digits=PrcsSec) # Grab the fractional sections
  timeRglr <- base::as.POSIXlt(base::floor(timeRglrNumc),tz=TzRglr,origin=epoc,digits=20) # Convert to POSIXlt
  timeRglr$sec <- timeRglr$sec+secRtio # Add back in the fractional seconds
  
  # Convert timeMeas to numeric seconds since epoc
  timeMeasNumc <- base::round(base::as.double(timeMeas)-epocNumc,digits=PrcsSec)
  
  
  # assign list for storing the results
  rpt <- base::list()
  rpt$TzRglr <- TzRglr
  rpt$FreqRglr <- FreqRglr
  rpt$MethRglr <- MethRglr
  rpt$timeRglr <- timeRglr
  numRglr <- length(timeRglr)
  
  # default: using the zoo::na.approx() function
  # takes 3 s for 1,728,000 observations, i.e. one day of one 20 Hz variable
  # tested to work with types "double" and "integer"; definitly does not work with type "character"
  if(MethRglr == "zoo") {
    
    # delete rows with times that are duplicates of rows with smaller indices
    #whr01 <- which(base::duplicated(timeMeasNumc))
    whr01 <- !base::duplicated(timeMeasNumc)
    if(base::sum(!whr01) != 0) {
      # dataMeas <- dataMeas[-whr01,]
      # timeMeasNumc <- timeMeasNumc[-whr01]
      dataMeas <- base::subset(x=dataMeas,subset=whr01,select=base::rep(TRUE,numVar))
      timeMeasNumc <- timeMeasNumc[whr01]
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
        
        rpt$dataRglr[,idx] <- base::rep(NaN, numRglr)
        
        #else interpolate dataMeas
      } else {
        
        rpt$dataRglr[,idx] <- zoo::na.approx(object = dataMeas[,idx], x = timeMeasNumc, xout = timeRglrNumc,
                                             method = "constant", maxgap = 0, na.rm = FALSE, rule = 1, f = 0)
        # example for current maxgap bug
        # xout <- 1:10
        # x = xout[-c(4:8)]
        # object = rnorm(length(x))
        # zoo::na.approx(object = object, x = x, xout = xout, method = "constant", maxgap = 0, na.rm = FALSE, rule = 1, f = 0)
        
      }
      
      # end loop around variables
    }; base::rm(idx, whr03)
    
    #remove temporary variable from data.frame
    rpt$dataRglr <- subset(rpt$dataRglr, select = -tmp)
    
    # assign unit attributes
    if(!base::is.null(unitMeas)){
      base::attributes(rpt$dataRglr)$unit <- unitMeas
    }
    
    # end MethRglr == zoo
  }
  
  # Method "CybiEc" implements the default regularization method for eddy-covariance 
  # processing utilized CI. The procedure is documented in NEON.DOC.001069.  
  if(MethRglr %in% c("CybiEc","CybiEcTimeMeas")) {
    
    # delete rows with times that are duplicates of rows with smaller indices
    set01 <- !base::duplicated(timeMeasNumc)
    if(base::sum(!set01) != 0) {
      dataMeas <- base::subset(x=dataMeas,subset=set01,select=base::rep(TRUE,numVar))
      timeMeasNumc <- timeMeasNumc[set01]
    }; base::rm(set01)
    
    
    # reduce dataMeas to variables that are of type double or integer (not character!)
    if(DropNotNumc == TRUE){
      set02 <- base::sapply(1:base::ncol(dataMeas), function(x) base::typeof(dataMeas[[x]]))
      set02 <- which((set02 %in% c("double", "integer")))
      dataMeas <- base::subset(dataMeas, select = set02)
      unitMeas <- unitMeas[set02]
      base::rm(set02)
    }
      
    # Number of variables in dataframe
    numVar <- base::ncol(dataMeas)
    # Variable names
    nameVar <- base::names(dataMeas)
    
    # Determine the binning windows based on the choice of WndwRglr
    if(WndwRglr == "Cntr"){
      timeWndw <- timeRglrNumc - (0.5*(1/FreqRglr))
    } else if (WndwRglr == "Lead"){
      timeWndw <- timeRglrNumc - (1/FreqRglr)
    } else if (WndwRglr == "Trlg"){
      timeWndw <- timeRglrNumc
    }
    
    #Add one extra break to the end for a final bin  
    timeWndw <- base::round(c(timeWndw,timeWndw[length(timeWndw)] + 1/FreqRglr),digits=PrcsSec)
    
    # Which time bin does each measurement time fit into? Allocating times to bins.
    if(WndwRglr == "Lead"){
      # If the window is leading, include values equal to the right edge of the bin
      idxRglr <- base::.bincode(timeMeasNumc,timeWndw,right=TRUE) # which bin?
    } else {
      # If the window is trailing or centered, do not include values at the right edge of the bin
      idxRglr <- base::.bincode(timeMeasNumc,timeWndw,right=FALSE) # which bin?
      
    }
    
    # Get rid of anomalous times/data not fitting in any bin
    dataMeas <- base::subset(dataMeas,!base::is.na(idxRglr),select=1:numVar) # Get rid of anomalous times/data not fitting in any bin
    timeMeasNumc <- base::subset(timeMeasNumc,!base::is.na(idxRglr)) 
    idxRglr <- base::subset(idxRglr,!base::is.na(idxRglr))
    
    # Checking for multiple values in a single bin with a logic vector 
    if(anyDuplicated(idxRglr) > 0){
      if(IdxWndw == "Clst"){
        #Determin all duplicates both forward and backward. Otherwise, only duplicates after the first observation of a value are flagged.
        dupl <- base::duplicated(idxRglr)|duplicated(idxRglr,fromLast = TRUE)
        #Determine vector positions for the duplicate positions
        setDupl <- which(duplicated(idxRglr)|duplicated(idxRglr,fromLast = TRUE))
        #Determine unique values of Wndw from idxRglr for the duplicate positions
        WndwDupl <- idxRglr[setDupl]
        WndwDupl <- unique(WndwDupl)
        #Determine the closest values to the regularized timestamp by minimum absolute deviation and change the value in the logic vector.
        idxGood <- sapply(WndwDupl, function(x) setDupl[which.min(abs(timeRglrNumc[x]-timeMeasNumc[setDupl]))])
        dupl[idxGood] <- FALSE
      } else if(IdxWndw == "IdxWndwMin"){
        dupl <- base::duplicated(idxRglr) # which fall into an already occupied bin with higher indices flagged as duplicates.
      } else if(IdxWndw == "IdxWndwMax"){
        dupl <- base::duplicated(idxRglr, fromLast = TRUE) # which fall into an already occupied bin with lower indices flagged as duplicates.
      }}else{dupl <- rep(FALSE, length(idxRglr))} #If no duplicates exist, all equal FALSE
    
    # Pull the value that chosen by IdxWndw within each bin 
    classData <- lapply(dataMeas,base::class) # Get the type of each variable so we can make sure the output gets the same
    typeData <- lapply(dataMeas,base::typeof)
    dataRglr <- base::data.frame(base::matrix(data=NA*1.5,nrow=numRglr,ncol=numVar)) # initialize, multiply by 1.5 to give numeric
    for(idxVar in 1:numVar){
      # Give the column its original class
      base::class(dataRglr[[idxVar]]) <- tryCatch(
        base::class(dataRglr[[idxVar]]) <- classData[[idxVar]],
        error=function(e){base::class(dataRglr[[idxVar]]) <- typeData[[idxVar]]})
        
      
      # place the value falling into each bin
      dataRglr[idxRglr[!dupl],idxVar] <- dataMeas[which(!dupl),idxVar]
    }
    base::names(dataRglr) <- nameVar # Assign names same as dataMeas

    # For CybiEcTimeMeas, replace the regularized timestamps with the actual timestamps of the measured values selected for each bin
    if(MethRglr == 'CybiEcTimeMeas'){
      rpt$timeRglr[idxRglr[!dupl]] <- timeMeas[which(!dupl)]
    }
    
    if(RptTimeWndw==TRUE){
      secRtioWndw <- base::round(timeWndw-base::floor(timeWndw),digits=PrcsSec) # Grab the fractional seconds
      timeWndw <- base::as.POSIXlt(base::floor(timeWndw),tz=TzRglr,origin=epoc,digits=20) # Convert to POSIXlt
      timeWndw$sec <- timeWndw$sec+secRtioWndw # Add back in the fractional seconds
      rpt$timeWndw <- data.frame(timeWndwBgn=timeWndw[1:numRglr],timeWndwEnd=timeWndw[2:(numRglr+1)])
    }
    
    
    # Report output
    rpt$dataRglr <- dataRglr
    # assign unit attributes
    base::attributes(rpt$dataRglr)$unit <- unitMeas
    
  }
  
  # Return to the previously set second's precision
  base::options(digits.secs = ParaPrcs$digits.secs)
  
  # return results
  return(rpt)
  
  # end function 
}
