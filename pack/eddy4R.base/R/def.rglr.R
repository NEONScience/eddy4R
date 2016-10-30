##############################################################################################
#' @title Regularizing irregular data to strictly regular / equidistant data

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description Function defintion. 
#' Takes a (potentially) irregularly spaced timeseries \code{timeMeas} of data \code{dataMeas} and returns a strictuly regularly spaced timeseries \code{timeRglr} of data \code{dataRglr}. \strong{ATTENTION}: \code{MethRglr = "zoo"} uses the zoo:na.approx() function, which does not currently abide by its \code{maxgap} argument version 1.7-13. In result, where gaps exist currently the last known value is repeated instead of NAs being inserted. An Email with a request for bugfixing has been sent to \email{Achim.Zeileis@R-project.org} (2016-05-08).  

#' @param \code{timeMeas} A vector containing the observation times. Of class "POSIXlt" including timezone attribute, and of the same length as \code{dataMeas}. [-]
#' @param \code{dataMeas} A named data.frame containing the observations. Columns may be of class "numeric" or "integer", and of the same length as \code{timeMeas}. Columns of classes other than "numeric" or "integer" are removed and not included in the returned \code{dataRegl}. [user-defined]
#' @param \code{unitMeas} A vector containing the unit of each column in \code{dataMeas}. Of class "character". It is recommended to conform to the "unit representation" guidelines documented in the eddy4R.base package.
#' @param \code{BgnRglr} Desired begin time for the regularized dataset. Of class "POSIXlt" including timezone attribute, and \code{length(BgnRglr) = 1}. This input is not used in the "cybiDflt" method. [-]
#' @param \code{EndRglr} Desired end time for the regularized dataset. Of class "POSIXlt" including timezone attribute, and \code{length(EndRglr) = 1}. This input is not used in the "cybiDflt" method. [-]
#' @param \code{TzRglr} Desired timezone for the regularized dataset. Of class "character" and \code{length(TzRglr) = 1}, defaults to the same timezone as \code{BgnRglr}. This input is not used in the "cybiDflt" method. [-]
#' @param \code{FreqRglr} Desired frequency of  the regularized dataset. Of class "numeric" or "integer" and \code{length(FreqRglr) = 1}. [Hz]
#' @param \code{MethRglr} Switch for different regularization methods. Of class "character", currently defaults to "zoo". [-] \cr
#' Method "CybiDflt" implements the default for metereological variable regularization performed by NEON CI. Namely, a new time series is created 
#' from the first measurement time, rounded toward zero, using the expected data frequency. The first measurement falling 
#' in between one time stamp and the next is assigned to the first of these, and all other measurements falling in this range are ignored.\cr
#' Method "CybiRglr" implements the default regularization method for eddy-covariance processing utilized CI. The procedure 
#' is documented in NEON.DOC.001069.\cr
#' @param \code{WndwRglr} Position of the window for binning in the "CybiRglr" method. \code{WndwRglr} can be centered [Cntr], leading [Lead], or trailing [Trlg] (defaults to centered).\cr
#' @param \code{RepWndw} Determines which observation to allocate to a bin if multiple observations fall into a single bin when using the "CybiRglr" method.. \code{RepWndw} can be set to closest [Clst], first [Frst], or last [Last] (defaults to closest).\cr


#' @return Returns a list with elements \code{TzRglr}, \code{FreqRglr}, \code{MethRglr}, \code{timeRglr}, and \code{dataRglr}.

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr
#' NEON.DOC.001069 Preprocessing ATBD: The ATBD that describes the CybiRglr and CybiDflt regularization methods. \cr
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
#' #"CybiRglr" example with multiple observations in a single bin
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
#'   MethRglr = "CybiRglr",
#'   WndwRglr = "Cntr",
#'   RepWndw = "Clst"
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
#     Addition of the new NEON CI regulariztion method outlined
#       in the preprocessing ATBD (NEON.DOC.001069)
##############################################################################################

# start function for regularization
def.rglr <- function(
  timeMeas,
  dataMeas,
  unitMeas=NULL,
  BgnRglr=NULL,
  EndRglr=NULL,
  TzRglr = attributes(BgnRglr)$tzone,
  FreqRglr,
  MethRglr= c("CybiRglr", "cybiDflt", "zoo")[1],
  WndwRglr = c("Cntr", "Lead", "Trlg")[1],
  RepWndw = c("Clst","Frst","Last")[1]
){
  
  
  # assign list for storing the results
  rpt <- base::list()
  rpt$TzRglr <- TzRglr
  rpt$FreqRglr <- FreqRglr
  rpt$MethRglr <- MethRglr
  
  # default: using the zoo::na.approx() function
  # takes 3 s for 1,728,000 observations, i.e. one day of one 20 Hz variable
  # tested to work with types "double" and "integer"; definitly does not work with type "character"
  if(MethRglr == "zoo") {
    
    # Check inputs specific to zoo method
    if(base::is.null(unitMeas)) {
      stop("Input 'unitMeas' is required for the 'zoo' method")
    }
    if(base::is.null(BgnRglr)) {
      stop("Input 'BgnRglr' is required for the 'zoo' method")
    }
    if(base::is.null(TzRglr)) {
      stop("Input 'TzRglr' is required for the 'zoo' method")
    }
    
    
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
    attributes(rpt$dataRglr)$unit <- unitMeas
    
    # end MethRglr == zoo
  }
  
  
  
  # Regularize time series according to default NEON cyber infrastructure (CI) L0 -> L0' procedure. 
  # NEON CI transforms raw L0 data into a regularized time series according to the expected data frequency.
  # Namely, a new time series is created from the first measurement time, rounded toward zero, using the 
  # expected data frequency. The first measurement falling in between one time stamp and the next is assigned
  # to the first of these, and all other measurements falling in this range are ignored. 
  # This code replicates this procedure in order to compare expected output to that produced by CI.
  if(MethRglr == "cybiDflt") {
    
    numVar <- base::length(dataMeas[1,])
    nameVar <- base::names(dataMeas)
    
    # Check timeMeas
    timeMeas <- try(base::as.POSIXct(timeMeas),silent=TRUE)
    numData <- base::length(dataMeas[,1])
    if(base::class(timeMeas)[1] == "try-error"){
      stop("Input variable timeMeas must be of class POSIXlt")
    } else if (base::length(timeMeas) != numData) {
      stop("Length of input variable timeMeas must be equal to the sample size of dataMeas.")
    } 
    
    # Check FreqRglr
    if(!base::is.numeric(FreqRglr) || (base::length(FreqRglr) != 1)) {
      stop("Input parameter FreqRglr must be single number.")
    }
    
    # CI uses the first value as the starting point for the regularization, rounding down to the nearest second
    # Note: the rounding down aspect is a change implemented week of 1 May 2016. Previously the starting point was
    # the exact time (to the decimal second).
    timeRglr <- base::as.POSIXct(base::seq.POSIXt(from=base::trunc.POSIXt(timeMeas[1],units="secs"),
                                                  to=timeMeas[length(timeMeas)]+1/FreqRglr,by=1/FreqRglr))
    
    # Which time bin does each measurement time fit into?
    posRglr <- base::.bincode(timeMeas,timeRglr,right=FALSE) # which bin?
    dataMeas <- base::subset(dataMeas,!base::is.na(posRglr),select=1:numVar) # Get rid of anomalous times/data not fitting in any bin
    timeMeas <- base::subset(timeMeas,!base::is.na(posRglr))
    posRglr <- base::subset(posRglr,!base::is.na(posRglr))
    dupl <- base::duplicated(posRglr) # which fall into an already occupied bin?
    
    # Pull the first value that falls within each bin
    dataRglr <- base::matrix(data=NA*1.5,nrow=length(timeRglr)-1,ncol=numVar) # initialize
    for(idxVar in 1:numVar){
      # place the first value falling into each bin
      dataRglr[posRglr[!dupl],idxVar] <- dataMeas[which(!dupl),idxVar]
    }
    dataRglr <- base::as.data.frame(dataRglr) # Make data frame
    base::names(dataRglr) <- nameVar # Assign names same as dataMeas
    
    # Report output
    timeRglr <- timeRglr[-length(timeRglr)]
    rpt$timeRglr <- base::as.POSIXlt(timeRglr)
    rpt$dataRglr <- dataRglr
    
    # assign unit attributes
    base::attributes(rpt$dataRglr)$unit <- unitMeas
  }
  
  # Method "CybiRglr" implements the default regularization method for eddy-covariance 
  # processing utilized CI. The procedure is documented in NEON.DOC.001069.  
  
  if(MethRglr == "CybiRglr") {
  
    #Check that BgnRglr is initialized; otherwise return error  
    if(base::is.null(BgnRglr)) {
      stop("Input 'BgnRglr' is required for the 'cybiRglr' method")
    }
    #Check that EndRglr is initialized; otherwise return error  
    if(base::is.null(EndRglr)) {
      stop("Input 'EndRglr' is required for the 'cybiRglr' method")
    }
    #Check that TzRglr is initialized; otherwise return error 
    if(base::is.null(TzRglr)) {
      stop("Input 'TzRglr' is required for the 'cybiRglr' method")
    }
    
    # Check FreqRglr is initialized; otherwise return error 
    if(!base::is.numeric(FreqRglr) || (base::length(FreqRglr) != 1)) {
      stop("Input parameter FreqRglr must be single number.")
    }
    
    #Check if RepWndw is set to "closest" the WndwRglr must be set to "centered"
    if(RepWndw == "Clst" & !(WndwRglr == "Cntr")){
      stop("If RepWndw is set to closest the WndwRglr must be set to centered")
    }
    
    # Check timeMeas is class POSIXlt and timeMeas and dataMeas are the same length
    timeMeas <- try(base::as.POSIXct(timeMeas),silent=TRUE)
    numData <- base::length(dataMeas[,1])
    if(base::class(timeMeas)[1] == "try-error"){
      stop("Input variable timeMeas must be of class POSIXlt")
    } else if (base::length(timeMeas) != numData) {
      stop("Length of input variable timeMeas must be equal to the sample size of dataMeas.")
    } 
    
    # add a small amount of time to avoid "down-rounding" by R-internal POSIX
    #timeMeas$sec <- timeMeas$sec + 0.0001
    BgnRglr$sec <- BgnRglr$sec + 0.0001
    EndRglr$sec <- EndRglr$sec + 0.0002
    
    # create equidistant reference time vector
    rpt$timeRglr <- base::as.POSIXlt(seq.POSIXt(from = BgnRglr, to = EndRglr - 1/FreqRglr, by = 1/FreqRglr), tz=TzRglr)
    
    # delete rows with times that are duplicates of rows with smaller indices
    pos01 <- which(base::duplicated(timeMeas))
    if(base::length(pos01) != 0) {
      dataMeas <- dataMeas[-pos01,]
      timeMeas <- timeMeas[-pos01]
    }; base::rm(pos01)
    
    # Number of variables in dataframe
    numVar <- base::ncol(dataMeas)
    # Variable names
    nameVar <- base::names(dataMeas)
    
    # Determine the binning windows based on the choice of WndwRglr
    if(WndwRglr == "Cntr"){
      timeWndw <- as.POSIXct(rpt$timeRglr - (0.5*(1/FreqRglr)))
    } else if (WndwRglr == "Lead"){
      timeWndw <- as.POSIXct(rpt$timeRglr - (1/FreqRglr))
    } else if (WndwRglr == "Trlg"){timeWndw <- as.POSIXct(rpt$timeRglr)}
    
    #Add one extra break to the end for a final bin  
    timeWndw <- c(timeWndw,timeWndw[length(timeWndw)] + 1/FreqRglr)
    
    # Which time bin does each measurement time fit into? Allocating times to bins.
    posRglr <- base::.bincode(timeMeas,timeWndw,right=FALSE) # which bin?
    
    # Get rid of anomalous times/data not fitting in any bin
    dataMeas <- base::subset(dataMeas,!base::is.na(posRglr),select=1:numVar) # Get rid of anomalous times/data not fitting in any bin
    timeMeas <- base::subset(timeMeas,!base::is.na(posRglr)) 
    posRglr <- base::subset(posRglr,!base::is.na(posRglr))
    
    # Checking for multiple values in a single bin with a logic vector 
    if(anyDuplicated(posRglr) > 0){
      if(RepWndw == "Clst"){
        #Determin all duplicates both forward and backward. Otherwise, only duplicates after the first observation of a value are flagged.
        dupl <- base::duplicated(posRglr)|duplicated(posRglr,fromLast = TRUE)
        #Determine vector positions for the duplicate positions
        posDupl <- which(duplicated(posRglr)|duplicated(posRglr,fromLast = TRUE))
        #Determine unique values of Wndw from posRglr for the duplicate positions
        WndwDupl <- posRglr[posDupl]
        WndwDupl <- unique(WndwDupl)
        #Determine the closest values to the regularized timestamp by minimum absolute deviation and change the value in the logic vector.
        posGood <- sapply(WndwDupl, function(x) posDupl[which.min(abs(difftime(rpt$timeRglr[x], timeMeas[posDupl])))])
        dupl[posGood] <- FALSE
      } else if(RepWndw == "Frst"){
        dupl <- base::duplicated(posRglr) # which fall into an already occupied bin with higher indices flagged as duplicates.
      } else if(RepWndw == "Last"){
        dupl <- base::duplicated(posRglr, fromLast = TRUE) # which fall into an already occupied bin with lower indices flagged as duplicates.
      }}else{dupl <- rep(FALSE, length(posRglr))} #If no duplicates exist, all equal FALSE
    
    # Pull the value that chosen by RepWndw within each bin 
    dataRglr <- base::matrix(data=NA*1.5,nrow=length(rpt$timeRglr),ncol=numVar) # initialize, mulitply by 1.5 to give numeric
    for(idxVar in 1:numVar){
      # place the value falling into each bin
      dataRglr[posRglr[!dupl],idxVar] <- dataMeas[which(!dupl),idxVar]
    }
    dataRglr <- base::as.data.frame(dataRglr) # Make data frame
    base::names(dataRglr) <- nameVar # Assign names same as dataMeas
    
    # Report output
    rpt$dataRglr <- dataRglr
    # assign unit attributes
    base::attributes(rpt$dataRglr)$unit <- unitMeas
  }
  
  # return results
  return(rpt)
  
  # end function 
}
