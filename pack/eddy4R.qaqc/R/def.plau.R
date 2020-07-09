##############################################################################################
#' @title Definition function: Plausibility tests (Range, Step, Persistence, Null, Gap) 

#' @author
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description 
#' Function definition. Determines inplausible data indices based on user-specified limits for the data range, step between adjacent values, persistence (similarity of adjacent values), nulls, and gaps.

#' @param data Required input. A data frame containing the data to be evaluated (do not include the time stamp vector here). 
#' @param time Optional. A time vector of class POSIXlt of times corresponding with each row in data. Defaults to an evenly spaced time vector starting from system time of execution by seconds. 
#' @param RngMin Optional. A numeric vector of length equal to number of variables in data containing the minimum acceptable value for each variable. Defaults to observed minimums (no flags will result)
#' @param RngMax Optional. A numeric vector of length equal to number of variables in data containing the maximum acceptable value for each variable. Defaults to observed maximums (no flags will result)
#' @param DiffStepMax Optional. A numeric vector of length equal to number of variables in data containing the maximum acceptable absolute difference between sequential data points for each variable. Defaults to observed maximum (no flags will result)
#' @param DiffPersMin Optional. A numeric vector of length equal to number of variables in data containing the minimum absolute change in value over the interval specified in WndwPers to indicate the sensor is not "stuck". Defaults to a vector of zeros (no flags will result).
#' @param WndwPers Optional. The time window for evaluting the persistence test. This must be a vector of length equal to number of variables in data. If the values are numeric (integer), then WndwPers specifies number of data points over which to test for the minimum absolute change in value specified in DiffPersMin. 
#' If the vector is a difftime object (e.g. as.difftime(5,units="secs")), it specifies the time interval over which to test for the minimum absolute change in value specified in DiffPersMin. The results are the same if the time-based window exactly corresponds to an integer number of data points.
#' Defaults to a difftime object of 60 x median observed time difference. 
#' @param TestNull Optional. Apply the null test? A logical vector of [TRUE or FALSE] of length equal to number of variables in data. Defaults to FALSE (no null values are flagged)
#' @param NumGap Optional.  A numeric value >= 1, interpretable as an integer, specifying the numer of consecutive NA values constituting a gap. Default is the one more than the length of the data series (no gaps will be flagged)
#' @param Vrbs Optional. A logical {FALSE/TRUE} value indicating whether to:\cr
#' \code{Vrbs = FALSE}: (Default) output the vector positions of the fail and na results for each test (default), or \cr
#' \code{Vrbs = TRUE}: output a data frame for each variable in data, with a column for each plausibility test outputting the actual quality flags [-1,0,1] for each data point

#' @return If:\cr
#' \code{Vrbs = FALSE} A list of variables in \code{data}, nested within each a list of qf test names: (\code{setQfRng}), Step (\code{setQfStep}), Persistence  (\code{setQfPers}), Null (\code{setQfNull}), and Gap(\code{setQfGap}) tests.
#' Each flag is itself a nested list of failed and na (unable to eval) flagged vector positions (e.g. \code{$X$setQfRng$fail} and \code{$X$setQfRng$na} \cr
#' \code{Vrbs = TRUE} A list of variables matching those in \code{data}, each containing a data frame with a column for each plausibility test outputting the actual quality flags [-1,0,1] for each data point, where -1 indicates the test could not be evaluated, 0 indicates a pass, and 1 indicates a fail
#' 

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007 \cr
#' NEON Algorithm Theoretical Basis Document QA/QC Plausibility Testing (NEON.DOC.011081)

#' @keywords NEON QAQC, plausibility, range, step, persistence, null, gap

#' @examples 
#' data <- data.frame(x=rnorm(1000,mean=0,sd=1)) # Start off with a vector of 1000 random values
#' data$x[c(20,50,500,90)] <- 50 # insert some spikes
#' data$x[600:699] <- rnorm(100,mean=0,sd=0.001) # Add some "stuck" data
#' data$x[800:810] <- NA
#' RngMin <- -4
#' RngMax <- 4
#' DiffStepMax <- 6
#' DiffPersMin <- 0.1
#' WndwPers <- as.difftime(10,units="secs") # We are using the default time variable, which generates a freq of 1 second
#' TestNull <- TRUE
#' NumGap <- 11
#' Vrbs=TRUE
#' qf <- eddy4R.qaqc::def.plau(data=data,RngMin=RngMin,RngMax=RngMax,DiffStepMax=DiffStepMax,DiffPersMin=DiffPersMin,WndwPers=WndwPers,TestNull=TestNull,NumGap=NumGap,Vrbs=Vrbs)


#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2015-12-30)
#     original creation
#   Cove Sturtevant (2015-01-06)
#     update to include indices where tests unable to be evaluated, and corrected some
#     variable names to conform to EC-TES naming convention
#   Cove Sturtevant (2015-02-09)
#     fixed bug in persistence test computation causing large blocks of NA to fail
#   Cove Sturtevant (2015-02-26)
#     adjusted gap test to reflect current NEON practice - not based on time difference
#        between measurements, but rather consecutive number of NA values
#     adjusted header to conform with eddy4R coding convention
#   Cove Sturtevant (2016-11-03)
#     added a Vrbs option to allow output of flag values (-1,0,1) rather than vector positions of failed and NA tests
#     also fix duplicate NA and fail indices for step and persistence tests
#   Cove Sturtevant (2016-11-09)
#     adjusted output of vector positions of failed and na spike positions (Vrbs = FALSE) to be nested 
#        under each variable rather than each variable nested under the lists of failed and na results 
#   Cove Sturtevant (2017-07-14)
#     Fixed bug causing neverending loop when all data NA
#     Added example
#     Fixed bug in step test not flagging the first point if NA
#   Cove Sturtevant (2017-07-18)
#     Overhauled code to optimize performance of persistence test by implementing a windowing approach based 
#         on number of points vs. time. The time-based approach is retained as an option, and has been 
#         optimized for cpu time. Output is identical if the time-based approach corresponds to an exact 
#         number of data points. Processing time has been reduced by >90%.
#   Natchaya P-Durden (2018-04-03)
#     update @param format
#   Natchaya P-Durden (2018-04-04)
#    applied eddy4R term name convention; replaced posQf by setQf
#   Natchaya P-Durden (2018-04-11)
#    applied eddy4R term name convention; replaced pos by idx or set
#   Cove Sturtevant (2020-07-07)
#     optimized time-based persistence test and removed point-based method, adding conversion to time-based method. 
#     also caught bug causing a potential lack of persistence test na flags when all values between 2 points are NA.
#   Cove Sturtevant (2020-07-08)
#     optimized gap test. WAY faster when there are a lot of gaps in the dataset
##############################################################################################
def.plau <- function (
  data,                               # a data frame containing the data to be evaluated (do not include the time stamp vector here). Required input.
  time = as.POSIXlt(seq.POSIXt(from=Sys.time(),by="sec",length.out=length(data[,1]))),  # time vector corresponding with the rows in data, in  Class "POSIXlt", which is a named list of vectors representing sec, min, hour,day,mon,year. Defaults to an evenly spaced time vector starting from execution by seconds.
  RngMin = apply(data,2,min,na.rm=TRUE), # a numeric vector containing the minimum acceptable value for each variable in data, defaults to observed minimums
  RngMax = apply(data,2,max,na.rm=TRUE), # a numeric vector containing the maximum acceptable value for each variable in data, defaults to observed maximums
  DiffStepMax = apply(abs(apply(data,2,diff)),2,max,na.rm=TRUE), # a vector containing the maximum acceptable absolute difference between sequential data points for each variable in data
  DiffPersMin = rep.int(0,length(data)), # a vector containing the minimum absolute change in value for each variable in data over the interval specified in WndwPers. Defaults to a vector of zeros.
  WndwPers = 60*median(abs(diff(time)),na.rm=TRUE)*rep.int(1,length(data)), # a vector of class difftime specifying the time interval for each variable in data over which to test for the minimum absolute change in value specified in DiffPersMin. Defaults to 60 x median observed time difference. Class difftime can be generated using as.difftime.
  TestNull = rep(FALSE,length(data)), # apply the null test? A logical vector of [TRUE or FALSE] of length equal to number of variables in data. Defaults to FALSE (no null values are flagged)
  NumGap = rep(length(data[,1])+1,length(data)), # an integer greater than 0 specifying the number of consecutive NA values that constitute a gap
  Vrbs = FALSE # FALSE = output the vector positions of the fail and na results, TRUE = output flag values for each test
) {
  
  
  # Error Checking ----------------------------------------------------------
  
  # Check data
  if(missing("data") | !is.data.frame(data)) {
    stop("Required input 'data' must be a data frame")
  }
  
  # Initial stats
  numVar <- length(data) # Get number of variables 
  nameData <- names(data) # Get variable names
  numData <- length(data[,1])
  
  
  # Check time
  time <- try(as.POSIXlt(time),silent=TRUE)
  if(class(time)[1] == "try-error"){
    stop("Input variable time must be of class POSIXlt")
  } else if (length(time) != numData) {
    stop("Length of input variable time must be equal to length of data.")
  } 
  
  # Check RngMin & RngMax
  if((!is.numeric(RngMin)) | (!is.numeric(RngMax))) {
    stop("Input parameters RngMin and RngMax must be numeric vectors.")
  } else if ((length(RngMin) != numVar) | (length(RngMax) != numVar)) {
    warning("Length of input parameters RngMin or RngMax not equal to number of data variables. Using first element of each for all variables.")
    RngMin <- rep(RngMin[1],numVar)
    RngMax <- rep(RngMax[1],numVar)
  }
  
  # Check DiffStepMax
  if(!is.numeric(DiffStepMax)) {
    stop("Input parameter DiffStepMax must be a numeric vector.")
  } else if (length(DiffStepMax) != numVar) {
    warning("Length of input parameter DiffStepMax not equal to number of data variables. Using first element of DiffStepMax for all variables.")
    DiffStepMax <- rep(DiffStepMax[1],numVar)
  }
  
  # Check DiffPersMin
  if((!is.numeric(DiffPersMin))) {
    stop("Input parameter DiffPersMin must be a numeric vector.")
  } else if (length(DiffPersMin) != numVar) {
    warning("Length of input parameter DiffPersMin not equal to number of data variables. Using first element of DiffPersMin for all variables.")
    DiffPersMin <- rep(DiffPersMin[1],numVar)  
  }
  
  # Check WndwPers
  if(!(class(WndwPers) %in% c("difftime","numeric","integer"))){
    stop("Input parameter WndwPers must be of class difftime, numeric, or integer.")
  }
  if(class(WndwPers) %in% c("integer","numeric") && WndwPers-as.integer(WndwPers) > 0){
    stop("If input parameter WndwPers is numeric, it must be an integer.")
  }
  if (length(WndwPers) != numVar) {
    warning("Length of input parameter WndwPers not equal to number of data variables. Using first element of WndwPers for all variables.")
    WndwPers <- WndwPers[1]*rep.int(1,numVar)  
  } 
  
  # Check TestNull
  if(!is.logical(TestNull)) {
    stop("Input parameter TestNull must be a logical vector.")
  } else if (length(TestNull) != numVar) {
    warning("Length of input parameter TestNull not equal to number of data variables. Using first element of TestNull for all variables.")
    TestNull <- rep(TestNull[1],numVar)
  }
  
  # Check NumGap
  if (length(NumGap) != numVar) {
    warning("Length of input parameter NumGap not equal to number of data variables. Using first element of NumGap for all variables.")
    NumGap <- rep(NumGap[1],numVar) 
  }
  if (!is.numeric(NumGap)) {
    stop("Input parameter NumGap must be a numeric vector.")
  } else if(length(which(NumGap < 1)) > 0) {
    warning("Elements of input parameter NumGap must be integers >= 1, setting values < 1 to 1.")
    NumGap[which(NumGap < 1)] <- 1
  } else if(length(which(NumGap-floor(NumGap) > 0)) > 0) {
    warning("Some or all elements of input parameter NumGap are not integers, these will be rounded toward zero.")
    NumGap <- floor(NumGap)
  } 
  
  
  
  # Perform QAQC tests ------------------------------------------------------
  
  # intialize output of failed and na vector positions
  setQf <- vector("list",numVar) # Initialize output for each variable
  names(setQf) <- nameData
  dataNa <- lapply(data,FUN=function(var){is.na(var)})# logical
  dataReal <- lapply(dataNa,FUN=function(var){!var}) # logical
  setDataReal <- lapply(dataReal,FUN=function(var){which(var)}) # indices
  setDataNa <- lapply(dataNa,FUN=function(var){which(var)}) # indices
  
  # For verbose option, initialize output
  if(Vrbs) {
    # set up a data frame for each variable containing all the plausibility tests
    qfTmp = matrix(data=0,nrow=numData,ncol=5) # Default to pass for each test
    qfTmp <- as.data.frame(qfTmp)
    names(qfTmp) <- c("qfRng","qfStep","qfPers","qfNull","qfGap")
    
    # Dole out the qfs to each variable 
    qf <- vector("list",length=numVar)
    names(qf) <- nameData
    qf <- lapply(qf,FUN = function(x){x=qfTmp})
  }
  
  # Do range test
  for(idxVar in 1:numVar) {
    setQf[[idxVar]]$setQfRng <- list(fail=numeric(0),na=numeric(0)) # initialize
    setQf[[idxVar]]$setQfRng$fail <- which((data[,idxVar] < RngMin[idxVar]) | (data[,idxVar] > RngMax[idxVar]))
    setQf[[idxVar]]$setQfRng$na <- setDataNa[[idxVar]]
    
    # For Verbose option, output actual flag values
    if(Vrbs) {
      qf[[idxVar]]$qfRng[setQf[[idxVar]]$setQfRng$fail] <- 1
      qf[[idxVar]]$qfRng[setQf[[idxVar]]$setQfRng$na] <- -1
    }
  }
  
  
  # Do step test
  for(idxVar in 1:numVar) {
    
    diffDataIdx <- diff(data[,idxVar])
    diffDataNaIdx <- is.na(diffDataIdx)
    setNaDiffDataIdx <- which(diffDataNaIdx)
    setRealDiffDataIdx <- which(!diffDataNaIdx)
    
    
    setQf[[idxVar]]$setQfStep <- list(fail=numeric(0),na=numeric(0)) # initialize
    
    setQf[[idxVar]]$setQfStep$fail <- which((abs(diffDataIdx) > DiffStepMax[idxVar]))
    setQf[[idxVar]]$setQfStep$fail <- union(setQf[[idxVar]]$setQfStep$fail,setQf[[idxVar]]$setQfStep$fail+1)
    setQf[[idxVar]]$setQfStep$na <- setNaDiffDataIdx+1
    
    # If either of the first two values are NULL, flag NA (the first value is missed by the above code)
    if(diffDataNaIdx[1]){
      setQf[[idxVar]]$setQfStep$na <- union(1,setQf[[idxVar]]$setQfStep$na)
    }
    
    # If previous point is null, but next value is present, evaluate the step test with next value
    setQf[[idxVar]]$setQfStep$na <- setdiff(setQf[[idxVar]]$setQfStep$na,setRealDiffDataIdx)
    
    # For Verbose option, output actual flag values
    if(Vrbs) {
      qf[[idxVar]]$qfStep[setQf[[idxVar]]$setQfStep$fail] <- 1
      qf[[idxVar]]$qfStep[setQf[[idxVar]]$setQfStep$na] <- -1
    }  
  }

  # Do persistence test 
  if(class(WndwPers) %in% c("numeric","integer")){
    # Figure out what the time-based representation would be
    timePers <- as.POSIXlt(seq.POSIXt(from=Sys.time(),by="sec",length.out=length(data[,1])))
    WndwPers <- as.difftime(WndwPers,units='secs')
  } else {
    timePers <- time
  }
  
  for(idxVar in 1:numVar) {
    
    dataIdxVar <- data[[idxVar]]
    dataNaIdx <- dataNa[[idxVar]]
    dataRealIdx <- dataReal[[idxVar]]
    DiffPersMinIdx <- DiffPersMin[idxVar]
    WndwPersIdx <- WndwPers[idxVar]
    
    # Initialize
    setQf[[idxVar]]$setQfPers <- list(fail=numeric(0),na=numeric(0))
    setQf[[idxVar]]$setQfPers$na <- setDataNa[[idxVar]]
    
    # Quit if all data are NA
    if(length(setDataReal[[idxVar]]) <= 1){
      setQf[[idxVar]]$setQfPers$na <- 1:numData
      if(Vrbs){qf[[idxVar]]$qfPers[setQf[[idxVar]]$setQfPers$na] <- -1}
      next
    } else if(DiffPersMinIdx <= 0){
      if(Vrbs){qf[[idxVar]]$qfPers[setQf[[idxVar]]$setQfPers$na] <- -1}
      next
    }
    
    # Start at the beginning, making sure we aren't on a null value
    idxDataBgn <- setDataReal[[idxVar]][1]

    idxDataMin <- idxDataBgn # initialize index of running min
    idxDataMax <- idxDataBgn # intialize index of running max
    idxData <- 2 # intialize index position

    # Grab the data for these indices
    timeIdxBgn <- timePers[idxDataBgn]
    dataIdxMin <- dataIdxVar[idxDataMin]
    dataIdxMax <- dataIdxVar[idxDataMax]
    
    while(idxData <= numData) {

      #If we hit NA, get to the next non-NA value
      if(dataNaIdx[idxData]){
        idxData <- idxData +1
        next
      } 

      # Is the value at this index the running max or min?
      if(dataIdxVar[idxData] < dataIdxMin) {
        idxDataMin <- idxData
        dataIdxMin <- dataIdxVar[idxDataMin]
      } else if(dataIdxVar[idxData] > dataIdxMax) {
        idxDataMax <- idxData
        dataIdxMax <- dataIdxVar[idxDataMax]
      }
      
      
      # Is diff between max and min at or larger than the persistence threshold?
      if(dataIdxMax-dataIdxMin >= DiffPersMinIdx) {

        # We've hit the threshold, now check whether we are beyond the allowable time interval
        if(timePers[idxData]-timeIdxBgn < WndwPersIdx) {
          # Hooray! The data is not "stuck"
          idxDataBgn <- min(c(idxDataMin,idxDataMax))+1 # set start of next window to the next point after the earlier of the running min and max
          
          # Make sure we aren't on a null value
          while(dataNaIdx[idxDataBgn]){
            idxDataBgn <- idxDataBgn+1
          }

          idxDataMin <- idxDataBgn # reset running minimum
          idxDataMax <- idxDataBgn # reset running maximum
          idxData <- idxDataBgn+1 # reset the next point to be evaluated
          
          # Grab the data for these indices, improves CPU time
          timeIdxBgn <- timePers[idxDataBgn]
          dataIdxMin <- dataIdxVar[idxDataMin]
          dataIdxMax <- dataIdxVar[idxDataMax]
          
        } else {
          
          # We might have a stuck sensor, but first let's check whether we blew the time threshold b/c 
          # all the data were NA prior to this point
          if (sum(dataRealIdx[idxDataBgn:(idxData-1)]) <= 1) {
            
            # Data were all NA between the starting index and the current point, mark the non-NA points 
            # as cannot evaluate if they haven't already been marked for test failure
            setNaAdd <- setdiff(idxDataBgn:(idxData-1),setQf[[idxVar]]$setQfPers$fail)
            setQf[[idxVar]]$setQfPers$na <- union(setQf[[idxVar]]$setQfPers$na,setNaAdd)
            
          } else {
            
            # Awe bummer, the sensor was stuck before this point.
            setQf[[idxVar]]$setQfPers$fail <- union(setQf[[idxVar]]$setQfPers$fail,
                                                    idxDataBgn:(idxData-1))
            
          }
          
          idxDataBgn <- idxData # restart the test from here
          idxData <- idxDataBgn+1 # reset the next point to be evaluated
          
          # Grab the data for these indices, improves CPU time
          timeIdxBgn <- timePers[idxDataBgn]
          
        } 
        
      } else if ((idxData == numData) && (timePers[idxData]-timeIdxBgn >= WndwPersIdx)) {
        
        # We didn't hit the threshold and we've reached the end of the data. We are also beyond the allowable 
        # time interval for the persistence test, so let's flag the data
        setQf[[idxVar]]$setQfPers$fail <- union(setQf[[idxVar]]$setQfPers$fail,idxDataBgn:idxData)
        
        idxData <- idxData+1 # We're done
        
      } else {
        
        # We didn't pass the minimum acceptable change on this point, move to the next
        idxData <- idxData+1
      }
    }
    
    # If we reached the end of the data but the last value was NA, we need to go back and evaluate the last
    # non-NA value
    idxData <- numData
    if (dataNaIdx[idxData]) {
      # Get to last non-NA point
      idxData <- tail(setDataReal[[idxVar]],n=1)
      
      if (timePers[idxData]-timeIdxBgn >= WndwPersIdx) {
        # We didn't hit the threshold for the final non-NA points and we were beyond the allowable 
        # time interval for the persistence test, so let's flag the end of the data
        setQf[[idxVar]]$setQfPers$fail <- union(setQf[[idxVar]]$setQfPers$fail,idxDataBgn:idxData)
        
      } else {
        # We didn't hit the threshold for the final non-NA points, but we are not yet beyond the 
        # allowable time interval, so let's flag as unable to evaluate
        setQf[[idxVar]]$setQfPers$na <- union(setQf[[idxVar]]$setQfPers$na,idxDataBgn:idxData)
      }
    }
    
    # Don't mark the NA values as fail
    setQf[[idxVar]]$setQfPers$fail <- setdiff(setQf[[idxVar]]$setQfPers$fail,setQf[[idxVar]]$setQfPers$na)
    
    
    # For Verbose option, output actual flag values
    if(Vrbs) {
      qf[[idxVar]]$qfPers[setQf[[idxVar]]$setQfPers$fail] <- 1
      qf[[idxVar]]$qfPers[setQf[[idxVar]]$setQfPers$na] <- -1
    }  
  }

  
  # Do Null test
  for(idxVar in 1:numVar) {
    setQf[[idxVar]]$setQfNull <- list(fail=numeric(0),na=numeric(0)) # initialize
    
    if(TestNull[idxVar]) {
      setQf[[idxVar]]$setQfNull$fail <- setDataNa[[idxVar]]
    }
    
    # For Verbose option, output actual flag values
    if(Vrbs) {
      qf[[idxVar]]$qfNull[setQf[[idxVar]]$setQfNull$fail] <- 1
      qf[[idxVar]]$qfNull[setQf[[idxVar]]$setQfNull$na] <- -1
    }
    
  }
  
  
  # Do Gap test
  for(idxVar in 1:numVar) {
    
    # Initialize
    setQf[[idxVar]]$setQfGap <- list(fail=numeric(0),na=numeric(0)) # initialize
    
    # Do a rolling count of NAs with a window the length of the gap
    dataNAIdx <- dataNa[[idxVar]]
    NumGapIdx <- NumGap[idxVar]
    numNaWndwGap <- RcppRoll::roll_sum(x=dataNAIdx,n=NumGapIdx,by=1,align='left',na.rm=FALSE)
    
    # Which of the windows has all gaps? Mark the test failure
    setGapBgn <- which(numNaWndwGap == 4)
    qfGap <- rep(0,numData)
    for(idxGapBgn in setGapBgn){
      qfGap[idxGapBgn:(idxGapBgn+NumGapIdx-1)] <- 1
    }
    
    setQf[[idxVar]]$setQfGap$fail <- which(qfGap == 1)
    
    if(Vrbs) {
      qf[[idxVar]]$qfGap <- qfGap
    } 
    
  }
  
  # Return results
  if(!Vrbs) {
    rpt <- setQf
  } else {
    rpt <- qf
  }
  
  
  return(rpt)
  
}
