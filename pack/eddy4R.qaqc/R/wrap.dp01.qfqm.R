##############################################################################################
#' @title Wrapper function: Generate basic L1 data product, including descriptive statics, quality metrics, and final quality flag 

#' @author
#' Cove Sturtevant \email{eddy4R.info@gmail.com}

#' @description 
#' Function wrapper. Aggregates Level 0' (calibrated raw) data and accompanying quality flags and metrics into a basic time-aggregated L1 data product, including descriptive statics, quality metrics, and final quality flag.

#' @param data Required input. A data frame containing the L0' (calibrated raw) data evaluated (do not include the time stamp vector here). 
#' @param time Optional. A time vector of class POSIXlt of times corresponding with each row in data. Defaults to an evenly spaced time vector starting from system time of execution by seconds. 
#' @param setQf Optional. Only input ONE of either \code{setQf} or \code{qf}. # A named list of variables matching those in data, each itself a named list of flags (standard or user-defined names), with nested lists of $fail and $na vector positions of failed and na quality tests for that variable and flag (eg. setQf$X$setQfStep$fail and setQf$Y$setQfStep$na). Aggregated quality flags and metrics will be computed for all flags in this list, and all will be used to compute the final quality flag. This function directly accepts the output from def.plaus.R and def.dspk.wndw.R. Defaults to an empty flag list.
#' @param qf Optional. Only input ONE of either \code{setQf} or \code{qf}. A list of variables matching those in data, each containing a data frame of quality flags for that variable. Number of rows must match that of \code{data}
#' @param WndwAgr Optional. A difftime object of length 1 specifying the time interval for aggregating the data and flags of each variable. Defaults to 1800 x median observed time difference. Class difftime can be generated using as.difftime. If \code{WndwAgr} is < 1
#' @param TimeBgn Optional. A POSIXlt vector of length 1 indicating the time to begin aggregating from, in windows of WndwAgr. If unspecified, defaults to the first value in time truncated to: the minute if \code{WndwAgr} is <= 1 min, the hour if 1 min < \code{WndwAgr} <= 1 hr, the day otherwise. Aggregation windows prior to the start of data will be removed.
#' @param TimeEnd Optional. A POSIXlt vector of length 1 indicating the time to end aggregation (non-inclusive). If unspecified, defaults to the end of the last aggregation window containing data values as constructed from \code{WndwAgr} and \code{TimeBgn}
#' @param NameQfExcl Optional. A list of length equal to number of variables, each itself a vector of character strings naming the flags (matching a subset of setQf or qf) for which to exclude the flagged indices of each variable from the L1 average. Eg. \code{NameQfExcl} <- list(x=c("setQfRng","setQfStep"),y=c("setQfPers","setQfNull","setQfGap")). Note that variables in the list must be in the same order as those in data. Defaults to an empty list (flagged points are not excluded from the L1 average).

#' @return A list of: \cr
#' timeAgrBgn - the starting time stamp of aggregated L1 data and quality metrics \cr
#' timeAgrEnd - the ending time stamp (non-inclusive) of aggregated L1 data and quality metrics
#' dataAgr - a list of variables, each containing a data frame of the time-aggregated mean, minimum, maximum, variance, number of points going into the average, and quality metrics (pass, fail, NA) pertaining to that variable for each flag in setQf, as well as the alpha & beta quality metrics and final quality flag.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113)

#' @keywords NEON QAQC, quality flags and metrics, L1 average, final quality flag

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-01-05)
#     original creation
#   Cove Sturtevant (2016-02-04)
#     corrected variable name to conform to coding convention
#     (existData changed to exstData)
#   Cove Sturtevant (2016-02-09)
#     changed computation of min & max to return NA if all data in aggregation period are NA
#   Cove Sturtevant (2016-11-10)
#     adjusted code to accept new output format from def.plau
#     adjusted variable naming to conform to eddy4R coding convention
#     added optional input of quality flags for either/or input instead of vector positions of 
#         failed and na test results
#   Cove Sturtevant (2016-11-14)
#     adjust variable naming to conform to eddy4R coding style
#   Cove Sturtevant (2016-11-22)
#     turned definition function into a wrapper function by modularizing the computation of 
#         descriptive statistics, quality metrics, and final quality flag
#     added optional specification of beginning and ending time
#   Natchaya P-Durden (2018-04-04)
#    applied eddy4R term name convention; replaced posQf by setQf
#   Natchaya P-Durden (2018-04-11)
#    applied eddy4R term name convention; replaced pos by set
##############################################################################################

wrap.dp01.qfqm <- function (
  data,             # a data frame containing the data to be evaluated (do not include the time stamp vector here). Required input.
  time = base::as.POSIXlt(base::seq.POSIXt(from=base::Sys.time(),by="sec",length.out=base::length(data[,1]))),  # time vector corresponding with the rows in data, in  Class "POSIXlt", which is a named list of vectors representing sec, min, hour,day,mon,year. Defaults to an evenly spaced time vector starting from execution by seconds.
  setQf = base::vector("list",base::length(data)),    # Only input ONE of either setQf or qf. A named list of variables matching those in data, each itself a named list of flags (standard or user-defined names), with nested lists of $fail and $na vector positions of failed and na quality tests for that variable and flag (eg. setQf$X$setQfStep$fail and setQf$Y$setQfStep$na). 
  qf = base::vector("list",base::length(data)), # Only input ONE of either setQf or qf. A list of variables matching those in data, each containing a data frame of quality flags for that variable. Number of rows must match that of \code{data}
  WndwAgr = 1800*stats::median(base::abs(base::diff(time)),na.rm=TRUE), # A difftime object of length 1 specifying the time interval for aggregating flags of each variable. 
  TimeBgn = NULL, # A POSIXlt vector of length 1 indicating the time to begin aggregating from. If unspecified, defaults to the first value in time truncated to: the minute if WndwAgr is <= 1 min, the hour if 1 min < WndwAgr <= 1 hr, the day otherwise
  TimeEnd = NULL, # A POSIXlt vector of length 1 indicating the time to end aggregation (non-inclusive). If unspecified, defaults to the end of the last aggregation window with data values
  NameQfExcl = base::as.list(base::character(length=base::length(data))) # A list of length equal to number of variables, each itself a list of strings naming the flags (matching a subset of setQf) for which to exclude the flagged indices of each variable from the L1 average. 
) {
  
  
# Error Checking ----------------------------------------------------------

  # Check data
  if(base::missing("data") | !base::is.data.frame(data)) {
    base::stop("Required input 'data' must be a data frame")
  }
  
  # Check time
  time <- base::try(base::as.POSIXlt(time),silent=TRUE)
  if(base::class(time)[1] == "try-error"){
    base::stop("Input variable time must be of class POSIXlt")
  } else if (base::length(time) != base::length(data[,1])) {
    base::stop("Length of input variable time must be equal to length of data.")
  } 
  
  # Check setQf
  if(!base::is.list(setQf)) {
    base::stop("Input list setQf must be a list. See documentation.")
  } else if (base::length(setQf) != base::length(data)) {
    base::warning("Input list setQf must have length equal to number of variables in data.")
  }
  
  # Check qf
  if(!base::is.list(qf)) {
    base::stop("Input list qf must be a list. See documentation.")
  } else if (base::length(qf) != base::length(data)) {
    base::warning("Input list qf must have length equal to number of variables in data.")
  }
  
  # Check WndwAgr
  WndwAgr <- base::try(base::as.difftime(WndwAgr),silent=TRUE)
  if(base::class(WndwAgr) == "try-error"){
    base::stop("Input parameter WndwAgr must be a difftime object")
  } else if (base::length(WndwAgr) != 1) {
    base::warning("Length of input parameter WndwAgr is greater than 1. Using first element of WndwAgr for all variables.")
    WndwAgr <- WndwAgr[1]  
  } 
  if (base::length(base::which(WndwAgr < stats::median(base::abs(base::diff(time)),na.rm=TRUE)*base::rep.int(1,base::length(data)))) > 0) {
    base::stop("Input parameter WndwAgr must be greater than the time interval of time")
  }
  
  # Check TimeBgn
  if(!base::is.null(TimeBgn) && (!Base::inherits(TimeBgn,"POSIXlt") || base::length(TimeBgn) != 1)) {
    base::stop("Input parameter TimeBgn must be a single value of class POSIXlt")
  }
  
  # Check TimeEnd
  if(!base::is.null(TimeEnd) && (!Base::inherits(TimeEnd,"POSIXlt") || base::length(TimeEnd) != 1)) {
    base::stop("Input parameter TimeEnd must be a single value of class POSIXlt")
  }
  
  # Check NameQfExcl
  if(!base::is.list(NameQfExcl)) {
    base::stop("Input list NameQfExcl must be a list of length equal to the number of variables, each a list of strings naming the flags which exclude data from the L1 average.")
  } else if (base::length(NameQfExcl) != base::length(data)) {
    base::warning("Input parameter NameQfExcl does not contain entries for some or all variables. L1 averages for some variables may not be computed correctly.")
  }
  
  

# Initialize output ------------------------------------------------------------------

  # Set up time sequence for averaging. If aggregation time is in ...
  # ... seconds, truncate to nearest minute. 
  # ... minutes, truncate to nearest hour.
  # ... hours or greater, truncate to nearest day
  # We will go back later and get rid of averaging intervals with no data in them
  flagTime <- FALSE # Intialize flag to go back and amend time sequence
  if (base::is.null(TimeBgn)){
    flagTime <- TRUE
    if (base::as.double(WndwAgr, units = "mins") <= 1) {
      TimeBgn <- base::trunc.POSIXt(time[1],units="mins") #Truncate to nearest minute
    } else if (base::as.double(WndwAgr, units="hours") <= 1) {
      TimeBgn <- base::trunc.POSIXt(time[1],units="hours") #Truncate to nearest hour
    } else {
      TimeBgn <- base::trunc.POSIXt(time[1],units="days") #Truncate to nearest day
    }
  }
  
  # Set ending time
  if(base::is.null(TimeEnd)) {
    TimeEnd <- time[base::length(time)]
  }
  
  
  # Time series of aggregation window starting points
  timeAgrBgn <- base::as.POSIXlt(base::seq.POSIXt(from=TimeBgn,to=TimeEnd,by=base::format(WndwAgr)))
  
  # Ammend starting point of aggregated time series to make sure start when there is data
  exstData <- FALSE
  idxAgr <- 1
  while (!exstData && flagTime) {
    
    # Do we have data points?
    setData <- base::which((time >= timeAgrBgn[idxAgr]) & (time < timeAgrBgn[idxAgr]+WndwAgr))
    
    if (base::length(setData) == 0) {
      idxAgr <- idxAgr+1
    } else {
      exstData <- TRUE
    }
  }
  timeAgrBgn <- timeAgrBgn[idxAgr:length(timeAgrBgn)] # truncate aggregated time series vector
  timeAgrEnd <- timeAgrBgn+WndwAgr # truncate aggregated time series vector
  
  # Take inventory of the flags we have and set up output naming
  nameVar <- base::names(data)
  numVar <- base::length(nameVar)
  numData <- base::nrow(data)
  dataAgr <- base::vector("list",length=numVar)
  base::names(dataAgr) <- nameVar
  numDataAgr <- base::length(timeAgrBgn)
  
  # If input is vector positions of failed and na tests, switch to quality flag values
  if(base::sum(base::unlist(base::lapply(qf,function(var){base::is.null(var)}))) == base::length(data)) {
    
    qf <- eddy4R.qaqc::def.conv.qf.vrbs(setQf=setQf,numRow=numData)
  }
  
# Remove data points marked for exclusion -----------------------------------------

  # Get rid of points marked to exclude from L1 data
  for(idxVar in nameVar) {
    
    # Check whether list of exclusion flags is named same as variables or whether 
    # we are using col index only
    idxVarExcl <- base::which(base::names(NameQfExcl) == idxVar)
    if(base::length(idxVarExcl) != 1) {
      # Bummer, we don't have a variable name match, exit
      base::stop("Cannot interpret which variables in input list NameQfExcl match to which data variables. Make sure each variable in data has a matching variable name in NameQfExcl, even if entry in NameQfExcl is an empty vector.")
    }

    
    # Remove bad data points
    for (idxQfExcl in NameQfExcl[[idxVarExcl]]) {
      
      # Make sure the flag in NameQfExcl has a matching flag
      if(!idxQfExcl %in% names(qf[[idxVar]])) {
        base::stop(base::paste0("Cannot find flag '",idxQfExcl,"' (listed in input NameQfExcl for variable '",idxVar,
                       "') in quality flags"))
      }
      
      # If we made it this far, we found a the quality flag that results in data exclusion. Let's do it.
      setExcl <- which(qf[[idxVar]][[idxQfExcl]] == 1)
      data[[idxVar]][setExcl] <- NA
      
    }
  }



# Compute aggregate stats -------------------------------------------------

  # Loop through each variable
  for(idxVar in nameVar) {
    
    # Initialize output
    numQf <- base::length(qf[[idxVar]]) # #flags we have
    tmp <- base::data.frame(base::matrix(nrow=numDataAgr,ncol=6+3*numQf+3))
    dataAgr[[idxVar]] <- tmp
    
    # Loop through each aggregation window
    for (idxAgr in 1:numDataAgr) {
      
      # Find data locations in window
      setData <- base::which((time >= timeAgrBgn[idxAgr]) & (time < timeAgrBgn[idxAgr]+WndwAgr))
      numDataAgr <- base::length(setData)
      
      # If there is no data to process move to next aggregation window
      if (numDataAgr == 0) {
        next
      }
      
      # Summary statistics
      statSmmy <- eddy4R.base::def.neon.dp01(data=data[[idxVar]][setData])
      
      # Quality metrics
      qm <- eddy4R.qaqc::def.qm(qf=qf[[idxVar]][setData,])
      
      # Alpha, beta qms and final quality flag
      qfFinl <- eddy4R.qaqc::def.qf.finl(qf=qf[[idxVar]][setData,], WghtAlphBeta=c(2,1), Thsh=0.2)[["qfqm"]]
      
      # Combine all outputs
      dataAgr[[idxVar]][idxAgr,] <- cbind(statSmmy,qm,qfFinl)
      
    }
    
    # Assign names to output columns
    base::colnames(dataAgr[[idxVar]]) <- base::colnames(cbind(statSmmy,qm,qfFinl))
    
  }
  
  
  
  # Return results
  return(base::list(timeAgrBgn=timeAgrBgn,timeAgrEnd=timeAgrEnd,dataAgr=dataAgr))
  
}