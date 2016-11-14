##############################################################################################
#' @title Quality flags and quality metrics (basic L1 data products) 

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description 
#' Function definition. Aggregates the quality flags and computes quality metrics and final quality flag for basic L1 (time window averaged) data products from the plausibility of the individual (L0) data indices to be aggregated.

#' @param data Required input. A data frame containing the L0 data evaluated (do not include the time stamp vector here). 
#' @param time Optional. A time vector of class POSIXlt of times corresponding with each row in data. Defaults to an evenly spaced time vector starting from system time of execution by seconds. 
#' @param posQf Optional. Only input ONE of either \code{posQf} or \code{qf}. # A named list of variables matching those in data, each itself a named list of flags (standard or user-defined names), with nested lists of $fail and $na vector positions of failed and na quality tests for that variable and flag (eg. posQf$X$posQfStep$fail and posQf$Y$posQfStep$na). Aggregated quality flags and metrics will be computed for all flags in this list, and all will be used to compute the final quality flag. This function directly accepts the output from def.plaus.R and def.dspk.wndw.R. Defaults to an empty flag list.
#' @param qf Optional. Only input ONE of either \code{posQf} or \code{qf}. A list of variables matching those in data, each containing a data frame of quality flags for that variable. Number of rows must match that of \code{data}
#' @param WndwAgr Optional. A difftime object of length equal to number of variables in data specifying the time interval for aggregating flags of each variable. Defaults to 1800 x median observed time difference. Class difftime can be generated using as.difftime.
#' @param NameQfExcl Optional. A list of length equal to number of variables, each itself a vector of strings naming the flags (matching a subset of posQf) for which to exclude the flagged indices of each variable from the L1 average. Eg. NameQfExcl <- list(x=c("posQfRng","posQfStep"),y=c("posQfPers","posQfNull","posQfGap")). Note that variables in the list must be in the same order as those in data. Defaults to an empty list (flagged points are not excluded from the L1 average).

#' @return A list of: \cr
#' timeAgrBgn - the starting time stamp of aggregated L1 data and quality metrics \cr
#' dataAgr - a list of variables, each containing a data frame of the time-aggregated mean, minimum, maximum, variance, standard deviation of the mean, number of points going into the average, and quality metrics (pass, fail, NA) pertaining to that variable for each flag in posQf, as well as the alpha & beta quality metrics and final quality flag.

#' @references 
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
##############################################################################################

def.qfqm.dp01 <- function (
  data,             # a data frame containing the data to be evaluated (do not include the time stamp vector here). Required input.
  time = as.POSIXlt(seq.POSIXt(from=Sys.time(),by="sec",length.out=length(data[,1]))),  # time vector corresponding with the rows in data, in  Class "POSIXlt", which is a named list of vectors representing sec, min, hour,day,mon,year. Defaults to an evenly spaced time vector starting from execution by seconds.
  posQf = vector("list",length(data)),    # Only input ONE of either posQf or qf. A named list of variables matching those in data, each itself a named list of flags (standard or user-defined names), with nested lists of $fail and $na vector positions of failed and na quality tests for that variable and flag (eg. posQf$X$posQfStep$fail and posQf$Y$posQfStep$na). 
  qf = vector("list",length(data)), # Only input ONE of either posQf or qf. A list of variables matching those in data, each containing a data frame of quality flags for that variable. Number of rows must match that of \code{data}
  WndwAgr = 1800*median(abs(diff(time)),na.rm=TRUE), # A difftime object of length 1 specifying the time interval for aggregating flags of each variable. 
  NameQfExcl = as.list(character(length=length(data))) # A list of length equal to number of variables, each itself a list of strings naming the flags (matching a subset of posQf) for which to exclude the flagged indices of each variable from the L1 average. 
) {
  
  
# Error Checking ----------------------------------------------------------

  # Check data
  if(missing("data") | !is.data.frame(data)) {
    stop("Required input 'data' must be a data frame")
  }
  
  # Check time
  time <- try(as.POSIXlt(time),silent=TRUE)
  if(class(time)[1] == "try-error"){
    stop("Input variable time must be of class POSIXlt")
  } else if (length(time) != length(data[,1])) {
    stop("Length of input variable time must be equal to length of data.")
  } 
  
  # Check posQf
  if(!is.list(posQf)) {
    stop("Input list posQf must be a list. See documentation.")
  } else if (length(posQf) != length(data)) {
    warning("Input list posQf must have length equal to number of variables in data.")
  }
  
  # Check WndwAgr
  WndwAgr <- try(as.difftime(WndwAgr),silent=TRUE)
  if(class(WndwAgr) == "try-error"){
    stop("Input parameter WndwAgr must be a difftime object")
  } else if (length(WndwAgr) != 1) {
    warning("Length of input parameter WndwAgr is greater than 1. Using first element of WndwAgr for all variables.")
    WndwAgr <- WndwAgr[1]  
  } 
  if (length(which(WndwAgr < median(abs(diff(time)),na.rm=TRUE)*rep.int(1,length(data)))) > 0) {
    stop("Input parameter WndwAgr must be greater than the time interval of time")
  }
  
  # Check NameQfExcl
  if(!is.list(NameQfExcl)) {
    stop("Input list NameQfExcl must be a list of length equal to the number of variables, each a list of strings naming the flags which exclude data from the L1 average.")
  } else if (length(NameQfExcl) != length(data)) {
    warning("Input parameter NameQfExcl does not contain entries for some or all variables. L1 averages for some variables may not be computed correctly.")
  }
  
  

# Initialize output ------------------------------------------------------------------

  # Set up time sequence for averaging. If aggregation time is in ...
  # ... seconds, truncate to nearest minute. 
  # ... minutes, truncate to nearest hour.
  # ... hours or greater, truncate to nearest day
  # We will go back later and get rid of averaging intervals with no data in them
  if (as.double(WndwAgr, units = "mins") <= 1) {
    dateBgn <- trunc.POSIXt(time[1],units="mins") #Truncate to nearest minute
  } else if (as.double(WndwAgr, units="hours") <= 1) {
    dateBgn <- trunc.POSIXt(time[1],units="hours") #Truncate to nearest minute
  } else {
    dateBgn <- trunc.POSIXt(time[1],units="days") #Truncate to nearest minute
  }
  
  # Time series of aggregation window starting points
  timeAgrBgn <- as.POSIXlt(seq.POSIXt(from=dateBgn,to=time[length(time)],by=format(WndwAgr)))
  
  # Ammend starting point of aggregated time series to make sure start when there is data
  exstData <- FALSE
  idxAggr <- 1
  while (!exstData) {
    
    # Do we have data points?
    posData <- which((time >= timeAgrBgn[idxAggr]) & (time < timeAgrBgn[idxAggr]+WndwAgr))
    
    if (length(posData) == 0) {
      idxAggr <- idxAggr+1
    } else {
      exstData <- TRUE
    }
  }
  timeAgrBgn <- timeAgrBgn[idxAggr:length(timeAgrBgn)] # truncate aggregated time series vector
  
  # Take inventory of the flags we have and set up output naming
  dataAgr <- as.list(data) # copy variable names to output
  
  # If input is quality flags, turn it into positions of failed and na tests
  if(sum(unlist(lapply(posQf,function(var){is.null(var)}))) == length(data)) {
    for (idxVar in 1:length(data)) {
      
      # Intialize flags for this variable
      numQf <- length(qf[[idxVar]])
      posQf[[idxVar]] <- vector("list",numQf)
      names(posQf[[idxVar]]) <- names(qf[[idxVar]])
      
      for (idxQf in 1:numQf) {
        posQf[[idxVar]][[idxQf]] <- list(fail=numeric(0),na=numeric(0)) # initialize
        
        # Get positions of failed & na vector positions
        posQf[[idxVar]][[idxQf]]$fail <- which(qf[[idxVar]][,idxQf] == 1) # fail
        posQf[[idxVar]][[idxQf]]$na <- which(qf[[idxVar]][,idxQf] == 1) # na
        
      }
      
    }
  }
  
  # Assign variable names to output and initialize
  for (idxVar in 1:length(data)) {
    numFlgs <- length(posQf[[idxVar]]) # #flags we have
    nameFlgs <- names(posQf[[1]]) # list of flag names
    nameVarsOut <- c("mean","min","max","var","ste","numPts") # initialize the 1st 6 variable names in the output data
    nameVarsOutQm <- c("Pass","Fail","Na") # 3 subvariables per quality metric
    
    # Put together output variable names
    for (idxFlag in numeric(numFlgs)+1:numFlgs) {
    
      tmp <- nameFlgs[idxFlag]
      # Get rid of leading "posQf" if using output from def.plau
      if (regexpr(pattern="posQf",text=tmp,ignore.case=FALSE)[1] == 1) {
        tmp <- sub(pattern="posQf", replacement="", x=tmp, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE)
      }
      # Get rid of leading "qf" if using other output 
      if (regexpr(pattern="qf",text=tmp,ignore.case=FALSE)[1] == 1) {
        tmp <- sub(pattern="qf", replacement="", x=tmp, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE)
      }
      
      
      for (idxQm in 1:3) {
        nameVarsOut[6+(idxFlag-1)*3+idxQm] <- paste0("qm",tools::toTitleCase(tmp),nameVarsOutQm[idxQm],collapse ="")
      }
    }
    nameVarsOut <- c(nameVarsOut,"qmAlpha","qmBeta","qfFinl") # Add alpha QM, beta QM, and final quality flag
            

    dataAgr[[idxVar]] <- data.frame(matrix(nrow=length(timeAgrBgn),ncol=6+3*numFlgs+3)) # 6 for mean/min/max/etc. 3*#flags for fail, pass, NA, and 3 for alpha & beta QMs and final QF
    colnames(dataAgr[[idxVar]]) <- nameVarsOut
  }
  

# Remove data points marked for exclusion -----------------------------------------


  # Get rid of points marked to exclude from L1 data
  nameVars <- names(data)
  for(idxVarData in nameVars) {
    
    # Check whether list of exclusion flags is named same as variables or whether 
    # we are using col index only
    idxVarExcl <- which(names(NameQfExcl) == idxVarData)
    if(length(idxVarExcl) != 1) {
      # Bummer, we don't have a variable name match, exit
        stop("Cannot interpret which variables in input list NameQfExcl match to which data variables. Make sure each variable in data has a matching variable name in NameQfExcl, even if entry in NameQfExcl is an empty vector.")
    }

    
    # Remove bad data points
    for (idxFlagExcl in names(NameQfExcl[[idxVarExcl]])) {
      
      # Make sure the flag in NameQfExcl has a matching flag in posQf
      which(names(posQf[[idxVarData]]) == idxFlagExcl)
      if(length(which(names(posQf[[idxVarData]]) == idxFlagExcl)) != 1) {
        stop(paste0("Cannot find flag '",idxFlagExcl,"' (listed in input NameQfExcl for variable '",idxVarData,
                       "') in input posQf"))
      }
      
      # Check whether we have access to failed indices for flag
      if(length(which(names(posQf[[idxVarData]][[idxFlagExcl]]) == "fail")) == 1) {
        # Bummer, we don't have a variable name match, exit
        stop(paste0("Cannot find list of failed vector positions for flag ",idxFlagExcl))
      }
      
      # If we made it this far, we found a list of indices to remove in this variable and flag. Let's do it.
      data[[idxVarData]][posQf[[idxVarData]][[idxFlagExcl]]$fail] <- NA
      
    }
  }



# Compute aggregate stats -------------------------------------------------

  # Loop through each variable
  for(idxVarData in nameVars) {
    
    # Loop through each aggregation window
    for (idxAggr in 1:length(timeAgrBgn)) {
      
      # Find data locations in window
      posData <- which((time >= timeAgrBgn[idxAggr]) & (time < timeAgrBgn[idxAggr]+WndwAgr))
      numDataAggr <- length(posData)
      
      # If there is no data to process move to next aggregation window
      if (numDataAggr == 0) {
        next
      }
      
      # Summary statistics
      dataAgr[[idxVarData]]$mean[idxAggr] <- mean(data[[idxVarData]][posData],na.rm=TRUE) # Mean
      # When calculating min & max, we want NA if there is no non-NA data
      if (sum(!is.na(data[posData,idxVarData])) > 0) {
        dataAgr[[idxVarData]]$min[idxAggr] <- min(data[[idxVarData]][posData],na.rm=TRUE) # Min 
        dataAgr[[idxVarData]]$max[idxAggr] <- max(data[[idxVarData]][posData],na.rm=TRUE) # Max 
      } else {
        dataAgr[[idxVarData]]$min[idxAggr] <- NA
        dataAgr[[idxVarData]]$max[idxAggr] <- NA
      }
      dataAgr[[idxVarData]]$var[idxAggr] <- var(data[[idxVarData]][posData],na.rm=TRUE) # Variance      
      dataAgr[[idxVarData]]$numPts[idxAggr] <- length(which(!is.na(data[[idxVarData]][posData]))) # number of non-na points      
      dataAgr[[idxVarData]]$ste[idxAggr] <- sqrt(dataAgr[[idxVarData]]$var[idxAggr])/sqrt(dataAgr[[idxVarData]]$numPts[idxAggr]) # Standard error of the mean    
      
      # Quality metrics
      posAlpha <- numeric(0) # initialize locations for alpha QM
      posBeta <- numeric(0) # initialize locations for beta QM
      posBetaNull <- numeric(0) # initialize beta locations attributed to null points
      for (idxFlag in 1:length(names(posQf[[idxVarData]]))){
        nameFlag <- names(posQf[[idxVarData]])[idxFlag]

        # Pass
        dataAgr[[idxVarData]][[6+(idxFlag-1)*3+1]][idxAggr] <- length(setdiff(posData,
            union(posQf[[idxVar]][[idxFlag]]$fail,posQf[[idxVar]][[idxFlag]]$na)))/numDataAggr*100
        # Fail
        dataAgr[[idxVarData]][[6+(idxFlag-1)*3+2]][idxAggr] <- length(intersect(posData,
            posQf[[idxVar]][[idxFlag]]$fail))/numDataAggr*100
        # NA
        dataAgr[[idxVarData]][[6+(idxFlag-1)*3+3]][idxAggr] <- length(intersect(posData,
            posQf[[idxVar]][[idxFlag]]$na))/numDataAggr*100
        
        # Keep a running tally of alpha and beta locations
        posAlpha <- union(posAlpha,intersect(posData,posQf[[idxVar]][[idxFlag]]$fail))
        posBeta <- union(posBeta,intersect(posData,posQf[[idxVar]][[idxFlag]]$na))
        if (length(grep("null",nameFlag,ignore.case=TRUE)) != 0)  {
          # We want to exclude beta points attributed to null flag, so save null points for later
          posBetaNull <- union(posBetaNull,intersect(posData,posQf[[idxVar]][[idxFlag]]$fail))
        }
      }

      # Tally up the alpha & beta QMs
      dataAgr[[idxVarData]]$qmAlpha[idxAggr] <- length(posAlpha)/numDataAggr*100
      posBeta <- setdiff(posBeta,posBetaNull) # exclude beta locations attributed to null test
      dataAgr[[idxVarData]]$qmBeta[idxAggr] <- length(posBeta)/numDataAggr*100
      
    }
    
    # Compute final quality flag
    dataAgr[[idxVarData]]$qfFinl[(2*dataAgr[[idxVarData]]$qmAlpha + dataAgr[[idxVarData]]$qmBeta) >= 20] <- 1
    dataAgr[[idxVarData]]$qfFinl[(2*dataAgr[[idxVarData]]$qmAlpha + dataAgr[[idxVarData]]$qmBeta) < 20] <- 0
  }
  
  
  
  # Return results
  return(list(timeAgrBgn=timeAgrBgn,dataAgr=dataAgr))
  
}