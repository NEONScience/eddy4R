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
#   Cove Sturtevant (2016-11-14)
#     adjust variable naming to conform to eddy4R coding style
##############################################################################################

def.qfqm.dp01 <- function (
  data,             # a data frame containing the data to be evaluated (do not include the time stamp vector here). Required input.
  time = base::as.POSIXlt(base::seq.POSIXt(from=base::Sys.time(),by="sec",length.out=base::length(data[,1]))),  # time vector corresponding with the rows in data, in  Class "POSIXlt", which is a named list of vectors representing sec, min, hour,day,mon,year. Defaults to an evenly spaced time vector starting from execution by seconds.
  posQf = base::vector("list",base::length(data)),    # Only input ONE of either posQf or qf. A named list of variables matching those in data, each itself a named list of flags (standard or user-defined names), with nested lists of $fail and $na vector positions of failed and na quality tests for that variable and flag (eg. posQf$X$posQfStep$fail and posQf$Y$posQfStep$na). 
  qf = base::vector("list",base::length(data)), # Only input ONE of either posQf or qf. A list of variables matching those in data, each containing a data frame of quality flags for that variable. Number of rows must match that of \code{data}
  WndwAgr = 1800*base::median(base::abs(base::diff(time)),na.rm=TRUE), # A difftime object of length 1 specifying the time interval for aggregating flags of each variable. 
  NameQfExcl = base::as.list(base::character(length=base::length(data))) # A list of length equal to number of variables, each itself a list of strings naming the flags (matching a subset of posQf) for which to exclude the flagged indices of each variable from the L1 average. 
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
  
  # Check posQf
  if(!base::is.list(posQf)) {
    base::stop("Input list posQf must be a list. See documentation.")
  } else if (base::length(posQf) != base::length(data)) {
    base::warning("Input list posQf must have length equal to number of variables in data.")
  }
  
  # Check WndwAgr
  WndwAgr <- base::try(base::as.difftime(WndwAgr),silent=TRUE)
  if(base::class(WndwAgr) == "try-error"){
    base::stop("Input parameter WndwAgr must be a difftime object")
  } else if (base::length(WndwAgr) != 1) {
    base::warning("Length of input parameter WndwAgr is greater than 1. Using first element of WndwAgr for all variables.")
    WndwAgr <- WndwAgr[1]  
  } 
  if (base::length(base::which(WndwAgr < base::median(base::abs(base::diff(time)),na.rm=TRUE)*base::rep.int(1,base::length(data)))) > 0) {
    base::stop("Input parameter WndwAgr must be greater than the time interval of time")
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
  if (base::as.double(WndwAgr, units = "mins") <= 1) {
    dateBgn <- base::trunc.POSIXt(time[1],units="mins") #Truncate to nearest minute
  } else if (base::as.double(WndwAgr, units="hours") <= 1) {
    dateBgn <- base::trunc.POSIXt(time[1],units="hours") #Truncate to nearest minute
  } else {
    dateBgn <- base::trunc.POSIXt(time[1],units="days") #Truncate to nearest minute
  }
  
  # Time series of aggregation window starting points
  timeAgrBgn <- base::as.POSIXlt(base::seq.POSIXt(from=dateBgn,to=time[base::length(time)],by=base::format(WndwAgr)))
  
  # Ammend starting point of aggregated time series to make sure start when there is data
  exstData <- FALSE
  idxAgr <- 1
  while (!exstData) {
    
    # Do we have data points?
    posData <- base::which((base::time >= timeAgrBgn[idxAgr]) & (time < timeAgrBgn[idxAgr]+WndwAgr))
    
    if (base::length(posData) == 0) {
      idxAgr <- idxAgr+1
    } else {
      exstData <- TRUE
    }
  }
  timeAgrBgn <- timeAgrBgn[idxAgr:length(timeAgrBgn)] # truncate aggregated time series vector
  
  # Take inventory of the flags we have and set up output naming
  dataAgr <- base::as.list(data) # copy variable names to output
  nameVar <- base::names(data)
  numVar <- base::length(nameVar)
  
  # If input is quality flags, turn it into positions of failed and na tests
  if(base::sum(base::unlist(base::lapply(posQf,function(var){base::is.null(var)}))) == base::length(data)) {
    
    posQf <- base::vector("list",numVar) # Initialize
    base::names(posQf) <- nameVar
    
    for (idxVar in nameVar) {
      
      # Intialize flags for this variable
      numQf <- base::length(qf[[idxVar]])
      posQf[[idxVar]] <- base::vector("list",numQf)
      base::names(posQf[[idxVar]]) <- base::names(qf[[idxVar]])
      
      for (idxQf in 1:numQf) {
        posQf[[idxVar]][[idxQf]] <- base::list(fail=numeric(0),na=numeric(0)) # initialize
        
        # Get positions of failed & na vector positions
        posQf[[idxVar]][[idxQf]]$fail <- base::which(qf[[idxVar]][,idxQf] == 1) # fail
        posQf[[idxVar]][[idxQf]]$na <- base::which(qf[[idxVar]][,idxQf] == -1) # na
        
      }
      
    }
  }
  
  # Assign variable names to output and initialize
  for (idxVar in 1:base::length(data)) {
    numQf <- base::length(posQf[[idxVar]]) # #flags we have
    nameQf <- base::names(posQf[[idxVar]]) # list of flag names
    nameVarOut <- base::c("mean","min","max","var","ste","numPts") # initialize the 1st 6 variable names in the output data
    nameVarOutQm <- base::c("Pass","Fail","Na") # 3 subvariables per quality metric
    
    # Put together output variable names
    for (idxQf in base::numeric(numQf)+1:numQf) {
    
      tmp <- base::nameQf[idxQf]
      # Get rid of leading "posQf" if using output from def.plau
      if (base::regexpr(pattern="posQf",text=tmp,ignore.case=FALSE)[1] == 1) {
        tmp <- base::sub(pattern="posQf", replacement="", x=tmp, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE)
      }
      # Get rid of leading "qf" if using other output 
      if (base::regexpr(pattern="qf",text=tmp,ignore.case=FALSE)[1] == 1) {
        tmp <- base::sub(pattern="qf", replacement="", x=tmp, ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE)
      }
      
      
      for (idxQm in 1:3) {
        nameVarOut[6+(idxQf-1)*3+idxQm] <- base::paste0("qm",tools::toTitleCase(tmp),nameVarOutQm[idxQm],collapse ="")
      }
    }
    nameVarOut <- base::c(nameVarOut,"qmAlpha","qmBeta","qfFinl") # Add alpha QM, beta QM, and final quality flag
            

    dataAgr[[idxVar]] <- base::data.frame(base::matrix(nrow=length(timeAgrBgn),ncol=6+3*numQf+3)) # 6 for mean/min/max/etc. 3*#flags for fail, pass, NA, and 3 for alpha & beta QMs and final QF
    base::colnames(dataAgr[[idxVar]]) <- nameVarOut
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
      
      # Make sure the flag in NameQfExcl has a matching flag in posQf
      if(base::length(base::which(base::names(posQf[[idxVar]]) == idxQfExcl)) != 1) {
        base::stop(base::paste0("Cannot find flag '",idxQfExcl,"' (listed in input NameQfExcl for variable '",idxVar,
                       "') in input posQf"))
      }
      
      # Check whether we have access to failed indices for flag
      if(base::length(base::which(base::names(posQf[[idxVar]][[idxQfExcl]]) == "fail")) != 1) {
        # Bummer, we don't have a variable name match, exit
        base::stop(base::paste0("Cannot find list of failed vector positions for flag ",idxQfExcl))
      }
      
      # If we made it this far, we found a list of indices to remove in this variable and flag. Let's do it.
      data[[idxVar]][posQf[[idxVar]][[idxQfExcl]]$fail] <- NA
      
    }
  }



# Compute aggregate stats -------------------------------------------------

  # Loop through each variable
  for(idxVar in nameVar) {
    
    # Loop through each aggregation window
    for (idxAgr in 1:base::length(timeAgrBgn)) {
      
      # Find data locations in window
      posData <- base::which((time >= timeAgrBgn[idxAgr]) & (time < timeAgrBgn[idxAgr]+WndwAgr))
      numDataAgr <- base::length(posData)
      
      # If there is no data to process move to next aggregation window
      if (numDataAgr == 0) {
        next
      }
      
      # Summary statistics
      dataAgr[[idxVar]]$mean[idxAgr] <- base::mean(data[[idxVar]][posData],na.rm=TRUE) # Mean
      # When calculating min & max, we want NA if there is no non-NA data
      if (base::sum(!base::is.na(data[posData,idxVar])) > 0) {
        dataAgr[[idxVar]]$min[idxAgr] <- base::min(data[[idxVar]][posData],na.rm=TRUE) # Min 
        dataAgr[[idxVar]]$max[idxAgr] <- base::max(data[[idxVar]][posData],na.rm=TRUE) # Max 
      } else {
        dataAgr[[idxVar]]$min[idxAgr] <- NA
        dataAgr[[idxVar]]$max[idxAgr] <- NA
      }
      dataAgr[[idxVar]]$var[idxAgr] <- stats::var(data[[idxVar]][posData],na.rm=TRUE) # Variance      
      dataAgr[[idxVar]]$numPts[idxAgr] <- base::length(base::which(!base::is.na(data[[idxVar]][posData]))) # number of non-na points      
      dataAgr[[idxVar]]$ste[idxAgr] <- base::sqrt(dataAgr[[idxVar]]$var[idxAgr])/base::sqrt(dataAgr[[idxVar]]$numPts[idxAgr]) # Standard error of the mean    
      
      # Quality metrics
      posAlpha <- base::numeric(0) # initialize locations for alpha QM
      posBeta <- base::numeric(0) # initialize locations for beta QM
      posBetaNull <- base::numeric(0) # initialize beta locations attributed to null points
      for (idxQf in 1:base::length(base::names(posQf[[idxVar]]))){
        nameQf <- base::names(posQf[[idxVar]])[idxQf]

        # Pass
        dataAgr[[idxVar]][[6+(idxQf-1)*3+1]][idxAgr] <- base::length(base::setdiff(posData,
            base::union(posQf[[idxVar]][[nameQf]]$fail,posQf[[idxVar]][[nameQf]]$na)))/numDataAgr*100
        # Fail
        dataAgr[[idxVar]][[6+(idxQf-1)*3+2]][idxAgr] <- base::length(base::intersect(posData,
            posQf[[idxVar]][[nameQf]]$fail))/numDataAgr*100
        # NA
        dataAgr[[idxVar]][[6+(idxQf-1)*3+3]][idxAgr] <- base::length(base::intersect(posData,
            posQf[[idxVar]][[nameQf]]$na))/numDataAgr*100
        
        # Keep a running tally of alpha and beta locations
        posAlpha <- base::union(posAlpha,base::intersect(posData,posQf[[idxVar]][[nameQf]]$fail))
        posBeta <- base::union(posBeta,base::intersect(posData,posQf[[idxVar]][[nameQf]]$na))
        if (base::length(base::grep("null",nameQf,ignore.case=TRUE)) != 0)  {
          # We want to exclude beta points attributed to null flag, so save null points for later
          posBetaNull <- base::union(posBetaNull,base::intersect(posData,posQf[[idxVar]][[nameQf]]$fail))
        }
      }

      # Tally up the alpha & beta QMs
      dataAgr[[idxVar]]$qmAlpha[idxAgr] <- base::length(posAlpha)/numDataAgr*100
      posBeta <- base::setdiff(posBeta,posBetaNull) # exclude beta locations attributed to null test
      dataAgr[[idxVar]]$qmBeta[idxAgr] <- base::length(posBeta)/numDataAgr*100
      
    }
    
    # Compute final quality flag
    dataAgr[[idxVar]]$qfFinl[(2*dataAgr[[idxVar]]$qmAlpha + dataAgr[[idxVar]]$qmBeta) >= 20] <- 1
    dataAgr[[idxVar]]$qfFinl[(2*dataAgr[[idxVar]]$qmAlpha + dataAgr[[idxVar]]$qmBeta) < 20] <- 0
  }
  
  
  
  # Return results
  return(base::list(timeAgrBgn=timeAgrBgn,dataAgr=dataAgr))
  
}