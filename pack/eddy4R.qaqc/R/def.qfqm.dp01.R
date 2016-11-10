##############################################################################################
#' @title Quality flags and quality metrics (basic L1 data products) 

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description 
#' Function definition. Aggregates the quality flags and computes quality metrics and final quality flag for basic L1 (time window averaged) data products from the plausibility of the individual (L0) data indices to be aggregated.

#' @param data Required input. A data frame containing the L0 data evaluated (do not include the time stamp vector here). 
#' @param ts Optional. A time vector of class POSIXlt of times corresponding with each row in data. Defaults to an evenly spaced time vector starting from system time of execution by seconds. 
#' @param flgs Optional. A list of named flags (standard or user-defined or names), each itself a list of flagged indices for each variable in data (ex. flgs$posQfStep$X and flgs$posQfStep$Y listing flagged positions for variables X and Y, respectively). Aggregated quality flags and metrics will be computed for all flags in this list, and all will be used to compute the final quality flag. This function directly accepts the output from def.plaus.R and def.dspk.wndw.R. Defaults to an empty flag list.
#' @param TintAggr Optional. A difftime object of length equal to number of variables in data specifying the time interval for aggregating flags of each variable. Defaults to 1800 x median observed time difference. Class difftime can be generated using as.difftime.
#' @param FlagExcl Optional. A list of length equal to number of variables, each itself a vector of strings naming the flags (matching a subset of flgs) for which to exclude the flagged indices of each variable from the L1 average. Eg. FlagExcl <- list(x=c("posQfRng","posQfStep"),y=c("posQfPers","posQfNull","posQfGap")). Note that variables in the list must be in the same order as those in data. Defaults to an empty list (flagged points are not excluded from the L1 average).

#' @return A list of: \cr
#' tsAggrBgn - the starting time stamp of aggregated L1 data and quality metrics \cr
#' qfqm - a list of variables, each containing a data frame of the time-aggregated mean, minimum, maximum, variance, standard deviation of the mean, number of points going into the average, and quality metrics (pass, fail, NA) pertaining to that variable for each flag in flgs, as well as the alpha & beta quality metrics and final quality flag.

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
##############################################################################################


def.qfqm.dp01 <- function (
  data,             # a data frame containing the data to be evaluated (do not include the time stamp vector here). Required input.
  ts = as.POSIXlt(seq.POSIXt(from=Sys.time(),by="sec",length.out=length(data[,1]))),  # time vector corresponding with the rows in data, in  Class "POSIXlt", which is a named list of vectors representing sec, min, hour,day,mon,year. Defaults to an evenly spaced time vector starting from execution by seconds.
  flgs = list(),    # A list of named flags (standard or user-defined or names), each itself a list of flagged indices for each variable in data (ex. flgs$posQfStep$X and flgs$posQfStep$Y listing flagged indices for variables X and Y, respectively). 
  TintAggr = 1800*median(abs(diff(ts)),na.rm=TRUE), # A difftime object of length 1 specifying the time interval for aggregating flags of each variable. 
  FlagExcl = as.list(character(length=length(data))) # A list of length equal to number of variables, each itself a list of strings naming the flags (matching a subset of flgs) for which to exclude the flagged indices of each variable from the L1 average. 
) {
  
  
# Error Checking ----------------------------------------------------------

  # Check data
  if(missing("data") | !is.data.frame(data)) {
    stop("Required input 'data' must be a data frame")
  }
  
  # Check ts
  ts <- try(as.POSIXlt(ts),silent=TRUE)
  if(class(ts)[1] == "try-error"){
    stop("Input variable ts must be of class POSIXlt")
  } else if (length(ts) != length(data[,1])) {
    stop("Length of input variable ts must be equal to length of data.")
  } 
  
  # Check flgs
  if(!is.list(flgs)) {
    stop("Input list flgs must be a list of named flags, each a list of length equal to the number of variables containing flagged indices for each variable.")
  } else if (length(unlist(lapply(lapply(flgs,function(var) {c(length(var$fail),length(var$na))}),function(lst) {which(lst != length(data))}))) > 0) {
    warning("Some or all named flags are not present for all variables in data. Aggregated flags and quality metrics for some variables may not be computed correctly.")
  }
  
  # Check TintAggr
  TintAggr <- try(as.difftime(TintAggr),silent=TRUE)
  if(class(TintAggr) == "try-error"){
    stop("Input parameter TintAggr must be a difftime object")
  } else if (length(TintAggr) != 1) {
    warning("Length of input parameter TintAggr is greater than 1. Using first element of TintAggr for all variables.")
    TintAggr <- TintAggr[1]  
  } 
  if (length(which(TintAggr < median(abs(diff(ts)),na.rm=TRUE)*rep.int(1,length(data)))) > 0) {
    stop("Input parameter TintAggr must be greater than the time interval of ts")
  }
  
  # Check FlagExcl
  if(!is.list(FlagExcl)) {
    stop("Input list FlagExcl must be a list of length equal to the number of variables, each a list of strings naming the flags which exclude data from the L1 average.")
  } else if (length(FlagExcl) != length(data)) {
    warning("Input parameter FlagExcl does not contain entries for some or all variables. L1 averages for some variables may not be computed correctly.")
  }
  
  

# Initialize output ------------------------------------------------------------------

  # Set up time sequence for averaging. If aggregation time is in ...
  # ... seconds, truncate to nearest minute. 
  # ... minutes, truncate to nearest hour.
  # ... hours or greater, truncate to nearest day
  # We will go back later and get rid of averaging intervals with no data in them
  if (as.double(TintAggr, units = "mins") <= 1) {
    dateBgn <- trunc.POSIXt(ts[1],units="mins") #Truncate to nearest minute
  } else if (as.double(TintAggr, units="hours") <= 1) {
    dateBgn <- trunc.POSIXt(ts[1],units="hours") #Truncate to nearest minute
  } else {
    dateBgn <- trunc.POSIXt(ts[1],units="days") #Truncate to nearest minute
  }
  
  # Time series of aggregation window starting points
  tsAggrBgn <- as.POSIXlt(seq.POSIXt(from=dateBgn,to=ts[length(ts)],by=format(TintAggr)))
  
  # Ammend starting point of aggregated time series to make sure start when there is data
  exstData <- FALSE
  idxAggr <- 1
  while (!exstData) {
    
    # Do we have data points?
    posData <- which((ts >= tsAggrBgn[idxAggr]) & (ts < tsAggrBgn[idxAggr]+TintAggr))
    
    if (length(posData) == 0) {
      idxAggr <- idxAggr+1
    } else {
      exstData <- TRUE
    }
  }
  tsAggrBgn <- tsAggrBgn[idxAggr:length(tsAggrBgn)] # truncate aggregated time series vector
  
  # Take inventory of the flags we have and set up output naming
  numFlgs <- length(flgs) # #flags we have
  qfqm <- as.list(data) # copy variable names to output
  nameFlgs <- names(flgs) # list of flag names
  nameVarsOut <- c("mean","min","max","var","ste","numPts") # initialize the 1st 6 variable names in the output data
  nameVarsOutQm <- c("Pass","Fail","Na") # 3 subvariables per quality metric
  
  # Put together output variable names
  for (idxFlag in numeric(numFlgs)+1:numFlgs) {
    
    # Get rid of leading "posQf" if using output from def.plau
    if (regexpr(pattern="posQf",text=nameFlag[idxFlag],ignore.case=FALSE)[1] == 1) {
      tmp <- sub(pattern="posQf", replacement="", x=nameFlag[idxFlag], ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE)
    }
    # Get rid of leading "qf" if using other output 
    if (regexpr(pattern="qf",text=tmp,ignore.case=FALSE)[1] == 1) {
      tmp <- sub(pattern="qf", replacement="", x=tmp, ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE)
    }
    
    
    for (idxQm in 1:3) {
      nameVarsOut[6+(idxFlag-1)*3+idxQm] <- paste0("qm",tools::toTitleCase(nameFlag),nameVarsOutQm[idxQm],collapse ="")
    }
  }
  nameVarsOut <- c(nameVarsOut,"qmAlpha","qmBeta","qfFinl") # Add alpha QM, beta QM, and final quality flag
  
   # Assign variable names to output and initialize
  for (idxVar in 1:length(data)) {
    qfqm[[idxVar]] <- data.frame(matrix(nrow=length(tsAggrBgn),ncol=6+3*numFlgs+3)) # 6 for mean/min/max/etc. 3*#flags for fail, pass, NA, and 3 for alpha & beta QMs and final QF
    colnames(qfqm[[idxVar]]) <- nameVarsOut
  }
  

# Remove data points marked for exclusion -----------------------------------------


  # Get rid of points marked to exclude from L1 data
  nameVars <- names(data)
  for(idxVarData in 1:length(data)) {
    # Check whether list of exclusion flags is named same as variables or whether 
    # we are using col index only
    idxVarExcl <- which(names(FlagExcl) == nameVars[idxVarData])
    if(length(idxVarExcl) != 1) {
      # Bummer, we don't have a variable name match, attempt to use same variable index
      idxVarExcl <- idxVarData
      if(length(FlagExcl) < idxVarExcl) {
        stop("Cannot interpret which variables in input list FlagExcl match to which data variables. Make sure each variable in data has a matching variable name in FlagExcl, even if entry in FlagExcl is an empty vector.")
      }
    }

    
    # Remove bad data points
    for (idxFlagExcl in numeric(length(length(FlagExcl[[idxVarExcl]])))+1:length(FlagExcl[[idxVarExcl]])) {
      
      # Make sure the flag in FlagExcl has a matching flag in flgs
      idxFlagFlgs <- which(names(flgs) == FlagExcl[[idxVarExcl]][idxFlagExcl])
      if(length(idxFlagFlgs) != 1) {
        warning(paste0("Cannot find flag '",FlagExcl[[idxVarExcl]][idxFlagExcl],
                       "' (listed in input FlagExcl for variable '",nameVars[idxVarData],
                       "') in input flgs for variable . Cannot exclude data in this variable for this flag."))
        next
      }
      
      # Check whether list of flags is named same as variables in data or whether 
      # we are using col index only
      idxVarFlgs <- which(names(flgs[[FlagExcl[[idxVarExcl]][idxFlagExcl]]]$fail) == nameVars[idxVarData])
      if(length(idxVarFlgs) != 1) {
        # Bummer, we don't have a variable name match, attempt to use same variable index
        idxVarFlgs <- idxVarData
        if(length(flgs[[idxFlagFlgs]]$fail) < idxVarFlgs) {
          warning(paste0("Cannot find list of flagged indices for variable '",nameVars[idxVarData], 
                      "' and flag '",FlagExcl[[idxVarExcl]][idxFlagExcl],
                      "' within input flgs. Cannot exclude data in this variable for this flag."))
          next
        }
      }
      
      # If we made it this far, we found a list of indices to remove in this variable and flag. Let's do it.
      data[flgs[[idxFlagFlgs]]$fail[[idxVarFlgs]],idxVarData] <- NA
      
    }
  }



# Compute aggregate stats -------------------------------------------------

  # Loop through each variable
  for(idxVarData in 1:length(data)) {
    
    # Loop through each aggregation window
    for (idxAggr in 1:length(tsAggrBgn)) {
      
      # Find data locations in window
      posData <- which((ts >= tsAggrBgn[idxAggr]) & (ts < tsAggrBgn[idxAggr]+TintAggr))
      numDataAggr <- length(posData)
      
      # If there is no data to process move to next aggregation window
      if (numDataAggr == 0) {
        next
      }
      
      # Summary statistics
      qfqm[[idxVarData]]$mean[idxAggr] <- mean(data[posData,idxVarData],na.rm=TRUE) # Mean
      # When calculating min & max, we want NA if there is no non-NA data
      if (sum(!is.na(data[posData,idxVarData])) > 0) {
        qfqm[[idxVarData]]$min[idxAggr] <- min(data[posData,idxVarData],na.rm=TRUE) # Min 
        qfqm[[idxVarData]]$max[idxAggr] <- max(data[posData,idxVarData],na.rm=TRUE) # Max 
      } else {
        qfqm[[idxVarData]]$min[idxAggr] <- NA
        qfqm[[idxVarData]]$max[idxAggr] <- NA
      }
      qfqm[[idxVarData]]$var[idxAggr] <- var(data[posData,idxVarData],na.rm=TRUE) # Variance      
      qfqm[[idxVarData]]$numPts[idxAggr] <- length(which(!is.na(data[posData,idxVarData]))) # number of non-na points      
      qfqm[[idxVarData]]$ste[idxAggr] <- sqrt(qfqm[[idxVarData]]$var[idxAggr])/sqrt(qfqm[[idxVarData]]$numPts[idxAggr]) # Standard error of the mean    
      
      # Quality metrics
      posAlpha <- numeric(0) # initialize locations for alpha QM
      posBeta <- numeric(0) # initialize locations for beta QM
      posBetaNull <- numeric(0) # initialize beta locations attributed to null points
      for (idxFlag in numeric(numFlgs)+1:numFlgs){
        # Check whether list of flags is named same as variables or whether 
        # we are using col index only
        idxVarFlag <- which(names(flgs[[idxFlag]]$fail) == nameVars[idxVarData])
        if(length(idxVarExcl) != 1) {
          # Bummer, we don't have a variable name match, attempt to use same variable index
          idxVarFlag <- idxVarData
          if(length(flgs[[idxFlag]]$fail) < idxVarFlag) {
            stop(paste0("Cannot interpret which variables in input list '", nameFlgs[idxFlag], "' match to which data variables. Make sure each variable in data has a matching variable name in the deepest levels of 'flgs', even if that entry is an empty vector."))
          }
        }
        
        # Pass
        qfqm[[idxVarData]][[6+(idxFlag-1)*3+1]][idxAggr] <- length(setdiff(posData,
            union(flgs[[idxFlag]]$fail[[idxVarFlag]],flgs[[idxFlag]]$na[[idxVarFlag]])))/numDataAggr*100
        # Fail
        qfqm[[idxVarData]][[6+(idxFlag-1)*3+2]][idxAggr] <- length(intersect(posData,
            flgs[[idxFlag]]$fail[[idxVarFlag]]))/numDataAggr*100
        # NA
        qfqm[[idxVarData]][[6+(idxFlag-1)*3+3]][idxAggr] <- length(intersect(posData,
            flgs[[idxFlag]]$na[[idxVarFlag]]))/numDataAggr*100
        
        # Keep a running tally of alpha and beta locations
        posAlpha <- union(posAlpha,intersect(posData,flgs[[idxFlag]]$fail[[idxVarFlag]]))
        posBeta <- union(posBeta,intersect(posData,flgs[[idxFlag]]$na[[idxVarFlag]]))
        if (length(grep("null",nameFlgs[idxFlag],ignore.case=TRUE)) != 0)  {
          # We want to exclude beta points attributed to null flag, so save null points for later
          posBetaNull <- union(posBetaNull,intersect(posData,flgs[[idxFlag]]$fail[[idxVarFlag]]))
        }
      }

      # Tally up the alpha & beta QMs
      qfqm[[idxVarData]]$qmAlpha[idxAggr] <- length(posAlpha)/numDataAggr*100
      posBeta <- setdiff(posBeta,posBetaNull) # exclude beta locations attributed to null test
      qfqm[[idxVarData]]$qmBeta[idxAggr] <- length(posBeta)/numDataAggr*100
      
    }
    
    # Compute final quality flag
    qfqm[[idxVarData]]$qfFinl[(2*qfqm[[idxVarData]]$qmAlpha + qfqm[[idxVarData]]$qmBeta) >= 20] <- 1
    qfqm[[idxVarData]]$qfFinl[(2*qfqm[[idxVarData]]$qmAlpha + qfqm[[idxVarData]]$qmBeta) < 20] <- 0
  }
  
  
  
  # Return results
  return(list(tsAggrBgn=tsAggrBgn,qfqm=qfqm))
  
}