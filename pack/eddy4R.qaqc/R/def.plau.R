##############################################################################################
#' @title Plausibility tests (Range, Step, Persistence, Null, Gap) 

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description 
#' Function definition. Determines inplausible data indices based on user-specified limits for the data range, step between adjacent values, persistence (similarity of adjacent values), nulls, and gaps.

#' @param \code{data} Required input. A data frame containing the data to be evaluated (do not include the time stamp vector here). 
#' @param \code{ts} Optional. A time vector of class POSIXlt of times corresponding with each row in data. Defaults to an evenly spaced time vector starting from system time of execution by seconds. 
#' @param \code{RngMin} Optional. A numeric vector of length equal to number of variables in data containing the minimum acceptable value for each variable. Defaults to observed minimums (no flags will result)
#' @param \code{RngMax} Optional. A numeric vector of length equal to number of variables in data containing the maximum acceptable value for each variable. Defaults to observed maximums (no flags will result)
#' @param \code{DiffStepMax} Optional. A numeric vector of length equal to number of variables in data containing the maximum acceptable absolute difference between sequential data points for each variable. Defaults to observed maximum (no flags will result)
#' @param \code{DiffPersMin} Optional. A numeric vector of length equal to number of variables in data containing the minimum absolute change in value over the interval specified in TintPers to indicate the sensor is not "stuck". Defaults to a vector of zeros (no flags will result).
#' @param \code{TintPers} Optional. A difftime object of length equal to number of variables in data specifying the time interval for each variable over which to test for the minimum absolute change in value specified in DiffPersMin. Defaults to 60 x median observed time difference. Class difftime can be generated using as.difftime.
#' @param \code{TestNull} Optional. Apply the null test? A logical vector of [TRUE or FALSE] of length equal to number of variables in data. Defaults to FALSE (no null values are flagged)
#' @param \code{NumGap} Optional.  A numeric value >= 1, interpretable as an integer, specifying the numer of consecutive NA values constituting a gap. Default is the one more than the length of the data series (no gaps will be flagged)
#' @param \code{Vrbs} Optional. A logical {FALSE/TRUE} value indicating whether to:\cr
#' \code{Vrbs = FALSE}: (Default) output the vector positions of the fail and na results for each test (default), or \cr
#' \code{Vrbs = TRUE}: output a data frame for each variable in data, with a column for each plausibility test outputting the actual quality flags [-1,0,1] for each data point

#' @return If:\cr
#' \code{Vrbs = FALSE} A list of flags giving the failed and NA positions for each the Range, Step, Persistence, Null, and Gap tests. Each flag is itself a nested list of failed and na (unable to eval) flagged indices for each variable in data. \cr
#' \code{Vrbs = TRUE} A list of variables matching those in \code{data}, each containing a data frame with a column for each plausibility test outputting the actual quality flags [-1,0,1] for each data point, where -1 indicates the test could not be evaluated, 0 indicates a pass, and 1 indicates a fail
#' 

#' @references 
#' NEON Algorithm Theoretical Basis Document QA/QC Plausibility Testing (NEON.DOC.011081)
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16
#' 
#' @keywords NEON QAQC, plausibility, range, step, persistence, null, gap

#' @examples Currently none

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
##############################################################################################


def.plau <- function (
  data,                               # a data frame containing the data to be evaluated (do not include the time stamp vector here). Required input.
  ts = as.POSIXlt(seq.POSIXt(from=Sys.time(),by="sec",length.out=length(data[,1]))),  # time vector corresponding with the rows in data, in  Class "POSIXlt", which is a named list of vectors representing sec, min, hour,day,mon,year. Defaults to an evenly spaced time vector starting from execution by seconds.
  RngMin = apply(data,2,min,na.rm=TRUE), # a numeric vector containing the minimum acceptable value for each variable in data, defaults to observed minimums
  RngMax = apply(data,2,max,na.rm=TRUE), # a numeric vector containing the maximum acceptable value for each variable in data, defaults to observed maximums
  DiffStepMax = apply(abs(apply(data,2,diff)),2,max,na.rm=TRUE), # a vector containing the maximum acceptable absolute difference between sequential data points for each variable in data
  DiffPersMin = rep.int(0,length(data)), # a vector containing the minimum absolute change in value for each variable in data over the interval specified in TintPers. Defaults to a vector of zeros.
  TintPers = 60*median(abs(diff(ts)),na.rm=TRUE)*rep.int(1,length(data)), # a vector of class difftime specifying the time interval for each variable in data over which to test for the minimum absolute change in value specified in DiffPersMin. Defaults to 60 x median observed time difference. Class difftime can be generated using as.difftime.
  TestNull = rep(FALSE,length(data)), # apply the null test? A logical vector of [TRUE or FALSE] of length equal to number of variables in data. Defaults to FALSE (no null values are flagged)
  NumGap = rep(length(data[,1])+1,length(data)), # an integer greater than 0 specifying the number of consecutive NA values that constitute a gap
  Vrbs = FALSE # FALSE = output the vector positions of the fail and na results, TRUE = output flag values for each test
) {
  
  
# Error Checking ----------------------------------------------------------

  # Check data
  if(missing("data") | !is.data.frame(data)) {
    stop("Required input 'data' must be a data frame")
  }
  
  # Check ts
  ts <- try(as.POSIXlt(ts),silent=TRUE)
  numData <- length(data[,1])
  if(class(ts)[1] == "try-error"){
    stop("Input variable ts must be of class POSIXlt")
  } else if (length(ts) != numData) {
    stop("Length of input variable ts must be equal to length of data.")
  } 
  
  # Check RngMin & RngMax
  if((!is.numeric(RngMin)) | (!is.numeric(RngMax))) {
    stop("Input parameters RngMin and RngMax must be numeric vectors.")
  } else if ((length(RngMin) != length(data)) | (length(RngMax) != length(data))) {
    warning("Length of input parameters RngMin or RngMax not equal to number of data variables. Using first element of each for all variables.")
    RngMin <- rep(RngMin[1],length(data))
    RngMax <- rep(RngMax[1],length(data))
  }
  
  # Check DiffStepMax
  if(!is.numeric(DiffStepMax)) {
    stop("Input parameter DiffStepMax must be a numeric vector.")
  } else if (length(DiffStepMax) != length(data)) {
    warning("Length of input parameter DiffStepMax not equal to number of data variables. Using first element of DiffStepMax for all variables.")
    DiffStepMax <- rep(DiffStepMax[1],length(data))
  }
  
  # Check DiffPersMin
  if((!is.numeric(DiffPersMin))) {
    stop("Input parameter DiffPersMin must be a numeric vector.")
  } else if (length(DiffPersMin) != length(data)) {
    warning("Length of input parameter DiffPersMin not equal to number of data variables. Using first element of DiffPersMin for all variables.")
    DiffPersMin <- rep(DiffPersMin[1],length(data))  
  }

  # Check TintPers
  TintPers <- try(as.difftime(TintPers),silent=TRUE)
  if(class(TintPers) == "try-error"){
    stop("Input parameter TintPers must be a difftime object")
  } else if (length(TintPers) != length(data)) {
    warning("Length of input parameter TintPers not equal to number of data variables. Using first element of TintPers for all variables.")
    TintPers <- TintPers[1]*rep.int(1,length(data))  
  } 

  # Check TestNull
  if(!is.logical(TestNull)) {
    stop("Input parameter TestNull must be a logical vector.")
  } else if (length(TestNull) != length(data)) {
    warning("Length of input parameter TestNull not equal to number of data variables. Using first element of TestNull for all variables.")
    TestNull <- rep(TestNull[1],length(data))
  }
  
  # Check NumGap
  if (length(NumGap) != length(data)) {
    warning("Length of input parameter NumGap not equal to number of data variables. Using first element of NumGap for all variables.")
    NumGap <- rep(NumGap[1],length(data)) 
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
  numVar <- length(data) # Get number of variables 
  nameData <- names(data) # Get variable names
  
  # For verbose option, initialize output
  if(Vrbs) {
    # set up a data frame for each variable containing all the plausibility tests
    qfDum = matrix(data=0,nrow=numData,ncol=5) # Default to pass for each test
    qfDum <- as.data.frame(qfDum)
    names(qfDum) <- c("qfRng","qfStep","qfPers","qfNull","qfGap")

    # Dole out the qfs to each variable 
    qf <- vector("list",length=numVar)
    names(qf) <- nameData
    qf <- lapply(qf,FUN = function(x){x=qfDum})
  }
  
  # Do range test
  posFlagRng <- list(fail=as.list(data),na=as.list(data)) # initialize null test output
  for(idxVar in 1:length(data)) {
    posFlagRng$fail[[idxVar]] <- which((data[,idxVar] < RngMin[idxVar]) | (data[,idxVar] > RngMax[idxVar]))
    posFlagRng$na[[idxVar]] <- which(is.na(data[,idxVar]))
    
    # For Verbose option, output actual flag values
    if(Vrbs) {
      qf[[idxVar]]$qfRng[posFlagRng$fail[[idxVar]]] <- 1
      qf[[idxVar]]$qfRng[posFlagRng$na[[idxVar]]] <- -1
    }
  }
  
    
  # Do step test
  posFlagStep <- list(fail=as.list(data),na=as.list(data)) # initialize step test output
  for(idxVar in 1:length(data)) {
    posFlagStep$fail[[idxVar]] <- which((abs(diff(data[,idxVar])) > DiffStepMax[idxVar]))
    posFlagStep$fail[[idxVar]] <- unique(c(posFlagStep$fail[[idxVar]],posFlagStep$fail[[idxVar]]+1))
    posFlagStep$na[[idxVar]] <- which(is.na(diff(data[,idxVar])))+1
    
    # If previous point is null, but next value is present, evaluate the step test with next value
    posFlagStep$na[[idxVar]] <- setdiff(posFlagStep$na[[idxVar]],which(!is.na(diff(data[,idxVar]))))
  
    # For Verbose option, output actual flag values
    if(Vrbs) {
      qf[[idxVar]]$qfStep[posFlagStep$fail[[idxVar]]] <- 1
      qf[[idxVar]]$qfStep[posFlagStep$na[[idxVar]]] <- -1
    }  
  }

  # Do persistence test
  posFlagPers <- list(fail=as.list(data),na=as.list(data)) # initialize persistence test output
  for(idxVar in 1:length(data)) {
    
    # Let users know persistence test may take some time
    if (DiffPersMin[idxVar] > 0) {
      print(paste0("Running persistence test for variable ",nameData[idxVar], ". This may take some time..."))
    }
    
    posFlagPers$fail[[idxVar]] <- numeric(length=0) # Initialize output
    posFlagPers$na[[idxVar]] <- which(is.na(data[,idxVar])) # Initialize output
    
    idxDataSt <- 1 # initialize starting index
    
    # Make sure we aren't on a null value
    while(is.na(data[idxDataSt,idxVar])) {
      idxDataSt <- idxDataSt+1
    }
    idxDataMin <- idxDataSt # initialize index of running min
    idxDataMax <- idxDataSt # intialize index of running max
    idxData <- 2 # intialize index position
    
    while((idxData <= numData) & (DiffPersMin[idxVar] > 0)) {
      
      # Is this a null value?
      if(is.na(data[idxData,idxVar])) {
        idxData <- idxData+1
        next
      }
      
      # Is the value at this index the running max or min?
      if(data[idxData,idxVar] < data[idxDataMin,idxVar]) {
        idxDataMin <- idxData
      } else if(data[idxData,idxVar] > data[idxDataMax,idxVar]) {
        idxDataMax <- idxData
      }
      
      # Is diff between max and min at or larger than the persistence threshold
      if(data[idxDataMax,idxVar]-data[idxDataMin,idxVar] >= DiffPersMin[idxVar]) {
        
        # We've hit the threshold, now check wether we are beyond the allowable time interval
        if(ts[idxData]-ts[idxDataSt] <= TintPers[idxVar]) {
          # Hooray! The data is not "stuck"
          idxDataSt <- min(c(idxDataMin,idxDataMax))+1 # set start of next window to the next point after the lower of the running min and max
          
          # Make sure we aren't on a null value
          while(is.na(data[idxDataSt,idxVar])) {
            idxDataSt <- idxDataSt+1
          }
          
          idxDataMin <- idxDataSt # reset running minimum
          idxDataMax <- idxDataSt # reset running maximum
          idxData <- idxDataSt+1 # reset the next point to be evaluated
          
        } else {
          
          # We might have a stuck sensor, but first let's check whether we blew the time threshold b/c 
          # all the data were NA prior to this point
          if (sum(!is.na(data[idxDataSt:(idxData-1),idxVar])) <= 1) {
            
            # Data were all NA after starting index, mark as cannot evaluate
            posFlagPers$na[[idxVar]] <- union(posFlagPers$na[[idxVar]],idxDataSt:(idxData-1))
            
          } else {
            
            # Awe bummer, the sensor was stuck before this point.
            posFlagPers$fail[[idxVar]] <- unique(c(posFlagPers$fail[[idxVar]],idxDataSt:(idxData-1)))
            
            # Don't mark the NA values as fail
            posFlagPers$fail[[idxVar]] <- setdiff(posFlagPers$fail[[idxVar]],posFlagPers$na[[idxVar]])
          }
          
          idxDataSt <- idxData # restart the test from here
          idxData <- idxDataSt+1 # reset the next point to be evaluated
        } 
        
      } else if ((idxData == numData) & (ts[idxData]-ts[idxDataSt] > TintPers[idxVar])) {

        # We didn't hit the threshold and we've reached the end of the data. We are also beyond the allowable 
        # time interval for the persistence test, so let's flag the data
        posFlagPers$fail[[idxVar]] <- unique(c(posFlagPers$fail[[idxVar]],idxDataSt:idxData))

        # Don't mark the NA values as fail
        posFlagPers$fail[[idxVar]] <- setdiff(posFlagPers$fail[[idxVar]],posFlagPers$na[[idxVar]])
        
        idxData <- idxData+1 # We're done
      
      } else {
        
        # We didn't pass the minimum acceptable change on this point, move to the next
        idxData <- idxData+1
      }
    }
    # If we reached the end of the data but the last value was NA, we need to go back and evaluate the last
    # non-NA value
    idxData <- numData
    if (is.na(data[idxData,idxVar])) {
      # Get to last non-NA point
      while(is.na(data[idxData,idxVar])) {
        idxData <- idxData-1
      }
      
      if (ts[idxData]-ts[idxDataSt] > TintPers[idxVar]) {
        # We didn't hit the threshold for the final non-NA points and we were beyond the allowable 
        # time interval for the persistence test, so let's flag the end of the data
        posFlagPers$fail[[idxVar]] <- unique(c(posFlagPers$fail[[idxVar]],idxDataSt:idxData))
        
        # Don't mark the NA values as fail
        posFlagPers$fail[[idxVar]] <- setdiff(posFlagPers$fail[[idxVar]],posFlagPers$na[[idxVar]])
        
      } else {
        # We didn't hit the threshold for the final non-NA points, but we are not yet beyond the 
        # allowable time interval, so let's flag as unable to evaluate
        posFlagPers$na[[idxVar]] <- unique(c(posFlagPers$na[[idxVar]],idxDataSt:idxData))
      }
    }
    
    # For Verbose option, output actual flag values
    if(Vrbs) {
      qf[[idxVar]]$qfPers[posFlagPers$fail[[idxVar]]] <- 1
      qf[[idxVar]]$qfPers[posFlagPers$na[[idxVar]]] <- -1
    }  
  }

  # Do Null test
  posFlagNull <- list(fail=as.list(data),na=as.list(data)) # initialize null test output
  for(idxVar in 1:length(data)) {
    posFlagNull$fail[[idxVar]] <- numeric(length=0)
    posFlagNull$na[[idxVar]] <- numeric(length=0) # there is never an instance where we cannot evaluate the null test
    if(TestNull[idxVar]) {
      posFlagNull$fail[[idxVar]] <- which(is.na(data[,idxVar]))
    }
    
    # For Verbose option, output actual flag values
    if(Vrbs) {
      qf[[idxVar]]$qfNull[posFlagNull$fail[[idxVar]]] <- 1
      qf[[idxVar]]$qfNull[posFlagNull$na[[idxVar]]] <- -1
    }
    
  }
  

  # Do Gap test
  posFlagGap <- list(fail=as.list(data),na=as.list(data)) # initialize gap test output
  for(idxVar in 1:length(data)) {
    posFlagGap$na[[idxVar]] <- numeric(length=0) # there is never an instance where we cannot evaluate the gap test
    
    posNull <- which(is.na(data[,idxVar])) # find NA values
    posGap <- posNull # Start out thinking every Null is a gap, we'll whittle it down below
    diffPosNull <- diff(posNull) # difference between null position
    
    # Only evaluate this if we need to
    if (length(posNull) >= NumGap[idxVar]) {
      
      # Go thru each NA value to determine if it is part of a set >= NumGap[idxVar]
      for (idxNull in 1:length(posNull)) {
        
        # Isolate positions within the NumGap[idxVar] range that are consecutive
        diffPosNullSelf <- c(diffPosNull[1:idxNull-1],1,diffPosNull[idxNull:length(diffPosNull)])# Fill in the position difference vector with a value of 1 for idxNull itself
        posNullPre <- seq(from=idxNull-NumGap[idxVar],to=idxNull-1,by=1)
        posNullPre <- rev(posNullPre[(posNullPre > 0) & (posNullPre <= numData)])
        numNullPre <- which(diffPosNullSelf[posNullPre]!=1)[1]-1 # number of consecutive nulls prior to this Null
        if (is.na(numNullPre)) {
          numNullPre <- length(posNullPre)
        }
        posNullPost <- seq(from=idxNull,to=idxNull+NumGap[idxVar]-1,by=1)
        posNullPost <- posNullPost[(posNullPost > 0) & (posNullPost <= numData)]
        numNullPost <- which(diffPosNullSelf[posNullPost]!=1)[1]-1  # number of consecutive nulls including and after this Null
        if (is.na(numNullPost)) {
          numNullPost <- length(posNullPost)
        }
        
        if (numNullPre+numNullPost < NumGap[idxVar]) {
          # This position is not within a gap, so remove it from out list
          posGap <- setdiff(posGap,posNull[idxNull])
        }        
      }
      
      posFlagGap$fail[[idxVar]] <- posGap
      
    } else {
      posFlagGap$fail[[idxVar]] <- numeric(length=0) # No gaps
    }
     
    # For Verbose option, output actual flag values
    if(Vrbs) {
      qf[[idxVar]]$qfGap[posFlagGap$fail[[idxVar]]] <- 1
      qf[[idxVar]]$qfGap[posFlagGap$na[[idxVar]]] <- -1
    } 
  }

  # Return results
  if(!Vrbs) {
    rpt <- list(
      posFlagRng = posFlagRng,
      posFlagStep = posFlagStep,
      posFlagPers = posFlagPers,
      posFlagNull = posFlagNull,
      posFlagGap = posFlagGap)
    
  } else {
    rpt <- qf
    
  }
  

  return(rpt)
  
}