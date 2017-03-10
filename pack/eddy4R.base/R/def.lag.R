##############################################################################################
#' @title Definition function: Lag two datasets, so as to maximise their cross-correlation

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function definition. Lag two datasets, so as to maximise their cross-correlation.

#' @param \code{refe} A vector with variable in reference time frame. Of class numeric. [-]
#' @param \code{meas} A vector with variable in time frame to be adjusted. Of class numeric. [-]
#' @param \code{freq_loc} Acquisition frequency of refe and meas. Of class ingeter. [Hz]
#' @param \code{refeData} A matrix or data.frame with all data that carries the time frame of refe. Defaults to refe. Of any class. [-]
#' @param \code{measData} A matrix or data.frame with all data that carries the time frame of meas. Defaults to meas. Of any class. [-]
#' @param \code{measVar} A vector specifying if only several columns in measData shall be lagged. Defaults to NULL. Of class integer or character. [-]
#' @param \code{maxlag} Maximum lag, by default 2 x freq_loc. Of class integer. [-]
#' @param \code{hardlag} TRUE - interpret maxlag as maximum permissible lag; FALSE - start with maxlag as first estimate and increase iteratively. Defaults to TRUE. Of class logical. [-]
#' @param \code{nplag} "n" - consider negative lag times only, i.e. meas is expected to lag behind refe; "p" - consider positive lag times only, i.e. refe is expected to lag behind meas; "np" - consider negative and positive lag times. Defaults to "np". Of class character. [-]
#' @param \code{absolute} TRUE - consider positive and negative correlations when finding lag time; FALSE - consider positive correlations only when finding lag time. Defaults to TRUE. Of class logical. [-]
#' @param \code{hpf} TRUE - apply Butterworth high-pass filter; FALSE - use raw data. Defaults to TRUE. Of class logical. [-]
#' @param \code{FRACmin} Minimum fraction of data to attempt lag determination. Defaults to 0.1. Of class numeric. [-]

#' @return Lagged input data and calculation results in a list consisting of:\cr
#' \code{refeData} The reference data.
#' \code{measData} The data that was lagged to coincide with the reference data.
#' \code{lag} The number of data points by which the lag correction was performed.
#' \code{ccf} The cross-correction coefficient between refe and meas for the determined lag time.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords average, aggregate, descriptive statistics

#' @examples Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-29)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Stefan Metzger (2017-01-28)
#     partial update to eddy4R terms
#   Hongyan Luo (2017-03-10)
#     update to eddy4R terms
##############################################################################################


#lag two datasets, so as to maximise their correlations
def.lag <- function(
  refe,
  meas,
  freq_loc,
  refeData = refe,
  measData = meas,
  measVar = NULL,
  maxlag = 2 * freq_loc,
  hardlag = TRUE,
  nplag = c("n", "p", "np")[3],
  absolute = TRUE,
  hpf = TRUE,
  FRACmin = 0.1
) {
  
  ###
  #start escape if too few non-NAs in period
  if(length(na.omit(cbind(refe, meas))) / length(refe) < 0.1) {
    ###
    
    
    
    output <- list(
      refeData=refeData,
      measData=measData,
      lag=NA,
      ccf=NA
    )
    return(output)
    
    
    
    ###
    #mid escape if too few non-NAs in period  
  } else {
    ###  
    
    
    
    #assign reference and refeervations
    if(is.null(measVar)) {
      refeData <- as.matrix(refeData)
      measData <- as.matrix(measData)
    } else {
      refecol <- ncol(refeData)
      refeData <- cbind(refeData, measData)
      measData <- as.matrix(measData[,measVar])
    }
    
    #fill gaps via linear interpolation
    refe <- approx(x=zoo::index(refe), y=refe, xout=zoo::index(refe))[[2]]
    meas <- approx(x=zoo::index(meas), y=meas, xout=zoo::index(meas))[[2]]
    
    #get rid of NAs at start and end
    dum_NA <- na.omit(data.frame(refe=refe, meas=meas))
    refe <- dum_NA$refe
    meas <- dum_NA$meas
    rm(dum_NA)
    
    #apply high-pass filter    
    if(hpf == TRUE) {
      
      #create high-pass filter
      #nyquist frequency [Hz]
      NY <- freq_loc / 2
      #cutoff frequency [Hz]
      cutoff <- 1 / (2 * maxlag / freq_loc)
      #butterworth filter        
      bf1 <- signal::butter(n=4, W=cutoff/NY, type="high")
      
      #apply high-pass filter
      #discard two filter lengths from start and end        
      whr_not <- -c(1:(freq_loc/cutoff * 3), (length(refe)-(freq_loc/cutoff * 3)):length(refe)) 
      #actual application
      refe_loc <- signal::filtfilt(bf1, refe)[whr_not]
      #           plot(refe[whr_not], type="l")
      #           lines(I(refe_loc + mean(refe)), col=2)
      meas_loc <- signal::filtfilt(bf1, meas)[whr_not]
      #           plot(meas[whr_not], type="l")
      #           lines(I(meas_loc + mean(meas[whr_not])), col=2)
      
      #assign results
      refe <- refe_loc
      meas <- meas_loc
      
      #clean up
      rm(whr_not)
      
    }
    
    
    #find correct lag time    
    #for hard maxlag argument
    if(hardlag == TRUE) {
      
      #calculate autocorrelation
      lagt <- ccf(refe, meas, lag.max = maxlag, plot = FALSE, na.action = na.pass)
      #consider negative lag times only: set correlations for positive lag time to zero
      if(nplag == "n") lagt$acf[which(lagt$lag > 0)] <- 0          
      #consider positive lag times only: set correlations for negative lag time to zero
      if(nplag == "p") lagt$acf[which(lagt$lag < 0)] <- 0
      #determine lag time
      lag <- ifelse(absolute == FALSE,
                    lagt$lag[which(lagt$acf == max(lagt$acf))],		#(-): meas lags behind refe
                    lagt$lag[which(abs(lagt$acf) == max(abs(lagt$acf)))]
      )      
      #don't lag if determined lag equals maxlag
      if(abs(lag) == maxlag) lag <- 0
      
      #for soft maxlag argument
    } else {
      
      lag <- maxlag
      count <- 1
      while(abs(lag) == maxlag) {
        #increase maxlag argument
        if(count > 1) maxlag <- 2 * maxlag
        #calculate autocorrelation
        lagt <- ccf(refe, meas, lag.max = maxlag, plot = FALSE, na.action = na.pass)
        #consider negative lag times only: set correlations for positive lag time to zero
        if(nplag == "n") lagt$acf[which(lagt$lag > 0)] <- 0          
        #consider positive lag times only: set correlations for negative lag time to zero
        if(nplag == "p") lagt$acf[which(lagt$lag < 0)] <- 0
        #determine lag time
        lag <- ifelse(absolute == FALSE,
                      lagt$lag[which(lagt$acf == max(lagt$acf))],		#(-): meas lags behind refe
                      lagt$lag[which(abs(lagt$acf) == max(abs(lagt$acf)))]
        )
        count <- count + 1
      }
      
    }
    
    #adjust entire measData time series to refeData time (assuming constant timing offset over all variables)
    #refe <- lag(refe, k=lag*freq)
    #meas data lags behind refe
    if(lag < 0) {
      refeData <- refeData[1:(nrow(refeData) + lag),]
      measData <- measData[(1 - lag):(nrow(measData)),]
    }
    #refe data lags behind meas
    if(lag > 0) {
      #lag <- lag + 1	#necessary to achieve correct displacement (nested test with ccf)
      refeData <- refeData[(1 + lag):(nrow(refeData)),]
      measData <- measData[1:(nrow(measData) - lag),]
    }
    #    length(measData)	#50174
    
    #if only certain variables within measData shall be lagged:
    if(is.null(measVar)) {
      refeOUT <- as.matrix(refeData)
      measOUT <- as.matrix(measData)
    } else {
      refeOUT <- as.matrix(refeData[,1:refecol])
      measOUT <- as.matrix(refeData[,(refecol+1):ncol(refeData)])
      measOUT[,measVar] <- measData
    }
    
    #prepare output
    if(ncol(refeOUT) == 1) refeOUT <- refeOUT[,1]
    if(ncol(measOUT) == 1) measOUT <- measOUT[,1]
    output <- list(
      refeData=refeOUT,
      measData=measOUT,
      lag=lag,
      ccf=ifelse(absolute==TRUE, max(abs(lagt$acf)), max(lagt$acf))
    )
    return(output)
    
    
    
    ###
  }
  #end escape if too few non-NAs in period  
  ###
  
  
  
}
