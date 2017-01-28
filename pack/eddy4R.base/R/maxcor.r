##############################################################################################
#' @title Definition function: Lag two datasets, so as to maximise their cross-correlation

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function definition. Lag two datasets, so as to maximise their cross-correlation.

#' @param \code{ref} A vector with variable in reference time frame. Of class numeric. [-]
#' @param \code{obs} A vector with variable in time frame to be adjusted. Of class numeric. [-]
#' @param \code{freq_loc} Acquisition frequency of ref and obs. Of class ingeter. [Hz]
#' @param \code{refDATA} A matrix or data.frame with all data that carries the time frame of ref. Defaults to ref. Of any class. [-]
#' @param \code{obsDATA} A matrix or data.frame with all data that carries the time frame of obs. Defaults to obs. Of any class. [-]
#' @param \code{obsVARS} A vector specifying if only several columns in obsDATA shall be lagged. Defaults to NULL. Of class integer or character. [-]
#' @param \code{maxlag} Maximum lag, by default 2 x freq_loc. Of class integer. [-]
#' @param \code{hardlag} TRUE - interpret maxlag as maximum permissible lag; FALSE - start with maxlag as first estimate and increase iteratively. Defaults to TRUE. Of class logical. [-]
#' @param \code{nplag} "n" - consider negative lag times only, i.e. obs is expected to lag behind ref; "p" - consider positive lag times only, i.e. ref is expected to lag behind obs; "np" - consider negative and positive lag times. Defaults to "np". Of class character. [-]
#' @param \code{absolute} TRUE - consider positive and negative correlations when finding lag time; FALSE - consider positive correlations only when finding lag time. Defaults to TRUE. Of class logical. [-]
#' @param \code{hpf} TRUE - apply Butterworth high-pass filter; FALSE - use raw data. Defaults to TRUE. Of class logical. [-]
#' @param \code{FRACmin} Minimum fraction of data to attempt lag determination. Defaults to 0.1. Of class numeric. [-]

#' @return Lagged input data and calculation results in a list consisting of:\cr
#' \code{refDATA} The reference data.
#' \code{obsDATA} The data that was lagged to coincide with the reference data.
#' \code{lag} The number of data points by which the lag correction was performed.
#' \code{ccf} The cross-correction coefficient between ref and obs for the determined lag time.

#' @references 
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16.

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
##############################################################################################


#lag two datasets, so as to maximise their correlations
maxcor <- function(
  ref,
  obs,
  freq_loc,
  refDATA = ref,
  obsDATA = obs,
  obsVARS = NULL,
  maxlag = 2 * freq_loc,
  hardlag = TRUE,
  nplag = c("n", "p", "np")[3],
  absolute = TRUE,
  hpf = TRUE,
  FRACmin = 0.1
) {
  
  ###
  #start escape if too few non-NAs in period
  if(length(na.omit(cbind(ref, obs))) / length(ref) < 0.1) {
    ###
    
    
    
    output <- list(
      refDATA=refDATA,
      obsDATA=obsDATA,
      lag=NA,
      ccf=NA
    )
    return(output)
    
    
    
    ###
    #mid escape if too few non-NAs in period  
  } else {
    ###  
    
    
    
    #assign reference and observations
    if(is.null(obsVARS)) {
      refDATA <- as.matrix(refDATA)
      obsDATA <- as.matrix(obsDATA)
    } else {
      refcol <- ncol(refDATA)
      refDATA <- cbind(refDATA, obsDATA)
      obsDATA <- as.matrix(obsDATA[,obsVARS])
    }
    
    #fill gaps via linear interpolation
    ref <- approx(x=zoo::index(ref), y=ref, xout=zoo::index(ref))[[2]]
    obs <- approx(x=zoo::index(obs), y=obs, xout=zoo::index(obs))[[2]]
    
    #get rid of NAs at start and end
    dum_NA <- na.omit(data.frame(ref=ref, obs=obs))
    ref <- dum_NA$ref
    obs <- dum_NA$obs
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
      whr_not <- -c(1:(freq_loc/cutoff * 3), (length(ref)-(freq_loc/cutoff * 3)):length(ref)) 
      #actual application
      ref_loc <- signal::filtfilt(bf1, ref)[whr_not]
      #           plot(ref[whr_not], type="l")
      #           lines(I(ref_loc + mean(ref)), col=2)
      obs_loc <- signal::filtfilt(bf1, obs)[whr_not]
      #           plot(obs[whr_not], type="l")
      #           lines(I(obs_loc + mean(obs[whr_not])), col=2)
      
      #assign results
      ref <- ref_loc
      obs <- obs_loc
      
      #clean up
      rm(whr_not)
      
    }
    
    
    #find correct lag time    
    #for hard maxlag argument
    if(hardlag == TRUE) {
      
      #calculate autocorrelation
      lagt <- ccf(ref, obs, lag.max = maxlag, plot = FALSE, na.action = na.pass)
      #consider negative lag times only: set correlations for positive lag time to zero
      if(nplag == "n") lagt$acf[which(lagt$lag > 0)] <- 0          
      #consider positive lag times only: set correlations for negative lag time to zero
      if(nplag == "p") lagt$acf[which(lagt$lag < 0)] <- 0
      #determine lag time
      lag <- ifelse(absolute == FALSE,
                    lagt$lag[which(lagt$acf == max(lagt$acf))],		#(-): obs lags behind ref
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
        lagt <- ccf(ref, obs, lag.max = maxlag, plot = FALSE, na.action = na.pass)
        #consider negative lag times only: set correlations for positive lag time to zero
        if(nplag == "n") lagt$acf[which(lagt$lag > 0)] <- 0          
        #consider positive lag times only: set correlations for negative lag time to zero
        if(nplag == "p") lagt$acf[which(lagt$lag < 0)] <- 0
        #determine lag time
        lag <- ifelse(absolute == FALSE,
                      lagt$lag[which(lagt$acf == max(lagt$acf))],		#(-): obs lags behind ref
                      lagt$lag[which(abs(lagt$acf) == max(abs(lagt$acf)))]
        )
        count <- count + 1
      }
      
    }
    
    #adjust entire obsDATA time series to refDATA time (assuming constant timing offset over all variables)
    #ref <- lag(ref, k=lag*freq)
    #obs data lags behind ref
    if(lag < 0) {
      refDATA <- refDATA[1:(nrow(refDATA) + lag),]
      obsDATA <- obsDATA[(1 - lag):(nrow(obsDATA)),]
    }
    #ref data lags behind obs
    if(lag > 0) {
      #lag <- lag + 1	#necessary to achieve correct displacement (nested test with ccf)
      refDATA <- refDATA[(1 + lag):(nrow(refDATA)),]
      obsDATA <- obsDATA[1:(nrow(obsDATA) - lag),]
    }
    #    length(obsDATA)	#50174
    
    #if only certain variables within obsDATA shall be lagged:
    if(is.null(obsVARS)) {
      refOUT <- as.matrix(refDATA)
      obsOUT <- as.matrix(obsDATA)
    } else {
      refOUT <- as.matrix(refDATA[,1:refcol])
      obsOUT <- as.matrix(refDATA[,(refcol+1):ncol(refDATA)])
      obsOUT[,obsVARS] <- obsDATA
    }
    
    #prepare output
    if(ncol(refOUT) == 1) refOUT <- refOUT[,1]
    if(ncol(obsOUT) == 1) obsOUT <- obsOUT[,1]
    output <- list(
      refDATA=refOUT,
      obsDATA=obsOUT,
      lag=lag,
      ccf=ifelse(absolute==TRUE, max(abs(lagt$acf)), max(lagt$acf))
    )
    return(output)
    
    
    
    ###
  }
  #end escape if too few non-NAs in period  
  ###
  
  
  
}
