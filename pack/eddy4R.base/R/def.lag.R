##############################################################################################
#' @title Definition function: Lag two datasets, so as to maximise their cross-correlation

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description 
#' Function definition. Lag two datasets, so as to maximise their cross-correlation.

#' @param refe A vector with variable in reference time frame. Of class numeric. [-]
#' @param meas A vector with variable in time frame to be adjusted. Of class numeric. [-]
#' @param freq Acquisition frequency of refe and meas. Of class ingeter. [Hz]
#' @param dataRefe A matrix or data.frame with all data that carries the time frame of refe. Defaults to refe. Of any class. [-]
#' @param dataMeas A matrix or data.frame with all data that carries the time frame of meas. Defaults to meas. Of any class. [-]
#' @param measVar A vector specifying if only several columns in dataMeas shall be lagged. Defaults to NULL. Of class integer or character. [-]
#' @param lagMax Maximum lag, by default 2 x freq. Of class integer. [-]
#' @param lagCnst TRUE - interpret lagMax as maximum permissible lag; FALSE - start with lagMax as first estimate and increase iteratively. Defaults to TRUE. Of class logical. [-]
#' @param lagNgtvPstv "n" - consider negative lag times only, i.e. meas is expected to lag behind refe; "p" - consider positive lag times only, i.e. refe is expected to lag behind meas; "np" - consider negative and positive lag times. Defaults to "np". Of class character. [-]
#' @param lagAll TRUE - consider positive and negative correlations when finding lag time; FALSE - consider positive correlations only when finding lag time. Defaults to TRUE. Of class logical. [-]
#' @param hpf TRUE - apply Butterworth high-pass filter; FALSE - use raw data. Defaults to TRUE. Of class logical. [-]
#' @param fracMin Minimum fraction of data to attempt lag determination. Defaults to 0.1. Of class numeric. [-]

#' @return Lagged input data and calculation results in a list consisting of:\cr
#' \code{dataRefe} The reference data.
#' \code{dataMeas} The data that was lagged to coincide with the reference data.
#' \code{lag} The number of data points by which the lag correction was performed.
#' \code{corrCros} The cross-correction coefficient between refe and meas for the determined lag time.

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
#   Natchaya P-Durden (2018-04-03)
#     update @param format
#   Natchaya P-Durden (2018-04-11)
#    applied eddy4R term name convention; replaced pos by idx
#   Natchaya P-Durden (2019-07-24)
#    add fail safe when lag is equal to NA
##############################################################################################


#lag two datasets, so as to maximise their correlations
def.lag <- function(
  refe,
  meas,
  freq,
  dataRefe = refe,
  dataMeas = meas,
  measVar = NULL,
  lagMax = 2 * freq,
  lagCnst = TRUE,
  lagNgtvPstv = c("n", "p", "np")[3],
  lagAll = TRUE,
  hpf = TRUE,
  fracMin = 0.1
) {
  
  ###
  #start escape if too few non-NAs in period
  if(length(na.omit(cbind(refe, meas))) / length(refe) < 0.1) {
    ###
    
    
    
    rpt <- list(
      dataRefe=dataRefe,
      dataMeas=dataMeas,
      lag=NA,
      corrCros=NA
    )
    return(rpt)
    
    
    
    ###
    #mid escape if too few non-NAs in period  
  } else {
    ###  
    
    
    
    #assign reference and refeervations
    if(is.null(measVar)) {
      dataRefe <- as.matrix(dataRefe)
      dataMeas <- as.matrix(dataMeas)
    } else {
      colRefe <- ncol(dataRefe)
      dataRefe <- cbind(dataRefe, dataMeas)
      dataMeas <- as.matrix(dataMeas[,measVar])
    }
    
    #fill gaps via linear interpolation
    refe <- approx(x=zoo::index(refe), y=refe, xout=zoo::index(refe))[[2]]
    meas <- approx(x=zoo::index(meas), y=meas, xout=zoo::index(meas))[[2]]
    
    #get rid of NAs at start and end
    naNot <- na.omit(data.frame(refe=refe, meas=meas))
    refe <- naNot$refe
    meas <- naNot$meas
    rm(naNot)
    
    #apply high-pass filter    
    if(hpf == TRUE) {
      
      #create high-pass filter
      #nyquist frequency [Hz]
      freqSplt <- freq / 2
      #cutoff frequency [Hz]
      freqThsh <- 1 / (2 * lagMax / freq)
      #butterworth filter        
      filt <- signal::butter(n=4, W=freqThsh/freqSplt, type="high")
      
      #apply high-pass filter
      #discard two filter lengths from start and end        
      filtTmp <- -c(1:(freq/freqThsh * 3), (length(refe)-(freq/freqThsh * 3)):length(refe)) 
      #actual application
      idxRefe <- signal::filtfilt(filt, refe)[filtTmp]
      #           plot(refe[filtTmp], type="l")
      #           lines(I(idxRefe + mean(refe)), col=2)
      idxMeas <- signal::filtfilt(filt, meas)[filtTmp]
      #           plot(meas[filtTmp], type="l")
      #           lines(I(idxMeas + mean(meas[filtTmp])), col=2)
      
      #assign results
      refe <- idxRefe
      meas <- idxMeas
      
      #clean up
      rm(filtTmp)
      
    }
    
    
    #find correct lag time    
    #for hard lagMax argument
    if(lagCnst == TRUE) {
      
      #calculate autocorrelation
      corr <- ccf(refe, meas, lag.max = lagMax, plot = FALSE, na.action = na.pass)
      #consider negative lag times only: set correlations for positive lag time to zero
      if(lagNgtvPstv == "n") corr$acf[which(corr$lag > 0)] <- 0          
      #consider positive lag times only: set correlations for negative lag time to zero
      if(lagNgtvPstv == "p") corr$acf[which(corr$lag < 0)] <- 0
      #determine lag time
      lag <- ifelse(lagAll == FALSE,
                    corr$lag[which(corr$acf == max(corr$acf))],		#(-): meas lags behind refe
                    corr$lag[which(abs(corr$acf) == max(abs(corr$acf)))]
      )      
      #don't lag if determined lag equals lagMax
      if(!is.na(lag) & (abs(lag) == lagMax)) lag <- 0
      
      #for soft lagMax argument
    } else {
      
      lag <- lagMax
      count <- 1
      while(abs(lag) == lagMax) {
        #increase lagMax argument
        if(count > 1) lagMax <- 2 * lagMax
        #calculate autocorrelation
        corr <- ccf(refe, meas, lag.max = lagMax, plot = FALSE, na.action = na.pass)
        #consider negative lag times only: set correlations for positive lag time to zero
        if(lagNgtvPstv == "n") corr$acf[which(corr$lag > 0)] <- 0          
        #consider positive lag times only: set correlations for negative lag time to zero
        if(lagNgtvPstv == "p") corr$acf[which(corr$lag < 0)] <- 0
        #determine lag time
        lag <- ifelse(lagAll == FALSE,
                      corr$lag[which(corr$acf == max(corr$acf))],		#(-): meas lags behind refe
                      corr$lag[which(abs(corr$acf) == max(abs(corr$acf)))]
        )
        count <- count + 1
      }
      
    }
    
    #adjust entire dataMeas time series to dataRefe time (assuming constant timing offset over all variables)
    #refe <- lag(refe, k=lag*freq)
    #meas data lags behind refe
    if(!is.na(lag)){
    if(lag < 0) {
      dataRefe <- dataRefe[1:(nrow(dataRefe) + lag),]
      dataMeas <- dataMeas[(1 - lag):(nrow(dataMeas)),]
    }
    #refe data lags behind meas
    if(lag > 0) {
      #lag <- lag + 1	#necessary to achieve correct displacement (nested test with ccf)
      dataRefe <- dataRefe[(1 + lag):(nrow(dataRefe)),]
      dataMeas <- dataMeas[1:(nrow(dataMeas) - lag),]
    }
    #    length(dataMeas)	#50174
    
    #if only certain variables within dataMeas shall be lagged:
    if(is.null(measVar)) {
      refeOut <- as.matrix(dataRefe)
      measOut <- as.matrix(dataMeas)
    } else {
      refeOut <- as.matrix(dataRefe[,1:colRefe])
      measOut <- as.matrix(dataRefe[,(colRefe+1):ncol(dataRefe)])
      measOut[,measVar] <- dataMeas
    }
    
    #prepare output
    if(ncol(refeOut) == 1) refeOut <- refeOut[,1]
    if(ncol(measOut) == 1) measOut <- measOut[,1]
    rpt <- list(
      dataRefe=refeOut,
      dataMeas=measOut,
      lag=lag,
      corrCros=ifelse(lagAll==TRUE, max(abs(corr$acf)), max(corr$acf))
    )
    return(rpt)
    #end if !is.na(lag)
    }else{
      rpt <- list(
        dataRefe=dataRefe,
        dataMeas=dataMeas,
        lag=NA,
        corrCros=NA
      )
      return(rpt)
    }
    
    
    ###
  }
  #end escape if too few non-NAs in period  
  ###
  
  
  
}
