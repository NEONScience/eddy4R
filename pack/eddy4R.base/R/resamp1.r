##############################################################################################
#' @title Resampling function

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Natchaya Pingintha-Durden

#' @description Function definition. Resampling data by changing the sample rate from high frequency to low frequency.

#' @param \code{data} A vector or matrix of type numeric, containing the data to be converted
#' @param \code{FreqInp} Input sampling or measurements frequency. Of class "numeric" or "integer". [Hz]
#' @param \code{FreqOut} Desired out put frequency. Of class "numeric" or "integer". [Hz]
#' @param \code{MethResp} An object of class "character" containing the resampling method ("zoo","filt"). [-]
#' @param \code{Col} Specific column in \code{data} which containing the discontinuity variable e.g. azimuth angle of wind direction. Defaults to NULL. [-] 


#' @return Currently none

#' @references Currently none
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2012-04-02)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-06-02)
#     Initail naming convention for eddy4R
##############################################################################################

#resamp1<-function(mat.raw,freq,freq_res,EIDAS) {#
def.resp <- function (data, FreqInp, FreqOut, MethResp, Col=NULL) {
#--------------------------------------------------------------------------------------------
#RESAMPLING
  #creating cartesian vector angles from azimuth angles
#mat.raw<-rawdata
  #if(EIDAS == T) {#
    #mat.raw<-cbind(mat.raw,def.conv.az.cart(mat.raw[,22]))	#heading nacelle#
    #mat.raw<-cbind(mat.raw,def.conv.az.cart(mat.raw[,38]))	#heading TCM#
  #}#
if (!is.null(Col)){
  #creating a new data frame for the discontinuity variables
  colData <-data[Col]
  temp01 <-c()
  
  #converting the discontinuity variables from azimuth angles to cartesian vector angles
  for (i in 1:length(Col)){
    
    temp01[[i]]<- def.conv.az.cart(colData[,i]) 
    
  }
  data <- cbind(data,temp01)
}

  #calculation of control variables
  #avg_no=freq/freq_res			#number of datasets to be averaged over#
  NumSamp = FreqInp/FreqOut #number of datasets to be averaged over
  #rows=as.integer(nrow(mat.raw)/avg_no)	#number of rows of the output data#
  NumRow = as.integer(nrow(data)/NumSamp) #number of rows of the output data
  
#allocate matrix
 # mat.raw.res=matrix(nrow=rows, ncol=ncol(mat.raw),#
  #  dimnames = list(c(as.character(1:rows)),dimnames(mat.raw) [[2]])) #

  #actual resampling
  #for (i in 1:ncol(mat.raw)){#
   # mat.raw.res[,i]<-zoo::rollapply(zoo(mat.raw[,i]),avg_no,mean,na.rm=T,by=avg_no)#
  #}#

#resampling method1 by using rollapply in zoo
if (MethResp == "zoo"){
  
  for (i in 1:ncol(data)){
    
    temp02 <- zoo::rollapply(zoo::zoo(data[,i]), NumSamp, mean, na.rm=T, by=NumSamp)
    if(i == 1) temp03 <- temp02 else temp03 <- cbind(temp03, temp02)
  }
}

#resampling method1 by using filter() function 
if (MethResp == "filt"){
  
  for(i in 1:ncol(data)) {
    temp02 <- stats::filter(data[,i], rep(1 / NumSamp, NumSamp), sides=2)
    if(i == 1) temp03 <- temp02 else temp03 <- cbind(temp03, temp02)
  }  
  
}

dimnames(temp03)[[2]] <- dimnames(data)[[2]]

#discard non-used data intervals
whr <- seq(ceiling(NumSamp), nrow(temp03), by=NumSamp)
rpt <- temp03[whr,]



  #overwriting the azimuth angle averages with the re-calculation from cartesian vector averages
  #if(EIDAS == T) {#
    #mat.raw.res[,22]<-def.conv.cart.az(mat.raw.res[,(ncol(mat.raw)-3):(ncol(mat.raw)-2)])	#heading nacelle#
   # mat.raw.res[,38]<-def.conv.cart.az(mat.raw.res[,(ncol(mat.raw)-1):(ncol(mat.raw)-0)])	#heading TCM #
  #  mat.raw.res<-mat.raw.res[,1:(ncol(mat.raw)-4)] #
  #}#
if (!is.null(Col)){
  
  for (i in 1:length(Col)){
  
  j <- 2*(length(Col))
  m <- ncol(rpt)-j
  n <- 2*i-1
  
  col01 <- (m+n) #indicated 1st column of cartesian vector variable that willbe used to calculate the azimuth angle
  col02 <- (m+n+1) #indicated 2nd column of cartesian vector variable that willbe used to calculate the azimuth angle
  
  rpt[,Col[i]] <- def.conv.cart.az(rpt[,col01:col02])  
}
rpt <- rpt[,1:(2*(length(Col)))]  #delete
}


return(data.frame(rpt))
}



##############################################################################################
#' @title Resampling using filter() function

#' @author Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description Resampling using filter() function.

#' @param Currently none

#' @return Currently none

#' @references Currently none
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2012-04-02)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-06-02)
#     Initail naming convention for eddy4R
##############################################################################################

#--------------------------------------------------------------------------------------------
#RESAMPLING USING INTERNAL FILTER() FUNCTION (faster)

resamp2 <- function(
  data,
  freq,
  freq_res,
  EIDAS
) {
  
  #creating cartesian vector angles from azimuth angles
    if(EIDAS == T) {
      data <- cbind(data, def.conv.az.cart(data[,22]))  #heading nacelle
      data <- cbind(data, def.conv.az.cart(data[,38]))	#heading TCM
    }

  #control variable
    factor <- freq / freq_res

  #moving average
    for(d in 1:ncol(data)) {
      dum <- stats:::filter(data[,d], rep(1 / factor, factor), sides=2)
      if(d == 1) out <- dum else out <- cbind(out, dum)
    }    
    dimnames(out)[[2]] <- dimnames(data)[[2]]
  
  #discard non-used data intervals
    whr_o <- seq(ceiling(factor), nrow(out), by=factor)
    OUT <- out[whr_o,]
    
  #overwriting the azimuth angle averages with the re-calculation from cartesian vector averages
    if(EIDAS == T) {
      OUT[,22] <- def.conv.cart.az(OUT[,(ncol(OUT)-3):(ncol(OUT)-2)])	#heading nacelle
      OUT[,38] <- def.conv.cart.az(OUT[,(ncol(OUT)-1):(ncol(OUT)-0)])	#heading TCM
      OUT <- OUT[,1:(ncol(OUT)-4)]
    }

  #clean up
    rm(factor, dum, out, whr_o)
    
  #return result
    return(data.frame(OUT))
    
}

