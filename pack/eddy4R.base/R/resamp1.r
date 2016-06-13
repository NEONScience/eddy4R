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

def.resp <- function (data, FreqInp, FreqOut, MethResp, Col=NULL) {
  
#creating a new data frame for the discontinuity variables
if (!is.null(Col)){
  
  colData <-data[Col]
  temp01 <-c()
  
  #converting the discontinuity variables e.g. wind direction data from azimuth angles to cartesian vector angles
  for (i in 1:length(Col)){
    
    temp01[[i]]<- def.conv.az.cart(colData[,i]) 
    
  }
  data <- cbind(data,temp01)
}

  #calculation of control variables
  NumSamp = FreqInp/FreqOut #number of datasets to be averaged over
  NumRow = as.integer(nrow(data)/NumSamp) #number of rows of the output data
  
#resampling method1 by using rollapply in zoo
if (MethResp == "zoo"){
  
  for (i in 1:ncol(data)){
    temp02 <- zoo::rollapply(zoo::zoo(data[,i]), NumSamp, mean, na.rm=T, by=NumSamp)
    if(i == 1) rpt <- temp02 else rpt <- cbind(rpt, temp02)
  }
}

#resampling method2 by using filter() function 
if (MethResp == "filt"){
  
  for(i in 1:ncol(data)) {
    temp02 <- stats::filter(data[,i], rep(1 / NumSamp, NumSamp), sides=2)
    if(i == 1) temp03 <- temp02 else temp03 <- cbind(temp03, temp02)
  }  
  #discard non-used data intervals
  idx <- seq(ceiling(NumSamp), nrow(temp03), by=NumSamp)
  rpt <- temp03[idx,] 
}

dimnames(rpt)[[2]] <- dimnames(data)[[2]]

#overwriting the azimuth angle averages with the re-calculation from average of cartesian vector 
if (!is.null(Col)){
  
  for (i in 1:length(Col)){
  
  j <- 2*(length(Col))
  m <- ncol(rpt)-j
  n <- 2*i-1
  
  col01 <- (m+n) #indicated 1st column of cartesian vector variable that willbe used to calculate the azimuth angle
  col02 <- (m+n+1) #indicated 2nd column of cartesian vector variable that willbe used to calculate the azimuth angle
  
  rpt[,Col[i]] <- def.conv.cart.az(rpt[,col01:col02])  
}

rpt <- rpt[,1:(ncol(rpt)-(2*(length(Col))))]  #discard non-used columns (the cartesian vector data)

}

rpt <- data.frame(rpt,row.names = NULL) 

return(rpt)

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

