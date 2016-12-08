##############################################################################################
#' @title Resampling function

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}
#' Natchaya Pingintha-Durden

#' @description Function definition. Resampling data by changing the sample rate from high frequency to low frequency.

#' @param \code{data} A vector or matrix of type numeric, containing the data to be resampled. [user-defined]
#' @param \code{FreqInp} Input sampling or measurements frequency. Of class "numeric" or "integer". [Hz]
#' @param \code{FreqOut} Desired out put frequency. Of class "numeric" or "integer". [Hz]
#' @param \code{MethRsmp} An object of class "character" containing the resampling method ("zoo","filt"). [-]
#' @param \code{ColDisc} Specific column in \code{data} which containing the discontinuity variable e.g. azimuth angle of wind direction. Of class "numeric" (column number) or "character" (column name). Defaults to NULL. [-] 

#' @return Data frame of resampling data and of the same number of variables as \code{data}. [user-defined]

#' @references Currently none
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @keywords resampling

#' @examples
#' #Generate data frame
#' df <- data.frame("A"= runif(200, min=0, max=360),"B" = runif(200, min=0.1, max=10), "C" = rnorm(200))
#' df1 <- def.rsmp(data = df,FreqInp = 10,FreqOut = 5, MethRsmp = "filt",ColDisc = c(1))
#' df2 <- def.rsmp(data = df,FreqInp = 10,FreqOut = 0.5, MethRsmp = "zoo",ColDisc = c("A"))

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

def.rsmp <- function (data, FreqInp, FreqOut, MethRsmp, ColDisc=NULL) {
  
#creating a new data frame for the discontinuity variables
if (!is.null(ColDisc)){
  
  colData <-data[ColDisc] #select the discontinuity variables
  tmp01 <-c() #temporary vector for the discontinuity variables
  
  #converting the discontinuity variables e.g. wind direction data from azimuth angles to cartesian vector angles
  for (i in 1:length(ColDisc)){
    
    tmp01[[i]]<- eddy4R.base::def.cart.pol(colData[,i]) 
    
  }
  data <- cbind(data,tmp01)
}

  #calculation of control variables
  NumSamp = FreqInp/FreqOut #number of datasets to be averaged over
  #NumRow = as.integer(nrow(data)/NumSamp) #number of rows of the output data
  
#resampling method1 by using rollapply in zoo
if (MethRsmp == "zoo"){
  
  for (i in 1:ncol(data)){
    tmp02 <- zoo::rollapply(zoo::zoo(data[,i]), NumSamp, mean, na.rm=TRUE, by=NumSamp)
    if(i == 1) rpt <- tmp02 else rpt <- cbind(rpt, tmp02)
  }
}

#resampling method2 by using filter() function 
if (MethRsmp == "filt"){
  
  for(i in 1:ncol(data)) {
    tmp02 <- stats::filter(data[,i], rep(1 / NumSamp, NumSamp), sides=2)
    if(i == 1) tmp03 <- tmp02 else tmp03 <- cbind(tmp03, tmp02)
  }  
  #discard non-used data intervals
  idx <- base::seq(ceiling(NumSamp), nrow(tmp03), by=NumSamp)
  rpt <- tmp03[idx,] 
}

dimnames(rpt)[[2]] <- dimnames(data)[[2]]

#overwriting the azimuth angle averages with the re-calculation from average of cartesian vector 
if (!is.null(ColDisc)){
  
  for (i in 1:length(ColDisc)){
  
  j <- 2*(length(ColDisc))
  m <- ncol(rpt)-j
  n <- 2*i-1
  
  col01 <- (m+n) #indicated 1st column of cartesian vector variable that willbe used to calculate the azimuth angle
  col02 <- (m+n+1) #indicated 2nd column of cartesian vector variable that willbe used to calculate the azimuth angle
  
  rpt[,ColDisc[i]] <- eddy4R.base::def.pol.cart(rpt[,col01:col02])  
}

rpt <- rpt[,1:(ncol(rpt)-(2*(length(ColDisc))))]  #discard non-used columns (the cartesian vector data)

}

rpt <- data.frame(rpt,row.names = NULL) 

#return outputs
return(rpt)
# end function def.rsmp()

}