##############################################################################################
#' @title Resampling using zoo() function

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2012-04-02)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Resampling using zoo() function.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

resamp1<-function(mat.raw,freq,freq_res,EIDAS) {
#--------------------------------------------------------------------------------------------
#RESAMPLING
  #creating cartesian vector angles from azimuth angles
#mat.raw<-rawdata
  if(EIDAS == T) {
    mat.raw<-cbind(mat.raw,def.conv.az.cart(mat.raw[,22]))	#heading nacelle
    mat.raw<-cbind(mat.raw,def.conv.az.cart(mat.raw[,38]))	#heading TCM
  }

  #calculation of control variables
  avg_no=freq/freq_res			#number of datasets to be averaged over
  rows=as.integer(nrow(mat.raw)/avg_no)	#number of rows of the output data

  #allocate matrix
  mat.raw.res=matrix(nrow=rows, ncol=ncol(mat.raw),
    dimnames = list(c(as.character(1:rows)),dimnames(mat.raw) [[2]]))

  #actual resampling
  for (i in 1:ncol(mat.raw)){
    mat.raw.res[,i]<-zoo::rollapply(zoo(mat.raw[,i]),avg_no,mean,na.rm=T,by=avg_no)
  }

  #overwriting the azimuth angle averages with the re-calculation from cartesian vector averages
  if(EIDAS == T) {
    mat.raw.res[,22]<-def.conv.cart.az(mat.raw.res[,(ncol(mat.raw)-3):(ncol(mat.raw)-2)])	#heading nacelle
    mat.raw.res[,38]<-def.conv.cart.az(mat.raw.res[,(ncol(mat.raw)-1):(ncol(mat.raw)-0)])	#heading TCM
    mat.raw.res<-mat.raw.res[,1:(ncol(mat.raw)-4)]
  }

return(mat.raw.res)
}



##############################################################################################
#' @title Resampling using filter() function

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2012-04-02)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Resampling using filter() function.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
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

