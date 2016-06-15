##############################################################################################
#' @title Resampling using rollapply() function

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2013-05-19)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Resampling using rollapply() function.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

resamp <- function(
  mat.raw,
  freq,
  freq_res,
  EIDAS)
  {
#--------------------------------------------------------------------------------------------
#RESAMPLING
#calculation of control variables
  #number of datasets to be averaged over
    avg_no=freq / freq_res
  #number of rows of the output data
#    rows=as.integer(nrow(mat.raw) / avg_no)

#allocate matrix
# mat.raw.res=matrix(nrow=rows, ncol=ncol(mat.raw),
#   dimnames = list(c(as.character(1:rows)),dimnames(mat.raw) [[2]]))

#actual resampling
for (i in 1:ncol(mat.raw)){

  dummy <- zoo::rollapply(zoo::zoo(mat.raw[,i]), avg_no, mean, na.rm=T, by=avg_no)
  if(i == 1) mat.raw.res <- as.matrix(dummy, ncol=1)
  if(i > 1) mat.raw.res <- cbind(mat.raw.res, as.matrix(dummy, ncol=1))

}
dimnames(mat.raw.res) = list(c(as.character(1:nrow(mat.raw.res))),dimnames(mat.raw) [[2]])


#overwriting the azimuth angle averages with the re-calculation from cartesian vector averages
if(EIDAS == T) {
  mat.raw.res[,22]<-def.conv.cart.az(mat.raw.res[,48:49])	#heading nacelle
  mat.raw.res[,38]<-def.conv.cart.az(mat.raw.res[,50:51])	#heading TCM
  mat.raw.res[,47]<-def.conv.cart.az(mat.raw.res[,52:53])	#DIRmet
}
return(mat.raw.res)
}
