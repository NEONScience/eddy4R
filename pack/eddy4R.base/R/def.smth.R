##############################################################################################
#' @title Definition function: Data smoothing

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description Function definition.Data smoothing method.

#' @param \code{data} A vector containing the input data. Of class "numeric" or "integer". []
#' @param \code{span} A Vector containing the span values. Of class "numeric" or "integer". The option \code{span} controls the degree of smoothing. For example; using span = c(15) to apply a long smoothing filter, or using spans = c(3,3) to apply two short filters in succession. [-]
#' @param \code{smthFunc} An object of class string containing the smooth functions ("smthKern"). []

#' @return Estimated smooth values and of the same length as \code{data}. []

#' @references 
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16 \cr
#' http://www.stat.rutgers.edu/home/rebecka/Stat565/lab42007.pdf \cr

#' @keywords Kernel smoother

#' @examples 
#' def.smth(data = rnorm(100), span = c(15), smthFunc = c("smthKern"))
#' def.smth(data = rnorm(100), span = c(3,3))

#' @seealso
#' \code{\link[stats]{kernel}}, \code{\link[stats]{kernapply}}

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-07-23)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-04-28)
#     Initail naming convention for eddy4R
##############################################################################################
#KERNEL SMOOTHER

def.smth <- function(data, span, smthFunc = c("smthKern")) {
#http://www.stat.rutgers.edu/home/rebecka/Stat565/lab42007.pdf: We reduce variance by applying a frequency domain smoothing filter. The option spans controls the degree of smoothing. For example; you can apply a long smoothing filter with spans=c(15), or two short filters in succession with spans=c(3,3).

#Smoothing data using the Kernel method
  if(smthFunc == "smthKern") {
#configure kernel
  kern <- stats::kernel("modified.daniell", span)
  #df <- df.kernel(kern)
  #bandwidth <- bandwidth.kernel(kern)

#apply kernel smoothing
  if(is.null(ncol(data))) {
    
    rpt <- stats::kernapply(data, kern, circular = TRUE)
    
  } else {
    
    for(i in 1:ncol(data)) {
      if(i == 1) rpt <- stats::kernapply(data[,i], kern, circular = TRUE)
      if(i > 1)  rpt <- cbind(rpt, stats::kernapply(data[,i], kern, circular = TRUE))
    }
    
    dimnames(rpt) <- dimnames(data)
  }
}
#return outputs
  return(rpt)
# end function def.smth()

}
