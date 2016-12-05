##############################################################################################
#' @title Decomposing azimuth angles to cartesian vectors

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya P-Durden

#' @description Decompose azimuth angles from polar to cartesian for angular averaging. 

#' @param \code{az} vector of type numeric, clockwise azimuth angle with 0 / 360 degree discontinuity in north [decimal degrees]

#' @return matrix consisting of unit vectors X (1st column, -1 ... 1, positive to north) and Y (2nd column, -1 ... 1, positive to east) 

#' @references Currently none

#' @keywords unit vector

#' @examples def.cart.pol(20)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2012-03-11)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-02-04)
#     initail naming convention for eddy4R
#   Natchaya P-Durden (2016-11-26)
#     rename to def.cart.az()
#   Natchaya P-Durden (2016-12-02)
#     rename to def.cart.pol()
##############################################################################################

#decompose heading from polar to cartesian for angular averaging
#input in decimal degrees
def.cart.pol <- function(az){
cart<-matrix(nrow=length(az),ncol=2)
cart[,1]<- -cos(az/180*pi)	#U, S(+), N(-) -> positive TO N
cart[,2]<- -sin(az/180*pi)	#V, W(+), E(-) -> positive TO E
cart
}
