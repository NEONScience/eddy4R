##############################################################################################
#' @title Definition function: Composing azimuth angle from cartesian vector data

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description Composing azimuth angle from cartesian vector data.

#' @param cart variable of type numeric, matrix consisting of unit vectors X (1st column, -1 ... 1, positive to north) and Y (2nd column, -1 ... 1, positive to east) 

#' @return clockwise azimuth angle with 0 / 360 degree discontinuity in north [decimal degrees]

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords unit vector

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2009-03-10)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-02-12)
#     initail naming convention for eddy4R
#   Natchaya P-Durden (2016-11-26)
#     rename function to def.az.cart()
#   Natchaya P-Durden (2016-12-02)
#     rename function to def.pol.cart()
#   Natchaya P-Durden (2018-04-03)
#     update @param format
##############################################################################################

#convert cartesian vector data to angular degree; convetion:
#cart[,1], X, S(+), N(-) -> positiv TO N
#cart[,2], Y, W(+), E(-) -> positiv TO E
def.pol.cart <- function(cart){
#  sector.o=vector(length=nrow(cart))
#  sector.o[which(cart[,2]>= 0)] <-0	#E wind vector component
#  sector.o[which(cart[,2]< 0)] <-360	#W wind vector component
  az <- (atan2(cart[,2],cart[,1])*180/pi)+180
return(az)
}

#test angular decompostion and retransformation from decomposed coordinate system
#angular decompostion:
#wdir=seq(0,359)
#wdir=mat.raw[,22]
#cart=matrix(nrow=length(wdir),ncol=2)
#cart[,1]<-	-cos(wdir/180*pi)	#U, positiv from N
#cart[,2]<-	-sin(wdir/180*pi)	#V, positiv from E
#retransformation:
#  sector.o=vector(length=nrow(cart))
#  sector.o[which(cart[,2]>= 0)] <-0	#E wind vector component
#  sector.o[which(cart[,2]< 0)] <-360	#W wind vector component
#  wdir2<-(atan2(cart[,2],cart[,1])*180/pi)+180	#sector.o
#plot(wdir,wdir2,type="l")


#test for trigonometric function on wind component coordinate system:
#test11<-seq(-10,0,by=0.1)
#test12<-seq(0,10,by=0.1)
#test13<-seq(10,0,by=-0.1)
#test14<-seq(0,-10,by=-0.1)
#cartXaxe=c(test11,test12,test13,test14)
#cartYaxe=c(test14,test11,test12,test13)
#plot(cartXaxe,cartYaxe,type="l")
#sector.o=vector(length=length(cartXaxe))
#sector.o[which(cartXaxe < 0 & cartYaxe < 0)] <-0
#sector.o[which(cartXaxe > 0 & cartYaxe < 0)] <-90
#sector.o[which(cartXaxe > 0 & cartYaxe > 0)] <-180
#sector.o[which(cartXaxe < 0 & cartYaxe > 0)] <-270
#sector.o[which(cartXaxe < 0 )] <-0
#sector.o[which(cartXaxe >= 0)] <-180
#dir.test<-(atan2(cartYaxe,cartXaxe)*180/pi)+180
#plot(cartYaxe,dir.test,type="l")
