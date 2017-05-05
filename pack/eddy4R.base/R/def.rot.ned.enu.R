# Test scripts for angle conversions

angNED <- rnorm(10, 275, 10)
x <- sin(angNED)
x <- cos(angNED)
y <- sin(angNED)
angNEDrad <- angNED * (pi/180)


Mbg <- function(roll, pich, hdng, XYZ) {
  #allocate ouput matrix North / East / Down
  NED=matrix(nrow=length(hdng), ncol=3)
  
  #carry out rotation
  NED[,1]=
    cos(hdng)*cos(pich)*XYZ[,1] +
    (-sin(hdng)*cos(roll)+cos(hdng)*sin(pich)*sin(roll))*XYZ[,2] +
    (sin(hdng)*sin(roll)+cos(hdng)*sin(pich)*cos(roll))*XYZ[,3]
  NED[,2]=
    (sin(hdng)*cos(pich))*XYZ[,1] + 
    (cos(hdng)*cos(roll)+sin(hdng)*sin(roll)*sin(pich))*XYZ[,2] +
    (sin(hdng)*sin(pich)*cos(roll)-cos(hdng)*sin(roll))*XYZ[,3]
  NED[,3]=
    (-sin(pich))*XYZ[,1] +
    (cos(pich)*sin(roll))*XYZ[,2] +
    (cos(pich)*cos(roll))*XYZ[,3]
  
  #return the result
  veloTA2GC<-list(list(NED[,1], NED[,2], NED[,3]))
  attributes(veloTA2GC)$names<-"veloTA2GC"
  attributes(veloTA2GC$veloTA2GC)$names<-c("x","y","z")
  return(veloTA2GC)
}



Mgm<-function(X) {
  #define permutation matrix Mgm
  Y1<-c(0, 1, 0)
  Y2<-c(1, 0, 0)
  Y3<-c(0, 0,-1)
  Y<-rbind(Y1, Y2, Y3)
  #perform permutation
  dumy<-X %*% Y
  #export
  export<-list(list(dumy[,1], dumy[,2], dumy[,3]))
  attributes(export)$names<-"veloMCwind"
  attributes(export$veloMCwind)$names<-c("u","v","w")
  return(export)
}