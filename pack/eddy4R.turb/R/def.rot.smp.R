#' Define Simple Rotation of Wind Vectors
#' 
#' Perform various types of wind vector rotations incl. single, double
#' 
#' @param data data.frame containing veloXaxs, veloYaxs and veloZaxs \cr
#'             vectors should be defined correspondingly as u/v/w == E/N/U == x/y/z [data.frame]
#' @param MethRot method of rotation to be used, one of: \itemize{
#'                \item "single" - rotate into the mean wind
#'                \item "double" - apply single rotation and additionally rotate to align w and z (minimise w)
#'                \item "none" - perform no rotation
#' }
#' @return Data object with rotated wind vectors names as veloXaxs, veloYaxs and veloZaxs
#' 
#' @references Code adapted from REYNFlux_P5 (35ceda9) and flow.turb.tow.neon.dp04.r (914a9e9) \cr
#'  Stefan Metzger / Dave Durden / Natchaya P-Durden / Cove Sturtevant / Ke Xu
#' @author
#' David Durden \email{ddurden@battelleecology.org}
#' @author W. S. Drysdale
#' 
#' @export
#' 

def.rot.smp = function(data, MethRot = c("single","double","none")[2]{
  
  
  # rotation angle (mnPSI_uv = veloYaxsXaxsMean)
  veloYaxsXaxsMean = eddy4R.base::def.pol.cart(matrix(c(mean(data$veloYaxsErth, na.rm = TRUE),
                                                mean(data$veloXaxsErth, na.rm = TRUE)),
                                              ncol=2))
  
  #Check rotation type
  if(MethRot %in% c("single","double")){
    rotAngXaxs <- (eddy4R.base::def.unit.conv(data=(veloYaxsXaxsMean+180),unitFrom="deg",unitTo="rad")) %% (2*pi)
    
    mtrx01 <- matrix(nrow=3, ncol=3)
    mtrx01[1,1] <- cos(rotAngXaxs)
    mtrx01[1,2] <- sin(rotAngXaxs)
    mtrx01[1,3] <- 0.
    mtrx01[2,1] <- -sin(rotAngXaxs)
    mtrx01[2,2] <- cos(rotAngXaxs)
    mtrx01[2,3] <- 0.
    mtrx01[3,1] <- 0.
    mtrx01[3,2] <- 0.
    mtrx01[3,3] <- 1.
    BT <- t(mtrx01)
    dfVelo <- rbind(data$veloYaxsErth, data$veloXaxsErth, data$veloZaxsErth)
    dfVeloRot <- mtrx01 %*% dfVelo
    
    if(MethRot == "double"){
      #second rotation
      rotAngYaxs = atan2(mean(dfVeloRot[3,],na.rm = TRUE),mean(dfVeloRot[1,],na.r = TRUE))
      mtrx02 <- matrix(nrow=3, ncol=3)
      mtrx02[1,1] <- cos(rotAngYaxs)
      mtrx02[1,2] <- 0
      mtrx02[1,3] <- sin(rotAngYaxs)
      mtrx02[2,1] <- 0
      mtrx02[2,2] <- 1
      mtrx02[2,3] <- 0
      mtrx02[3,1] <- -sin(rotAngYaxs)
      mtrx02[3,2] <- 0
      mtrx02[3,3] <- cos(rotAngYaxs)
      mtrx = mtrx01 %*% mtrx02
      BT = t(mtrx)
      dfVeloRot = mtrx02 %*% dfVeloRot
    }
    
    data$veloXaxsErth <- dfVeloRot[1,]
    data$veloYaxsErth <- -dfVeloRot[2,]
    data$veloZaxsErth <- dfVeloRot[3,]
    
    #Add attribute for Xaxs rotation (into mean wind)
    attributes(data)$rotAngXaxs <- rotAngXaxs
    #Add attribute for Yaxs rotation
    attributes(data)$rotAngYaxs <- ifelse(MethRot == "double", rotAngYaxs, NA)
    
  }
  
  if(MethRot == "none"){
    return(data)
  }
  
}