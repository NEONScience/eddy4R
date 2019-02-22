#' Define Rotation of Wind Vectors
#' 
#' Perform either single or double rotation of wind vectors. TODO: include planar fit rotation based on suplied coefficients
#' 
#' @param data input data to REYNflux [data.frame]
#' @param mn mean of input data. created during "TIME SERIES AVERAGES" step [data.frame]
#' @param rotType type of rotation to be performed, one of "single", "double" or "planarFit" (once implemented) [character vector]
#' @param plnrFitCoef coefficients for planar fit [numeric vector]
#' 
#' @return list containing updated data and mn objects, also B and BT rotation matricies in single or double cases
#' 
#' @author W. S. Drysdale
#' 
#' @export

wrap.rot = function(data,
                   mn,
                   rotType = c("single","double","planarFit")[1],
                   plnrFitCoef = NULL)
  {
  if(rotType %in% c("single","double")){
    #rotation angle
    rotang <- (eddy4R.base::def.unit.conv(data=(mn$PSI_uv+180),unitFrom="deg",unitTo="rad")) %% (2*pi)
    
    B <- matrix(nrow=3, ncol=3)
    B[1,1] <- cos(rotang)
    B[1,2] <- sin(rotang)
    B[1,3] <- 0.
    B[2,1] <- -sin(rotang)
    B[2,2] <- cos(rotang)
    B[2,3] <- 0.
    B[3,1] <- 0.
    B[3,2] <- 0.
    B[3,3] <- 1.
    BT <- t(B)
    U <- rbind(data$v_met, data$u_met, data$w_met)
    Urot <- B %*% U
    
    if(rotType == "double"){
      #second rotation
      rotang_v = atan2(mean(Urot[3,],na.rm = T),mean(Urot[1,],na.r = T))
      B2 <- matrix(nrow=3, ncol=3)
      B2[1,1] <- cos(rotang_v)
      B2[1,2] <- 0
      B2[1,3] <- sin(rotang_v)
      B2[2,1] <- 0
      B2[2,2] <- 1
      B2[2,3] <- 0
      B2[3,1] <- -sin(rotang_v)
      B2[3,2] <- 0
      B2[3,3] <- cos(rotang_v)
      B = B %*% B2
      BT = t(B)
      Urot = B2 %*% Urot
    }
  }
  
  if(rotType == "planarFit"){
    warning("Planar Fit is not yet implemented into def.rot - use existing planar fit code")
    
    ret = list(data,
               mn)
    return(ret)
  }
  
  data$u_hor <- Urot[1,]
  data$v_hor <- -Urot[2,]
  data$w_hor <- Urot[3,]
  mn$u_hor <- mean(Urot[1,], na.rm=TRUE)
  mn$v_hor <- mean(Urot[2,], na.rm=TRUE)
  mn$w_hor <- mean(Urot[3,], na.rm=TRUE)

  #Construnt return list
  ret = list(data = data,
       mn = mn)
  
  if(rotType %in% c("single","double")){
    ret$B = B
    ret$BT = BT
  }
  
  #return
  ret
  
}
