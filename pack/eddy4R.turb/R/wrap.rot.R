#' Define Rotation of Wind Vectors
#' 
#' Perform various types of wind vector rotations incl. single, double, planar fit.
#' 
#' @param data data.frame containing u_met, v_met and w_met [data.frame]
#' @param MethRot method of rotation to be used, one of "single", "double" or "planarFit".\cr
#'  "none" can be supplied to perform no rotation [character vector]
#' @param plnrFitCoef coefficients for planar fit. [numeric vector or data.frame] \cr 
#' Depending on plnrFitType, plnrFitCoef should be supplied as the following structure: \itemize{
#'                    \item simple - numeric vector constant of coefficeients, or coefficeients that are controlled from the workflow. c(al,be,b0)
#'                    \item time - data.frame with columns date, al, be, b0. values with date nearest to mean(data$date) will be used
#'                    \item wind - data.frame with columns PSI_uv, al, be, b0. values with PSI_uv nearest to average PSI_uv will be used
#'                    }
#' @param plnrFitType type of planar fit, "simple", "date" or "wind". [character vector]
#' @return Data object with rotated wind vectors added
#' 
#' @references Code adapted from REYNFlux_P5 - Stefan Metzger / Cove Sturtevant / Ke Xu - as of commit 35ceda9
#' 
#' @author W. S. Drysdale
#' 
#' @export

wrap.rot = function(data,
                    MethRot = c("single","double","planarFit","none")[1],
                    plnrFitCoef = NULL,
                    plnrFitType = c("simple","time","wind")[1]){
  
  # rotation angle
  mnPSI_uv = eddy4R.base::def.pol.cart(matrix(c(mean(data$v_met, na.rm = TRUE),
                                                mean(data$u_met, na.rm = TRUE)),
                                              ncol=2))
  
  if(MethRot %in% c("single","double")){
    rotang <- (eddy4R.base::def.unit.conv(data=(mnPSI_uv+180),unitFrom="deg",unitTo="rad")) %% (2*pi)
    
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
    
    if(MethRot == "double"){
      #second rotation
      rotang_v = atan2(mean(Urot[3,],na.rm = TRUE),mean(Urot[1,],na.r = TRUE))
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
    
    data$u_hor <- Urot[1,]
    data$v_hor <- -Urot[2,]
    data$w_hor <- Urot[3,]
    
  }
  
  if(MethRot == "none"){
    data$u_hor = data$u_met
    data$v_hor = data$v_met
    data$w_hor = data$w_met
  }
  
  if(MethRot == "planarFit"){
    
    if(is.null(plnrFitCoef)){
      stop("plnrFitCoef is NULL")
    }
    
    data$v_met = -data$v_met # reverse the v wind vector as PFIT_apply() expects +ve left,front,below.
    # workflow input should be +ve left,behind,below
    
    if(plnrFitType == "simple"){
      
      # Expect a vector for plnrFitCoef
      if(class(plnrFitCoef) != "numeric")
        stop("When plnrFitType == simple, plnrFitCoef must be a numeric vector")
      
      # Apply planar fit
      plnrFitData = PFIT_apply(
        u_m = data.frame(
          xaxs = data$v_met,
          yaxs = data$u_met,
          zaxs = data$w_met
        ),
        al = plnrFitCoef[1],
        be = plnrFitCoef[2],
        b0 = plnrFitCoef[3]
      )
    }
    
    if(plnrFitType %in% c("time","wind")){
      if(class(plnrFitCoef) != "data.frame")
        stop("When plnrFitType == time or wind, plnrFitCoef must be a data.frame")
      
      # Filter plnrFitCoef
      if(plnrFitType == "time")
        plnrFitCoef = plnrFitCoef[which.min(plnrFitCoef$date-mean(data$date,na.rm = TRUE)),] # for time, the nearest plnrFitCoef to the mean date is selected
      if(plnrFitType == "wind"){
        min_dir = which.min(abs(plnrFitCoef$PSI_uv-mnPSI_uv))
        plnrFitCoef = plnrFitCoef[min_dir,]
      } # for wind, the nearest plnrFitCoef to the mean PSI_uv is selected
      
      # Apply planar fit
      plnrFitData = PFIT_apply(
        u_m = data.frame(
          xaxs = data$u_met,
          yaxs = data$v_met,
          zaxs = data$w_met
        ),
        al = plnrFitCoef$al,
        be = plnrFitCoef$be,
        b0 = plnrFitCoef$b0
      )
      
    }
    
    # reassign vectors
    data$u_hor = plnrFitData$xaxs
    data$v_hor = -plnrFitData$yaxs # negative as inputs were reversed earlier
    data$w_hor = plnrFitData$zaxs
  }
  
  # Return
  data
  
}
