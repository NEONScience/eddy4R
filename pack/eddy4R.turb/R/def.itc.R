##############################################################################################
#' @title Integral turbulence characteristics

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden

#' @description 
#' Function defintion. Integral turbulence characteristics.

#' @param \code{stblObkv} Stability parameter and of class "numeric". [dimensionless]
#' @param \code{lat} Latitude and of class "numeric". [degrees North]
#' @param \code{sd} A data frame containing standard deviation of the interested variables, i.e., along-axis horizontal wind speed, vertical-axis wind speed, and air temperature. Of class "numeric". [user-defined]
#' @param \code{varScal} A data frame containing the scaling variables of the interested variables, i.e., scaling variables of wind speed and air temperature. Of class "numeric". [user-defined]

#' @return Data frame of integral turbulence characteristics test results for interested variables. For eddy4R use case, it includes test results for individual variable, i.e., along-axis horizontal wind speed, vertical-axis wind speed, and air temperature and for combined variables, i.e., friction velocity and sensible heat flux. [percent]

#' @references Thomas, C. and Foken, T: Re-evaluation of integral turbulence characteristics and their parameterisations, 15th Conference on Turbulence and Boundary Layers, Wageningen, NL, American Meteorological Society, pp. 129-132, 2002.

#' @keywords eddy-covariance, integral turbulence characteristics, turbulent flux

#' @examples 
#' #input
#' sd <- data.frame(u_hor= 0.5254, w_hor= 0.2570, T_air= 0.1405)
#' varScal <- data.frame(u_star=0.1918, T_star_SL= 0.0268)
#' #call function
#' def.itc(stblObkv=0.0255, lat=70.4214, sd=sd, varScal=varScal)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Natchaya P-Durden (2016-07-22)
#     Initail naming convention for eddy4R
##############################################################################################
#INTEGRAL TURBULENCE CHARACTERISTICS

def.itc <- function(
  stblObkv,
  lat,
  sd,
  varScal
) {
  #define local constants
  #constants for stability parameter
  CnstLocStblObkv <- list(Cnst01 = -0.032, Cnst02 = -1, Cnst03 = -0.062, Cnst04 = 0.02, Cnst05 = -0.2, Cnst06 = 0.4)
  #constants for along-axis horizontal wind speed
  CnstLocVeloXaxs <- list(Cnst01 = 4.15, Cnst02 = 1/8, Cnst03 = 2.7, Cnst04 = 0, Cnst05 = 0.44, Cnst06 = 6.3)
  #constants for vertical-axis wind speed
  CnstLocVeloZaxs <- list(Cnst01 = 2.0, Cnst02 = 1/8, Cnst03 = 1.3, Cnst04 = 0, Cnst05 = 0.21, Cnst06 = 3.1)
  #constants for temperature
  CnstLocTemp <- list(Cnst01 = 1, Cnst02 = -1/3, Cnst03 = 1, Cnst04 = -1/4, Cnst05 = 0.5, Cnst06 = -1/2, Cnst07 = 1.4, Cnst08 = -1/4)
    
  #-----------------------------------------------------------
  #MODELLED ITCS
  
  #STANDARD MODEL
  #assign model coefficients
  #stdevU / u_star
  CoefVeloXaxs <- if(stblObkv < CnstLocStblObkv$Cnst01) c(CnstLocVeloXaxs$Cnst01, CnstLocVeloXaxs$Cnst02) else c(CnstLocVeloXaxs$Cnst03, CnstLocVeloXaxs$Cnst04)
  #stdevW / u_star
  CoefVeloZaxs <- if(stblObkv < CnstLocStblObkv$Cnst01) c(CnstLocVeloZaxs$Cnst01, CnstLocVeloZaxs$Cnst02) else c(CnstLocVeloZaxs$Cnst03, CnstLocVeloZaxs$Cnst04)
  #stdevT / T_star_SL
  CoefTemp <-  if(stblObkv < CnstLocStblObkv$Cnst02) c(CnstLocTemp$Cnst01, CnstLocTemp$Cnst02) else 
    if(stblObkv >= CnstLocStblObkv$Cnst02 & stblObkv < CnstLocStblObkv$Cnst03) c(CnstLocTemp$Cnst03, CnstLocTemp$Cnst04) else
      if(stblObkv >= CnstLocStblObkv$Cnst03 & stblObkv < CnstLocStblObkv$Cnst04) c(CnstLocTemp$Cnst05, CnstLocTemp$Cnst06) else
        if(stblObkv >= CnstLocStblObkv$Cnst04) c(CnstLocTemp$Cnst07,  CnstLocTemp$Cnst08)
  #calculate models; the absolute value of stability is used, since negative values often result in NAs of the potency
  itcVeloXaxsModl <- CoefVeloXaxs[1] * base::abs(stblObkv)^CoefVeloXaxs[2]
  itcVeloZaxsModl <- CoefVeloZaxs[1] * base::abs(stblObkv)^CoefVeloZaxs[2]
  itcTempModl <- CoefTemp[1] * base::abs(stblObkv)^CoefTemp[2]
  
  #UPDATED MODEL (THOMAS AND FOKEN, 2002)
  #coriolis parameter
  coefCorl <- def.coef.corl(lat)
  #update models
  if(stblObkv > CnstLocStblObkv$Cnst05 & stblObkv < CnstLocStblObkv$Cnst06) {
    itcVeloXaxsModl <- CnstLocVeloXaxs$Cnst05 * base::log(coefCorl / varScal$u_star) + CnstLocVeloXaxs$Cnst06
    itcVeloZaxsModl <- CnstLocVeloZaxs$Cnst05 * base::log(coefCorl / varScal$u_star) + CnstLocVeloZaxs$Cnst06
  }
  #clean up
  rm(CoefVeloXaxs, CoefVeloZaxs, CoefTemp, coefCorl)
  
  #-----------------------------------------------------------
  #COMPARE TO MEASURED ITCS
  
  #calculate observed ITCs
  itcVeloXaxsMeas <- sd$u_hor / varScal$u_star
  itcVeloZaxsMeas <- sd$w_hor / varScal$u_star
  itcTempMeas <- sd$T_air / varScal$T_star_SL
  
  #final criteria [%]
  #individual
  itc <- data.frame(
    u_hor=(base::abs(itcVeloXaxsMeas - itcVeloXaxsModl) / itcVeloXaxsModl) * 100,
    w_hor=(base::abs(itcVeloZaxsMeas - itcVeloZaxsModl) / itcVeloZaxsModl) * 100,
    T_air=(base::abs(itcTempMeas - itcTempModl) / itcTempModl) * 100
  )
  
  #combined
  itc$u_star=base::sqrt(itc$u_hor^2 + itc$w_hor^2)
  itc$F_H_kin=base::sqrt(itc$w_hor^2 + itc$T_air^2)
  
  #clean up
  rm(itcVeloXaxsMeas, itcVeloXaxsModl, itcVeloZaxsMeas, itcVeloZaxsModl, itcTempMeas, itcTempModl)
  
  #-----------------------------------------------------------
  #EXPORT RESULTS
  
  return(itc)
  
}
