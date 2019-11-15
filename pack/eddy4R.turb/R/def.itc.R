##############################################################################################
#' @title Definition function: Integral turbulence characteristics

#' @author
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \cr

#' @description 
#' Function defintion. Integral turbulence characteristics.

#' @param \code{stblObkv} Stability parameter and of class "numeric". [dimensionless]
#' @param \code{lat} Latitude and of class "numeric". [degrees North]
#' @param \code{VarInp} A vector of class "character" containing the name of variables to be performed integral turbulence characteristics test. \code{VarInp} = c("veloXaxs","veloZaxs","temp","all"), where "veloXaxs" is along-axis horizontal wind speed, "veloZaxs" is vertical-axis wind speed, "temp" is air temperature, and "all" is all three variables. Defaults to "all".[-]
#' @param \code{sd} A vector or data frame containing standard deviation of \code{VarInp} and of class "numeric". If \code{VarInp} = "all",  \code{sd} shall contain in the follwing orders, standard deviation of along-axis horizontal wind speed, standard deviation of vertical-axis wind speed, and standard deviation of air temperature. [user-defined]
#' @param \code{varScal} A vector or data frame containing the scaling variables of \code{VarInp} and of class "numeric". If \code{VarInp} = "all", \code{varScal} shall contain in the follwing orders, scaling variable of wind speed (friction velocity will be used for both "veloXaxs" and "veloZaxs") and scaling variable of air temperature.  [user-defined]
#' @param \code{Thsh} Threshold value to indicate if quality flag will raise high (qfItc = 1) when the calculated quality indicator values are greater the threshold. Defaults as 100. [percent]
#' @param \code{CorTemp}  Logical state if the temperture is used in the determination of ITCs of sensible heat flux (fluxSens). Defaults as TRUE.
#' 
#' @return 
#' The returned object consists of:\cr
#' \code{qiItc} integral turbulence characteristics test results (quality indicator) of \code{VarInp}. If \code{VarInp} = "all", it includes test results for individual variable, i.e., along-axis horizontal wind speed, vertical-axis wind speed, and air temperature and also combined variables, i.e., friction velocity and sensible heat flux. [percent]\cr
#' \code{qfItc} integral turbulence characteristics quality flag of of \code{VarInp}. If \code{VarInp} = "all", it includes quality flag results for individual variable, i.e., along-axis horizontal wind speed, vertical-axis wind speed, and air temperature and also combined variables, i.e., friction velocity and sensible heat flux. [NA]

#' @references 
#' Thomas, C. and Foken, T: Re-evaluation of integral turbulence characteristics and their parameterisations, 15th Conference on Turbulence and Boundary Layers, Wageningen, NL, American Meteorological Society, pp. 129-132, 2002.

#' @keywords eddy-covariance, integral turbulence characteristics, turbulent flux

#' @examples 
#' #input
#'sd <- data.frame(veloXaxs=0.5254, veloZaxs=0.2570, temp=0.1405)
#'varScal <- data.frame(veloFric=0.1918, tempScal=0.0268)
#'#call function
#'def.itc(stblObkv=0.0255, lat=70.4214, VarInp=c("all"), sd=sd, varScal=varScal)
#'#single entry
#'def.itc(stblObkv=0.0255, lat=70.4214, VarInp=c("veloXaxs"), sd=0.5254, varScal=0.1918)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-03-18)
#     original creation
#   Stefan Metzger (2015-11-29)
#     added Roxygen2 tags
#   Natchaya P-Durden (2016-07-22)
#     initail naming convention for eddy4R
#     modified function by adding the input variable options
#   Natchaya P-Durden (2019-09-09)
#     changed outputs format to list
#     added calculation of quality flags
#   Natchaya P-Durden (2019-09-27)
#     add threshold value in the parameter
#     update the final criteria for combined variables (u_star and sensible heat flux) 
#     to use the maximum value
#   Natchaya P-Durden (2019-10-25)
#     add CorTemp in function parameters
#   Natchaya P-Durden (2019-11-07)
#     update function to handle when stblObkv is NA
#   Natchaya Pingintha-Durden (2019-11-15)
#     add Perc to input parameter;
#     update output of qi when Perc equal TRUE or FALSE
#     update qf algorithm to be 0 when qi is NaN
##############################################################################################
#INTEGRAL TURBULENCE CHARACTERISTICS
def.itc <- function(
  stblObkv,
  lat,
  VarInp=c("veloXaxs","veloZaxs","temp","all")[4],
  sd,
  varScal,
  Thsh = 100,
  Perc = TRUE,
  CorTemp = TRUE
) {
  #initial list
  rpt <- list()
  #constants for stability parameter
  CnstLocStblObkv <- list(Cnst01 = -0.032, Cnst02 = -1, Cnst03 = -0.062, Cnst04 = 0.02, Cnst05 = -0.2, Cnst06 = 0.4)
  
  #coriolis parameter
  coefCorl <- eddy4R.base::def.coef.corl(lat)

  #Calculate MODEL ITCS for along-axis horizontal wind speed
  if(VarInp %in% c("veloXaxs", "all")) {
    if (is.na(stblObkv)){
      veloXaxs <- NaN
    } else {
      #constants for along-axis horizontal wind speed
      CnstLocVeloXaxs <- list(Cnst01 = 4.15, Cnst02 = 1/8, Cnst03 = 2.7, Cnst04 = 0, Cnst05 = 0.44, Cnst06 = 6.3)

      #assign model coefficients
      CoefVeloXaxs <- if(stblObkv < CnstLocStblObkv$Cnst01) c(CnstLocVeloXaxs$Cnst01, CnstLocVeloXaxs$Cnst02) else c(CnstLocVeloXaxs$Cnst03, CnstLocVeloXaxs$Cnst04)
      
      #calculate model; the absolute value of stability is used, since negative values often result in NAs of the potency
      itcVeloXaxsModl <- CoefVeloXaxs[1] * base::abs(stblObkv)^CoefVeloXaxs[2]
      
      #UPDATED MODEL (THOMAS AND FOKEN, 2002)
      if(stblObkv > CnstLocStblObkv$Cnst05 & stblObkv < CnstLocStblObkv$Cnst06) {
        itcVeloXaxsModl <- CnstLocVeloXaxs$Cnst05 * base::log(coefCorl / varScal[[1]]) + CnstLocVeloXaxs$Cnst06
      }
      
      #calculate observed ITCs for along-axis horizontal wind speed
      #stdevU / u_star
      if(VarInp %in% c("veloXaxs")) {
        itcVeloXaxsMeas <- sd[[1]] / varScal[[1]]
      }
      if(VarInp %in% c("all")) {
        itcVeloXaxsMeas <- sd[[1]] / varScal[[1]]
      }
      
      #final criteria [%]
      veloXaxs <- (base::abs(itcVeloXaxsMeas - itcVeloXaxsModl) / itcVeloXaxsModl) * 100
    } #end else
    
    #calculate the flag; pass (0) if  <= 100% or NaN; failed (1) >100%
    qfVeloXaxs <- as.integer(ifelse(is.na(veloXaxs) | veloXaxs <= Thsh, 0, 1))
  }
  
  #Calculate MODEL ITCS for vertical-axis wind speed
  if(VarInp %in% c("veloZaxs", "all")) {
    if (is.na(stblObkv)){
      veloZaxs <- NaN
    } else {
      #constants for vertical-axis wind speed
      CnstLocVeloZaxs <- list(Cnst01 = 2.0, Cnst02 = 1/8, Cnst03 = 1.3, Cnst04 = 0, Cnst05 = 0.21, Cnst06 = 3.1)
      
      #assign model coefficients
      CoefVeloZaxs <- if(stblObkv < CnstLocStblObkv$Cnst01) c(CnstLocVeloZaxs$Cnst01, CnstLocVeloZaxs$Cnst02) else c(CnstLocVeloZaxs$Cnst03, CnstLocVeloZaxs$Cnst04)
      
      #calculate model; the absolute value of stability is used, since negative values often result in NAs of the potency
      itcVeloZaxsModl <- CoefVeloZaxs[1] * base::abs(stblObkv)^CoefVeloZaxs[2]
      
      #UPDATED MODEL (THOMAS AND FOKEN, 2002)
      if(stblObkv > CnstLocStblObkv$Cnst05 & stblObkv < CnstLocStblObkv$Cnst06) {
        itcVeloZaxsModl <- CnstLocVeloZaxs$Cnst05 * base::log(coefCorl / varScal[[1]]) + CnstLocVeloZaxs$Cnst06
      }
      
      #calculate observed ITCs for vertical-axis wind speed
      #stdevW / u_star
      if (VarInp %in% c("veloZaxs")) {
        itcVeloZaxsMeas <- sd[[1]] / varScal[[1]]
      }
      if (VarInp %in% c("all")) {
        itcVeloZaxsMeas <- sd[[2]] / varScal[[1]]
      }
      
      #final criteria [%]
      veloZaxs <- (base::abs(itcVeloZaxsMeas - itcVeloZaxsModl) / itcVeloZaxsModl) * 100
    } #end else
    #calculate the flag; pass (0) if  <= 100% or NaN; failed (1) >100%
    qfVeloZaxs <- as.integer(ifelse(is.na(veloZaxs) | veloZaxs <= Thsh, 0, 1))
  }

  #Calculate MODEL ITCS for temperature
  if(VarInp %in% c("temp", "all")) {
    if (is.na(stblObkv)){
      temp <- NaN
    } else {
      #constants for temperature
      CnstLocTemp <- list(Cnst01 = 1, Cnst02 = -1/3, Cnst03 = 1, Cnst04 = -1/4, Cnst05 = 0.5, Cnst06 = -1/2, Cnst07 = 1.4, Cnst08 = -1/4)
      
      #assign model coefficients
      CoefTemp <-  if(stblObkv < CnstLocStblObkv$Cnst02) c(CnstLocTemp$Cnst01, CnstLocTemp$Cnst02) else
        if(stblObkv >= CnstLocStblObkv$Cnst02 & stblObkv < CnstLocStblObkv$Cnst03) c(CnstLocTemp$Cnst03, CnstLocTemp$Cnst04) else
          if(stblObkv >= CnstLocStblObkv$Cnst03 & stblObkv < CnstLocStblObkv$Cnst04) c(CnstLocTemp$Cnst05, CnstLocTemp$Cnst06) else
            if(stblObkv >= CnstLocStblObkv$Cnst04) c(CnstLocTemp$Cnst07,  CnstLocTemp$Cnst08)
      
      #calculate model; the absolute value of stability is used, since negative values often result in NAs of the potency
      itcTempModl <- CoefTemp[1] * base::abs(stblObkv)^CoefTemp[2]
      
      #calculate observed ITCs for vertical-axis wind speed
      #stdevT / T_star_SL
      if (VarInp %in% c("temp")) {
        itcTempMeas <- sd[[1]] / varScal[[1]]
      }
      if (VarInp %in% c("all")) {
        itcTempMeas <- sd[[3]] / varScal[[2]]
      }
      
      #final criteria [%]
      temp <- (base::abs(itcTempMeas - itcTempModl) / itcTempModl) * 100
    }#end else
    #calculate the flag; pass (0) if  <= 100% or NaN; failed (1) >100%
    qfTemp <- as.integer(ifelse(is.na(temp) | temp <= Thsh, 0, 1))
  }

  ##final criteria [%] for combined variables: u_star and sensible heat flux
  if(VarInp %in% c("all")) {
    veloFric <- base::max(veloXaxs, veloZaxs)
    #calculate the flag; pass (0) if  =< 100%; failed (1) >100%
    qfVeloFric <- as.integer(ifelse(is.na(veloFric) | veloFric <= Thsh, 0, 1))
    if(CorTemp == TRUE){
      fluxSens <- base::max(veloZaxs, temp)
      qfFluxSens <- as.integer(ifelse(is.na(fluxSens) | fluxSens <= Thsh, 0, 1))
      }else{
        #not included temperature in the determination of itcs
        fluxSens <- veloZaxs
        qfFluxSens <- qfVeloZaxs
        }
  }
#clean up
#rm(CoefVeloXaxs, CoefVeloZaxs, CoefTemp, coefCorl)
rm(coefCorl)

#-----------------------------------------------------------
#output dataframe
#if Prec == FALSE convert unit to fraction by deviding 100
if (Perc == TRUE) {Frac <- 1 } else { Frac <- 100}
  if(VarInp == "veloXaxs"){
    #itc <- data.frame(veloXaxs=veloXaxs)
    rpt$qiItc <- data.frame(u_hor=veloXaxs/Frac) #this line will be replaced by above line once we apply name convention to the reference file header 
    rpt$qfItc <- data.frame(u_hor=qfVeloXaxs)
  }
  if(VarInp == "veloZaxs"){
    #itc <- data.frame(veloZaxs=veloZaxs)
    rpt$qiItc <- data.frame(w_hor=veloZaxs/Frac) #this line will be replaced by above line once we apply name convention to the reference file header
    rpt$qfItc <- data.frame(w_hor=qfVeloZaxs)
  }
  if(VarInp == "temp"){
    #itc <- data.frame(temp=temp)
    rpt$qiItc <- data.frame(T_air=temp/Frac)
    rpt$qfItc <- data.frame(T_air=qfTemp)
  }
  if(VarInp == "all"){
    #itc <- data.frame(veloXaxs=veloXaxs, veloZaxs=veloZaxs, temp=temp, veloFric=veloFric, fluxSens=fluxSens)
    rpt$qiItc <- data.frame(u_hor=veloXaxs/Frac, w_hor=veloZaxs/Frac, T_air=temp/Frac, u_star=veloFric/Frac, F_H_kin=fluxSens/Frac) #this line will be replaced by above line once we apply name convention to the reference file header
    rpt$qfItc <- data.frame(u_hor=qfVeloXaxs, w_hor=qfVeloZaxs, T_air=qfTemp, u_star=qfVeloFric, F_H_kin=qfFluxSens)
  }
#
#clean up
#rm(itcVeloXaxsMeas, itcVeloXaxsModl, itcVeloZaxsMeas, itcVeloZaxsModl, itcTempMeas, itcTempModl)
#-----------------------------------------------------------
#EXPORT RESULTS
return(rpt)
}
