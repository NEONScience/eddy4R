##############################################################################################
#' @title Definition function: Threshold IRGA validation data based on benchmarking regression

#' @author
#' Chris Florian \email{eddy4R.info@gmail.com}

#' @description Wrapper function to apply IRGA validation.

#' @param data List of validation data as a report from eddy4R.base::wrap.irga.vali()
#' @param gasRefe List containing the values of the reference gases. [mol mol-1]
#' @param DateProc A vector of class "character" containing the processing date.
#' @param bnchSlpMax Maximum acceptable slope of the benchmarking regression
#' @param bnchSlpMin Minimum acceptable slope of the benchmarking regression


#' @return pass/fail criteria for a day's validation 


#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords derived, irgaTurb, post-processing, pre-processing, validation

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Chris Florian (2021-08-03)
#     original creation   
#   Chris Florian (2021-08-09)
#     adding logic to prevent errors if data are missing
##############################################################################################

def.irga.vali.thsh <- function(
  data,
  DateProc,
  bnchSlpMax,
  bnchSlpMin
) {
  
  #get reference gas values for the processing date (in mol mol-1)
  
  zeroRefe <- data$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[2]
  lowRefe <- data$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[3]
  midRefe <- data$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[4]
  highRefe <- data$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[5]
  
  refeVals <- c(zeroRefe, lowRefe, midRefe, highRefe)
  
  #get the mean measured values of the reference gas for the processing date
  zeroMeas <- data$rtioMoleDryCo2Vali$mean[2]
  lowMeas <- data$rtioMoleDryCo2Vali$mean[3]
  midMeas <- data$rtioMoleDryCo2Vali$mean[4]
  highMeas <- data$rtioMoleDryCo2Vali$mean[5]
  
  #correct measured LI7200 validation gas data based on calibration coefficients 
  
  measZeroCor <- zeroMeas*data$rtioMoleDryCo2Mlf$coef[2] + data$rtioMoleDryCo2Mlf$coef[1]
  measLowCor <- lowMeas*data$rtioMoleDryCo2Mlf$coef[2] + data$rtioMoleDryCo2Mlf$coef[1]
  measMidCor <- midMeas*data$rtioMoleDryCo2Mlf$coef[2] + data$rtioMoleDryCo2Mlf$coef[1]
  measHighCor <- highMeas*data$rtioMoleDryCo2Mlf$coef[2] + data$rtioMoleDryCo2Mlf$coef[1]
  
  rtioMoleDryCo2RefeCor <- c(measZeroCor, measLowCor, measMidCor, measHighCor)
  
  
  #run benchmarking least squares regression on corrected mean values from the reference gasses vs. the reference values
  #adding logic to avoid an error when one of the lists passed into lm() is entirely NA
  
  if(all(!is.na(refeVals)) == TRUE & all(!is.na(rtioMoleDryCo2RefeCor)) == TRUE){
  valiLmSlp <- stats::lm(rtioMoleDryCo2RefeCor ~ refeVals)$coefficient[[2]]
  } else {
    valiLmSlp <- NA
  }
  
  #set valiPass flag, 0 for good validation, 1 for bad, -1 for missing values 
  if(!is.na(valiLmSlp)){
    if (valiLmSlp >= bnchSlpMin & valiLmSlp <= bnchSlpMax){
      valiPass <- TRUE
    } else {
      valiPass <- FALSE
    }
  } else {
    valiPass <- -1
  }
    
  #compile report including validation pass status and the corrected reference files to add to the vali table
  rpt <- list()
  rpt$valiPass <- valiPass
  rpt$rtioMoleDryCo2RefeCor <- rtioMoleDryCo2RefeCor
  
  return(rpt)
}
