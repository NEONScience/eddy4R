##############################################################################################
#' @title Definition function: Threshold IRGA validation data based on benchmarking regression

#' @author
#' Chris Florian \email{eddy4R.info@gmail.com}

#' @description Wrapper function to apply IRGA validation.

#' @param data List of validation data as a report from eddy4R.base::wrap.irga.vali()
#' @param gasRefe List containing the values of the reference gases. [mol mol-1]
#' @param DateProc A vector of class "character" containing the processing date.
#' @param evalSlpMax Maximum acceptable slope of the benchmarking regression
#' @param evalSlpMin Minimum acceptable slope of the benchmarking regression
#' @param evalOfstMax Maximum acceptable offset of the benchmarking regression
#' @param evalOfstMin Minimum acceptable offset of the benchmarking regression


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
#   Chris Florian (2021-08-18)
#     updating terms to replace bnch with eval, adding additional coef outputs and adding offset criteria
##############################################################################################

def.irga.vali.thsh <- function(
  data,
  DateProc,
  evalSlpMax,
  evalSlpMin,
  evalOfstMax,
  evalOfstMin) {
  
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
  
  meanZeroCor <- zeroMeas*data$rtioMoleDryCo2Mlf$coef[2] + data$rtioMoleDryCo2Mlf$coef[1]
  meanLowCor <- lowMeas*data$rtioMoleDryCo2Mlf$coef[2] + data$rtioMoleDryCo2Mlf$coef[1]
  meanMidCor <- midMeas*data$rtioMoleDryCo2Mlf$coef[2] + data$rtioMoleDryCo2Mlf$coef[1]
  meanHighCor <- highMeas*data$rtioMoleDryCo2Mlf$coef[2] + data$rtioMoleDryCo2Mlf$coef[1]
  
  meanCor <- c(meanZeroCor, meanLowCor, meanMidCor, meanHighCor)
  
  
  #run benchmarking least squares regression on corrected mean values from the reference gasses vs. the reference values
  #adding logic to avoid an error when one of the lists passed into lm() is entirely NA
  
  if(all(!is.na(refeVals)) == TRUE & all(!is.na(meanCor)) == TRUE){
  valiEval <- stats::lm(meanCor ~ refeVals)
  valiEvalSe <- sqrt(diag(vcov(valiEval)))
  valiEvalSlp <- valiEval$coefficient[[2]]
  valiEvalOfst <- valiEval$coefficient[[1]]
  } else {
    valiEval <- NA
    valiEvalSe <- NA
    valiEvalSlp <- NA
    valiEvalOfst <- NA
  }
  
  #determine if slope passes
  
  evalSlpPass <- valiEvalSlp >= evalSlpMin & valiEvalSlp <= evalSlpMax
  
  #determine if offset passes
  
  evalOfstPass <- valiEvalOfst >= evalOfstMin & valiEvalOfst <= evalOfstMax
  
  #set valiPass flag, 0 for good validation, 1 for bad, -1 for missing values 
  if(!is.na(valiEvalSlp)){
    if (evalSlpPass == TRUE & evalOfstPass == TRUE){
      valiEvalPass <- TRUE
    } else {
      valiEvalPass <- FALSE
    }
  } else {
    valiEvalPass <- -1
  }
    
  #compile report including validation pass status and the corrected reference files to add to the vali table
  rpt <- list()
  rpt$valiEvalPass <- valiEvalPass
  rpt$meanCor <- meanCor
  rpt$evalCoef <- c(valiEvalOfst, valiEvalSlp)
  rpt$evalCoefSe <- valiEvalSe
  rpt$evalSlpThsh <- c(evalSlpMin, evalSlpMax)
  rpt$evalOfstThsh <- c(evalOfstMin, evalOfstMax)
  
  return(rpt)
}
