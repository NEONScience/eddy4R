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
#   Chris Florian (2021-02-15)
#     updating logic for failsafe to prevent lm() error due to missing values
#   Adam Young (2023-08-18)
#     - Removed hardcoding when indexing measured and reference data values to avoid unintentionally selecting wrong
#       reference values. 
#     - Altered coding for logic statement so that if there are at least 
#       two complete set of observations (n >= 2) in data matrix used for linear model. 
#     - Simplified correction calculations.
##############################################################################################

def.irga.vali.thsh <- function(
  data,
  DateProc,
  evalSlpMax,
  evalSlpMin,
  evalOfstMax,
  evalOfstMin) {
  
  #get reference gas values for the processing date (in mol mol-1)
  
  zeroRefe <- data$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[data$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas02"]
  lowRefe  <- data$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[data$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas03"]
  midRefe  <- data$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[data$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas04"]
  highRefe <- data$rtioMoleDryCo2Vali$rtioMoleDryCo2Refe[data$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas05"]
  
  refeVals <- c(zeroRefe, lowRefe, midRefe, highRefe)
  
  #get the mean measured values of the reference gas for the processing date
  zeroMeas <- data$rtioMoleDryCo2Vali$mean[data$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas02"]
  lowMeas <- data$rtioMoleDryCo2Vali$mean[data$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas03"]
  midMeas <- data$rtioMoleDryCo2Vali$mean[data$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas04"]
  highMeas <- data$rtioMoleDryCo2Vali$mean[data$rtioMoleDryCo2Vali$gasType == "qfIrgaTurbValiGas05"]
  
  #correct measured LI7200 validation gas data based on calibration coefficients 
  
  ofst<- data$rtioMoleDryCo2Mlf$coef[1]
  slp <- data$rtioMoleDryCo2Mlf$coef[2]
  
  meanZeroCor <- slp * zeroMeas + ofst
  meanLowCor  <- slp * lowMeas  + ofst
  meanMidCor  <- slp * midMeas  + ofst
  meanHighCor <- slp * highMeas + ofst
  
  meanCor <- c(meanZeroCor, meanLowCor, meanMidCor, meanHighCor)
  
  
  #run benchmarking least squares regression on corrected mean values from the reference gasses vs. the reference values
  #adding logic to avoid an error when one of the lists passed into lm() is entirely NA
  
  # Simple data frame to feed into 'data' argument in lm() function
  modlMtrx <- data.frame(refeVals = refeVals, meanCor = meanCor)
  modlMtrx <- na.omit(modlMtrx)
  
  if (nrow(modlMtrx) >= 2) {
    
    valiEval <- stats::lm(meanCor ~ refeVals, data = modlMtrx)
    valiEvalSe <- sqrt(diag(vcov(valiEval)))
    valiEvalSlp <- valiEval$coefficient[[2]]
    valiEvalOfst <- valiEval$coefficient[[1]]
    
  } else {
    
    msg <- paste0("valiEval coefficients set to NA beacuse of insufficent refe or measured values")
    tryCatch({rlog$debug(msg)}, error=function(cond){print(msg)})
    valiEval <- NA
    valiEvalSe <- NA
    valiEvalSlp <- NA
    valiEvalOfst <- NA
    
  }
  
  # Old logic not used anymore 
  # -----
  # if(sum(!is.na(refeVals)) > 1 & sum(!is.na(meanCor)) > 0){ #lm() will fail if one list is entirely NA, or if both lists have only one value.
  # 
  # } else {
  # 
  # }
  # -----
  
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
