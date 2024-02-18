##############################################################################################
#' @title Definition function: Quality Metrics 

#' @author
#' Cove Sturtevant \email{eddy4R.info@gmail.com}\cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description 
#' Function definition. Determine the quality metrics of failed, pass, and NA for each of the individual quality flag following the method described in Smith et.al. (2014). Performed for the entire set of input data.

#' @param qf A data frame of quality flags, class integer. Each column contains the quality flag values [-1,0,1] for that flag. Note: This is the Vrbs output from def.plau, def.dspk.wndw, and def.dspk.br86. See def.conv.qf.vrbs for converting from non-verbose to verbose output.
#' @param nameQmOut Optional. A vector of class "character" containing the base name of the output quality metrics for each flag in \code{qf}. These names will be ammended with "Pass", "Fail", or "Na" at the end when outputting their respective quality metrics. Default behavior is to auto assign names based on the column names of \code{qf} [-] 

#' @return A dataframe containing quality metrics (fractions) of failed, pass, and NA for each of the individual flag defined in \code{qf}.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113) \cr
#' Smith, D.E., Metzger, S., and Taylor, J.R.: A transparent and transferable framework for tracking quality information in large datasets. PLoS ONE, 9(11), e112249.doi:10.1371/journal.pone.0112249, 2014. \cr 

#' @keywords NEON QAQC, quality flags, quality metrics

#' @examples
#' qfA <- c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,1) 
#' qfB <- c(0,0,-1,-1,-1,1,1,0,0,0,0,0,0,0,0)
#' qfC <- c(0,1,1,0,0,0,0,0,0,0,0,0,-1,-1,-1)
#' test<-list()
#' test$qf <- data.frame(qfA,qfB,qfC)
#' test$qm<- def.qm(qf=test$qf, nameQmOut=c("qmA","qmB","qmC"))

#' @seealso 
#' \code{\link[eddy4R.qaqc]{wrap.qfqm.dp01}}
#' \code{\link[eddy4R.qaqc]{def.plau}}
#' \code{\link[eddy4R.qaqc]{def.dspk.wndw}}
#' \code{\link[eddy4R.qaqc]{def.dspk.br86}}
#' \code{\link[eddy4R.qaqc]{def.conv.qf.vrbs}}

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-01-05)
#     original creation
#   Natchaya P-Durden (2016-08-15)
#     modified wrap.dp01.qfqm()
#   Cove Sturtevant (2016-11-22)
#     added default functionality for naming of output quality metrics
#     adjusted output of quality metrics to fractions (previously percentage)
#   Natchaya P-Durden (2018-04-04)
#    applied eddy4R term name convention; replaced posQf by setQf
#   David Durden (2022-09-08)
# Remove flags that are filled with NAN (represents flags that are not expected for a sensor [i.e. soni diagnostics for soni3B])
##############################################################################################
def.qm <- function (
  qf, # A data frame of quality flags, class integer. Each column contains the quality flag values [-1,0,1] for that flag. Note: This is the Vrbs output from def.plau, def.dspk.wndw, and def.dspk.br86
  nameQmOut=NULL
  ) {
  
  # Error Checking --------------------------------------------------
  
  # Check qf
  if(!base::is.data.frame(qf)) {
    base::stop("Input qf must be a data frame. See documentation.")
  }
  
  # Save columns where all values are NAN (represents flags that are not expected for a sensor [i.e. soni diagnostics for soni3B])
  qmRmv <- names(dplyr::select(qf, where(~base::all(base::is.nan(.)))))
  # Remove columns where all values are NAN (represents flags that are not expected for a sensor [i.e. soni diagnostics for soni3B])
  qf <- dplyr::select(qf, where(~!base::all(base::is.nan(.))))
  
  if(base::sum(!(base::as.matrix(qf) %in% c(-1,0,1))) != 0){
    stop("Values of qf must be equal to -1, 0, or 1")
  }
  
  # Initialize Outputs --------------------------------------------------
  
  # Take inventory of the flags we have and set up output naming
  numQf <- base::length(qf)
  nameQf <- base::names(qf)  
  flagNameOut <- FALSE # Initialize use of default naming
  if(base::is.null(nameQmOut)){
    flagNameOut <- TRUE # Do default naming
    nameQmOut <- nameQf
  }
  nameQm <- base::c("Pass","Fail","Na") # 3 subvariables per quality metric
  nameOut <- base::c()
  
  # Put together output variable names
  for (idxQf in base::numeric(numQf)+1:numQf) {
    
    tmp <- nameQmOut[idxQf]
    
    # Get rid of "qf" or "setQf" (if using default naming)
    if (flagNameOut && (base::regexpr(pattern="qf",text=tmp,ignore.case=FALSE)[1] == 1 || 
                        base::regexpr(pattern="setQf",text=tmp,ignore.case=FALSE)[1] == 1)) {
      tmp <- base::sub(pattern="qf", replacement="", x=tmp, ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE)
      
      tmp <- base::sub(pattern="setQf", replacement="", x=tmp, ignore.case = FALSE, perl = FALSE,
                 fixed = FALSE, useBytes = FALSE)
      
      tmp <- base::paste0("qm",tools::toTitleCase(tmp),collapse="")
    } 
    
    for (idxQm in 1:3) {
      nameOut[(idxQf-1)*3+idxQm] <- base::paste0(tmp,nameQm[idxQm],collapse ="")
    }
  }
  
  qm <- base::data.frame(base::matrix(,ncol=3*numQf))#initial output
  base::colnames(qm) <- nameOut
  
  # Compute Quality metric -------------------------------------------------
  # Loop through each flag
  for(idxQf in base::numeric(numQf)+1:numQf) {

    # Quality metrics for pass, failed, and NA flags
    # Pass
    qm[,(idxQf-1)*3+1]  <- base::sum(qf[,idxQf]== 0, na.rm = TRUE)/base::nrow(qf)
    # Fail
    qm[,(idxQf-1)*3+2] <- base::sum(qf[,idxQf]== 1, na.rm = TRUE)/base::nrow(qf)
    # NA
    qm[,(idxQf-1)*3+3] <- base::sum(qf[,idxQf]== -1, na.rm = TRUE)/base::nrow(qf) 
    
  }
  
  
  #Create NaN flags names
  qmRmv <- base::gsub("qf", "qm", qmRmv)
  qmRmvOut <-  sapply(qmRmv, function(x) {base::paste0(x,nameQm)})
  #Add NaN flags back to output to allow continuous data series
  qm[,qmRmvOut] <- NaN
  
  return(qm)
}
