##############################################################################################
#' @title Definition function: Final Quality Flag

#' @author
#' Cove Sturtevant \email{eddy4R.info@gmail.com} \cr
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}

#' @description 
#' Function definition. Determine the final quality flag for individual level 1 data product following the method described in Smith et.al. (2014). The alpha and beta quality flags and metrics are also computed in this function. Performed for the entire set of input data.


#' @param qf A data frame of quality flags, class integer, from which to compute the final quality flag. Each column contains the quality flag values [-1,0,1] for that flag. Note: This is the Vrbs output from def.plau, def.dspk.wndw, and def.dspk.br86. See def.conv.qf.vrbs for converting from non-verbose to verbose output.
#' @param WghtAlphBeta A 2-element integer vector of weights to apply to the alpha and beta quality metrics, which will then be summed and evaluated against the threshold (\code{Thsh}) for determing the final quality flag. Default to WghtAlphBeta=c(2,1). [-] 
#' @param Thsh Threshold for determine the condition (pass = 0 or failed = 1) of final quality flag. Default to 0.2 (0.2 percent). [fraction]

#' @return A list of: \cr
#' \code{qaqcRpt} A dataframe containing alpha and beta quality flags at the same frequency as input \code{qf}. [-] \cr
#' \code{qfqm} A dataframe containing alpha and beta quality metric and final qualiy flag. [-] \cr

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113) \cr
#' Smith, D.E., Metzger, S., and Taylor, J.R.: A transparent and transferable framework for tracking quality information in large datasets. PLoS ONE, 9(11), e112249.doi:10.1371/journal.pone.0112249, 2014. \cr 

#' @keywords final quality flag, NEON QAQC, quality flags and metrics

#' @examples
#' qfA <- c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,1) 
#' qfB <- c(0,0,-1,-1,-1,1,1,0,0,0,0,0,0,0,0)
#' qfC <- c(0,1,1,0,0,0,0,0,0,0,0,0,-1,-1,-1)
#' test<-list()
#' test$qf <- data.frame(qfA,qfB,qfC)
#' out <- def.qf.finl(qf=test$qf, WghtAlphBeta=c(2,1), Thsh=0.2)

#' @seealso 
#' \code{\link[eddy4R.qaqc]{wrap.dp01.qfqm}} \cr
#' \code{\link[eddy4R.qaqc]{def.qm}} \cr

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-01-05)
#     original creation of def.qfqm.l1.R 
#   Natchaya P-Durden (2016-09-19)
#     Generated def.qf.finl() from def.qfqm.l1() 
#   Cove Sturtevant (2016-11-28)
#     Added some error checking on inputs
#     Changed units of qms from percent to fraction 
#     Added exclusion of Null test failures from qmBeta (to avoid double-counting)
#   Natchaya P-Durden (2018-04-03)
#     update @param format
#   David Durden (2021-08-18)
#     Removing quality indicators (qi) from qfFinal determination
#   David Durden  (2022-09-08)
# Remove flags that are filled with NAN (represents flags that are not expected for a sensor [i.e. soni diagnostics for soni3B])
##############################################################################################
def.qf.finl <- function (
  qf,
  WghtAlphBeta=c(2,1),
  Thsh=0.2
) {
 
# Error Checking --------------------------------------------------
  
  # Check qf
  if(!base::is.data.frame(qf)) {
    base::stop("Input qf must be a data frame. See documentation.")
  }
  
  # Remove columns where all values are NAN (represents flags that are not expected for a sensor [i.e. soni diagnostics for soni3B])
  qf <- dplyr::select(qf, where(~!base::all(base::is.nan(.))))
  
  if(base::sum(!(base::as.matrix(qf) %in% c(-1,0,1))) != 0){
    stop("Values of qf must be equal to -1, 0, or 1")
  }
  
  if((base::length(WghtAlphBeta) != 2) || !(base::is.numeric(WghtAlphBeta))) {
    stop("Input parameter WghtAlphBeta must be a numeric vector of length 2")
  }
  
  if((base::length(Thsh) != 1) || !(base::is.numeric(Thsh))) {
    stop("Input parameter Thsh must be a numeric vector of length 1")
  }
  
  #Remove quality indicators from being qfFinal determination
  qf <- qf[,grep("^qi", x = names(qf), invert = TRUE)]

  
  
# Compute Quality metric ---------------------------------------------------
  #compute qfAlpha and qfBeta
  qf[,"qfAlph"] <- apply(qf, 1, function(x) ifelse(any(x==1),1,0))
  #calculate qfBeta
  qf[,"qfBeta"] <- apply(qf, 1, function(x) ifelse(any(x==-1),1,0))
  
  # Remove failures of the Null test from qfBeta
  nameQf <- names(qf)
  idxQfNull <- base::grep("null",nameQf,ignore.case=TRUE)
  if (base::length(idxQfNull) != 0)  {
    # We want to exclude beta points attributed to null flag
    qf[qf[,idxQfNull] == 1,"qfBeta"] <- 0
  }
  
  #calculate QMAlpha
  qmAlph <- sum(qf[,"qfAlph"]== 1, na.rm = TRUE)/nrow(qf)
  #calculate QMBeta
  qmBeta <- sum(qf[,"qfBeta"]== 1, na.rm = TRUE)/nrow(qf) 
  
  #calculate QF Final
  if ((WghtAlphBeta[1]*qmAlph) + (WghtAlphBeta[2]*qmBeta) >= Thsh) {
    qfFinl <- 1
  } else {qfFinl <- 0}

#AGGREGATE AND RETURN RESULTS ----------------------------------------------
  
  #aggregate results
  rpt<-list()
  rpt$qaqcRpt <- data.frame(qfAlph=qf[,"qfAlph"], qfBeta=qf[,"qfBeta"])
  rpt$qfqm <- data.frame(qmAlph=qmAlph, qmBeta=qmBeta, qfFinl=qfFinl)
  
  #return results
  return(rpt)

}
