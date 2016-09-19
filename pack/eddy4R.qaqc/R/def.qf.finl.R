##############################################################################################
#' @title Final Quality Flag (basic L1 data products)

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org} \cr
#' Natchaya Pingintha-Durden \email{ndurdent@neoninc.org} \cr

#' @description 
#' Function definition. Determine the final quality flag for individual level 1 data product following the method described in Smith et.al. (2014). The alpha and beta quality flags and metrics were also computed in this function.


#' @param \code{data} A dataframe containing the input flag data that going to use to determine alpha and beta quality flags, quality metrics, and final quality flag. Of class integer". [-] 
#' @param \code{rtioAlphBeta} Ratio of alpha quailty metric to beta quailty metric for determing the final quality flag. Default to rtioAlphBeta=c(2,1). [-] 
#' @param \code{thsh} Threshold for determine the condition (pass = 0 or failed = 1) of final quality flag. Default to 20 percent. [percent]

#' @return A list of: \cr
#' \code{qaqcRpt} A dataframe containing alpha and beta quality flags at the same frequency as input data. [-] \cr
#' \code{qfqm} A dataframe containing alpha and beta quality metric and final qualiy flag. [-] \cr

#' @references 
#' NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113) \cr
#' Smith, D.E., Metzger, S., and Taylor, J.R.: A transparent and transferable framework for tracking quality information in large datasets. PLoS ONE, 9(11), e112249.doi:10.1371/journal.pone.0112249, 2014. \cr 

#' @keywords final quality flag, NEON QAQC, quality flags and metrics

#' @examples
#' qfA <- c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,1) 
#' qfB <- c(0,0,-1,-1,-1,1,1,0,0,0,0,0,0,0,0)
#' qfC <- c(0,1,1,0,0,0,0,0,0,0,0,0,-1,-1,-1)
#' test<-list()
#' test$qf <- data.frame(qfA,qfB,qfC)
#' out <- def.qf.finl(data=test$qf, rtioAlphBeta=c(2,1), thsh=20)

#' @seealso 
#' \code{\link[eddy4R.qaqc]{def.qfqm.l1}} \cr
#' \code{\link[eddy4R.qaqc]{def.qm}} \cr

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-01-05)
#     original creation of def.qfqm.l1.R 
#   Natchaya P-Durden (2016-09-19)
#     Generated def.qf.finl() from def.qfqm.l1() 
##############################################################################################
def.qf.finl <- function (
  data,
  rtioAlphBeta=c(2,1),
  thsh=20
) {
 
# Compute Quality metric ---------------------------------------------------
  #compute qfAlpha and qfBeta
  data[,"qfAlph"] <- apply(data, 1, function(x) ifelse(any(x==1),1,0))
  #calculate qfBeta
  data[,"qfBeta"] <- apply(data, 1, function(x) ifelse(any(x==-1),1,0))
  
  #calculate QMAlpha
  qmAlph <- sum(data[,"qfAlph"]== 1, na.rm = TRUE)/nrow(data)*100
  #calculate QMBeta
  qmBeta <- sum(data[,"qfBeta"]== 1, na.rm = TRUE)/nrow(data)*100 
  
  #calculate QF Final
  if ((rtioAlphBeta[1]*qmAlph) + (rtioAlphBeta[2]*qmBeta) >= thsh) {
    qfFinl <- 1
  } else {qfFinl <- 0}

#AGGREGATE AND RETURN RESULTS ----------------------------------------------
  
  #aggregate results
  rpt<-list()
  rpt$qaqcRpt <- data.frame(qfAlph=data[,"qfAlph"], qfBeta=data[,"qfBeta"])
  rpt$qfqm <- data.frame(qmAlph=qmAlph, qmBeta=qmBeta, qfFinl=qfFinl)
  
  #return results
  return(rpt)

}
