##############################################################################################
#' @title Definition function: Quality Indicators

#' @author
#' David Durden \email{eddy4R.info@gmail.com}

#' @description 
#' Function definition. Convert quality flags (qf) to quality indicators (qi) by changing names. Performed for the entire set of input data.

#' @param qf A data frame or list of data.frames containing quality flags, class integer. Each column contains the quality flag values [-1,0,1] for that flag. Note: This is the Vrbs output from def.plau, def.dspk.wndw, and def.dspk.br86. See def.conv.qf.vrbs for converting from non-verbose to verbose output.

#' @return A dataframe containing quality indicators of failed (1), pass (0), and NA (-1) for each of the individual flag defined by input \code{qf}.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113) \cr
#' Smith, D.E., Metzger, S., and Taylor, J.R.: A transparent and transferable framework for tracking quality information in large datasets. PLoS ONE, 9(11), e112249.doi:10.1371/journal.pone.0112249, 2014. \cr 

#' @keywords NEON QAQC, quality flags, quality metrics

#' @examples
#' qfA <- c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,1) 
#' qfB <- c(0,0,-1,-1,-1,1,1,0,0,0,0,0,0,0,0)
#' qfC <- c(0,1,1,0,0,0,0,0,0,0,0,0,-1,-1,-1)
#' qfD <- data.frame(qfE = c(0,1,1,0,0,0,0,0,0,0,0,0,-1,-1,-1), qfF = c(0,1,1,0,0,0,0,-1,0,0,0,0,-1,-1,-1))
#' test<-list()
#' test$dfQf <- data.frame(qfA,qfB,qfC)
#' test$listQf <- list(test$dfQf,qfD)
#' test$qi<- def.qi.qf(qf=test$listQf)

#' @seealso 
#' \code{\link[eddy4R.qaqc]{wrap.qfqm.dp01}}
#' \code{\link[eddy4R.qaqc]{def.dp01.grp.qf}}
#' \code{\link[eddy4R.qaqc]{wrap.dp01.qfqm.eddy}}
#' \code{\link[eddy4R.qaqc]{def.qf.finl.R}}
#' \code{\link[eddy4R.qaqc]{def.qm.R}}

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2021-08-24)
#     original creation

##############################################################################################
def.qi.qf <- function (
  qf # A data.frame or list of data.frames containing quality flags, class integer. Each column contains the quality flag values [-1,0,1] for that flag. 

) {
  
 #Check if input is a list of data.frames, if TRUE convert to single data frame
  if(base::is.list(qf)){qf <- base::cbind.data.frame(qf)}
  
  #convert quality flags to quality indicators
  names(qf) <- base::gsub(pattern = "^qf", replacement = "qi", x = names(qf))
  
  #Return output data.frame with updated names
  return(qf)
}#End def.qi.qf