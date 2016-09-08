##############################################################################################
#' @title Final Quality Flag (basic L1 data products) 

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org} \cr
#' Natchaya Pingintha-Durden \email{ndurdent@neoninc.org} \cr

#' @description 
#' Function definition. Determine the final quality flag for individual level 1 data product following the method described in Smith et.al. (2014). The quality metrics of failed, pass, and NA for each of the individual quality flag, alpha and beta quality metric were also computed in this function.

#' @param \code{data} A dataframe containing the input flag data. Of class integer". [-] 
#' @param \code{nameFlag} A vector of class "character" containing the name of flag data which will be used to calculate quality metrics of failed, pass, NA, alpha, and beta and final quality flag. [-] 
#' @param \code{rtioAlphBeta} Ratio of alpha quailty metric to beta quailty metric for determing the final quality flag. Default to rtioAlphBeta=c(2,1). [-] 
#' @param \code{thsh} Threshold for determine the condition (pass = 0 or failed = 1) of final quality flag. Default to 20 percent. [percent]

#' @return A dataframe containing quality metrics of failed, pass, and NA for each of the individual flag defined in \code{nameFlag} as well as alpha and beta quality metric and final qualiy flag.

#' @references 
#' NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113) \cr
#' Smith, D.E., Metzger, S., and Taylor, J.R.: A transparent and transferable framework for tracking quality information in large datasets. PLoS ONE, 9(11), e112249.doi:10.1371/journal.pone.0112249, 2014. \cr 

#' @keywords NEON QAQC, quality flags and metrics, final quality flag

#' @examples
#' qfA <- c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,1) 
#' qfB <- c(0,0,-1,-1,-1,1,1,0,0,0,0,0,0,0,0)
#' qfC <- c(0,1,1,0,0,0,0,0,0,0,0,0,-1,-1,-1)
#' data<- data.frame(qfA,qfB,qfC)
#' test<- def.qf.finl.l1(data=data, nameFlag=colnames(data), rtioAlphBeta=c(2,1), thsh=20)

#' @seealso def.qfqm.l1()

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-01-05)
#     original creation of def.qfqm.l1.R
#   Natchaya P-Durden (2016-08-15)
#     modified def.qfqm.l1()
##############################################################################################
def.qf.finl.l1 <- function (
  data,
  nameFlag,
  rtioAlphBeta=c(2,1),
  thsh=20) {
  
  # Take inventory of the flags we have and set up output naming
  #nameFlag <- colnames(data) # name of flags
  qfqm <- as.list(data) # copy variable names to output
  numFlag <- length(nameFlag) #number of flags 
  nameVarOut <- c() #output variable names
  nameQm <- c("Pass","Fail","Na") # 3 subvariables per quality metric
  
  # Put together output variable names
  for (idxFlag in numeric(numFlag)+1:numFlag) {
    tmp <- strsplit(nameFlag[idxFlag],vector(length=0))
    for (idxQm in 1:3) {
      nameVarOut[(idxFlag-1)*3+idxQm] <- paste("qm",toupper(tmp[[1]][1]),paste(tmp[[1]][2:length(tmp[[1]])],collapse=""),nameQm[idxQm],sep="",collapse ="")
    }
  }
  
  nameVarOut <- c(nameVarOut,"qmAlph","qmBeta","qfFinl") # Add alpha QM, beta QM, and final quality flag
  qfqm<- data.frame(matrix(,ncol=3*numFlag+3))#initial output
  colnames(qfqm) <- nameVarOut
  
  # Compute Quality metric -------------------------------------------------
  # Loop through each flag
  for(idxFlag in 1:numFlag) {
    
    # Quality metrics for pass, failed, and NA flags
    # Pass
    qfqm[,(idxFlag-1)*3+1]  <- sum(data[,idxFlag]== 0, na.rm = TRUE)/nrow(data)*100
    # Fail
    qfqm[,(idxFlag-1)*3+2] <- sum(data[,idxFlag]== 1, na.rm = TRUE)/nrow(data)*100
    # NA
    qfqm[,(idxFlag-1)*3+3] <- sum(data[,idxFlag]== -1, na.rm = TRUE)/nrow(data)*100 
    
  }
  #compute qfAlpha and qfBeta
  data[,"qfAlph"] <- apply(data, 1, function(x) ifelse(any(x==1),1,0))
  #calculate qfBeta
  data[,"qfBeta"] <- apply(data, 1, function(x) ifelse(any(x==-1),1,0))
  
  #calculate QMAlpha
  qfqm[, "qmAlph"] <- sum(data[,"qfAlph"]== 1, na.rm = TRUE)/nrow(data)*100
  #calculate QMBeta
  qfqm[, "qmBeta"] <- sum(data[,"qfBeta"]== 1, na.rm = TRUE)/nrow(data)*100  
  
  #calculate QF Final
  if ((rtioAlphBeta[1]*qfqm[, "qmAlph"]) + (rtioAlphBeta[2]*qfqm[, "qmBeta"]) >= thsh) {
    qfqm[, "qfFinl"] <- 1
  } else {qfqm[, "qfFinl"] <- 0}
  
  return(qfqm)
}
