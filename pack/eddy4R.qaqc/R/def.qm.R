##############################################################################################
#' @title Quality Metrics 

#' @author
#' Cove Sturtevant \email{csturtevant@neoninc.org} \cr
#' Natchaya Pingintha-Durden \email{ndurdent@neoninc.org} \cr

#' @description 
#' Function definition. Determine the quality metrics of failed, pass, and NA for each of the individual quality flag following the method described in Smith et.al. (2014).

#' @param \code{data} A dataframe containing the input flag data. Of class integer". [-] 
#' @param \code{nameFlag} A vector of class "character" containing the name of flag data which will be used to calculate quality metrics of failed, pass, NA, alpha, and beta and final quality flag. [-] 

#' @return A dataframe containing quality metrics of failed, pass, and NA for each of the individual flag defined in \code{nameFlag}.

#' @references 
#' NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113) \cr
#' Smith, D.E., Metzger, S., and Taylor, J.R.: A transparent and transferable framework for tracking quality information in large datasets. PLoS ONE, 9(11), e112249.doi:10.1371/journal.pone.0112249, 2014. \cr 

#' @keywords NEON QAQC, quality flags, quality metrics

#' @examples
#' qfA <- c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,1) 
#' qfB <- c(0,0,-1,-1,-1,1,1,0,0,0,0,0,0,0,0)
#' qfC <- c(0,1,1,0,0,0,0,0,0,0,0,0,-1,-1,-1)
#' test<-list()
#' test$qf <- data.frame(qfA,qfB,qfC)
#' test$qm<- def.qm(data=test$qf, nameFlag=colnames(test$qf))

#' @seealso 
#' \code{\link[eddy4R.qaqc]{def.qfqm.l1}}

#' @export

# changelog and author contributions / copyrights 
#   Cove Sturtevant (2016-01-05)
#     original creation of def.qfqm.l1.R
#   Natchaya P-Durden (2016-08-15)
#     modified def.qfqm.l1()
##############################################################################################
def.qm <- function (
  data,
  nameFlag
  ) {
  
  # Take inventory of the flags we have and set up output naming
  #nameFlag <- colnames(data) # name of flags
  qm <- as.list(data) # copy variable names to output
  numFlag <- length(nameFlag) #number of flags 
  nameVarOut <- c() #output variable names
  nameQm <- c("Pass","Fail","Na") # 3 subvariables per quality metric
  
  # Put together output variable names
  for (idxFlag in numeric(numFlgs)+1:numFlgs) {
    
    # Get rid of "posFlag" if using output from def.plau
    tmp <- sub(pattern="posFlag", replacement="", x=nameFlag[idxFlag], ignore.case = FALSE, perl = FALSE,
               fixed = FALSE, useBytes = FALSE)
    # Get rid of "qf" if using other output 
    tmp <- sub(pattern="qf", replacement="", x=nameFlag[idxFlag], ignore.case = FALSE, perl = FALSE,
               fixed = FALSE, useBytes = FALSE)
    
    for (idxQm in 1:3) {
      nameVarOut[6+(idxFlag-1)*3+idxQm] <- paste0("qm",tools::toTitleCase(tmp),nameVarsOutQm[idxQm],collapse ="")
    }
  }
  
  qm<- data.frame(matrix(,ncol=3*numFlag))#initial output
  colnames(qm) <- nameVarOut
  
  # Compute Quality metric -------------------------------------------------
  # Loop through each flag
  for(idxFlag in 1:numFlag) {
    
    # Quality metrics for pass, failed, and NA flags
    # Pass
    qm[,(idxFlag-1)*3+1]  <- sum(data[,idxFlag]== 0, na.rm = TRUE)/nrow(data)*100
    # Fail
    qm[,(idxFlag-1)*3+2] <- sum(data[,idxFlag]== 1, na.rm = TRUE)/nrow(data)*100
    # NA
    qm[,(idxFlag-1)*3+3] <- sum(data[,idxFlag]== -1, na.rm = TRUE)/nrow(data)*100 
    
  }
  
  return(qm)
}
