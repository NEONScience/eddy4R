##############################################################################################
#' @title Wrapper function: QFQM quality report for eddy-covariance dp04 HDF5 files

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Wrapper function. Summarize the qfqm reports for NEON expanded eddy-covariance HDF5 files.

#' @param File character vector for the full path to the HDF5 file to perform the quality report [-]
#' @param Dp A vector of data products to check the quality report for. Defaults to NULL which will test all dp's found in HDF5 file. Of type character. [-]

#' @param Vrbs A logical parameter that determines if all failed quality times should be reported (defaults to FALSE).

#' @return A list for all dp's provided including either summary or verbose quality report: \cr
#' \code{qm's}   quality metrics (percent) passed sub-data products if Vrbs = TRUE. [percent] \cr
#' \code{qmAlph} In dataframe column as "numeric" containing the alpha quality metric for sub-data products. [percent] \cr
#' \code{qmBeta} In dataframe column as "numeric" containing the beta quality metric for sub-data products. [percent] \cr
#' \code{qfFinl} In dataframe column as class "integer", [0,1], containing the final quality flag for sub-data products. [-] \cr
#' \code{qfSciRevw} In dataframe column as class "integer", [0,1], containing the scientific review quality flag for sub-data products. [-] \cr

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON QAQC, quality flags, quality metrics

#' @examples 
#' #no example at this time

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2019-04-30)
##############################################################################################


# start function wrap.qfqm.rpt()
wrap.qfqm.rpt <- function(
  File,
  dp = NULL,
  Vrbs = FALSE
) {
  
#Check if dp's are provided for quality report, otherwise check all dp's in qfqm portion of HDF5 file
if(is.null(dp)){
  # List of objects in the HDF5 file
  listObj <- rhdf5::h5ls(file = fileName, datasetinfo = FALSE)
  #Vector of data product names in the qfqm tabs in the HDF5 file
  dp <- listObj[grep(pattern = "/qfqm$", x = listObj$group),"name"]
} # End of if statement is.null(dp) if statement

#Base quality metric variable names for creating order of variables
qfqmVarBase <- c("timeBgn","timeEnd","qfFinl","qfSciRevw","qmAlph","qmBeta")

#Extract all the groups, data, and attributes from the HDF5 file
listHdf5 <- eddy4R.base::def.hdf5.extr(FileInp = fileName)

#Initialize lists
qfqm <- list()
rptQfqm <- list()

#Loop around data products to create quality report
for (idxDp in dp) {
  #idxDp <- "soni"
  #Create character string for regular expression search
  charSlct <- paste0("/qfqm/",idxDp,".*_30m")
  #Select the qfqm data for idxDp
  qfqm[[idxDp]] <- listHdf5$listData[grep(pattern = charSlct , x = names(listHdf5$listData))]
  
  #Report all qfqm values for periods where qfFinl = 1
  rptQfqm[[idxDp]] <- lapply(qfqm[[idxDp]], function(x) {
    
    rpt <- list()
    #Check if any final quality flags (qfFinl) are tripped
    if(sum(x$qfFinl) > 0) {
      
      #Subset rows where the final quality flag is failed
      rpt <- x[x$qfFinl == 1,]
      
      #Determine if all qfVarBase variables are present
      qfqmVarBaseSub <- dplyr::intersect(qfqmVarBase, base::names(rpt))
      
      #Determine all qf expanded variables
      qfqmVarExp <- base::grep(pattern = "Pass", x = dplyr::setdiff(base::names(rpt),qfqmVarBaseSub), value = TRUE)
      #Rearrange the data with the base variables at the front of the data.frame
      rpt <- rpt[,c(qfqmVarBaseSub,qfqmVarExp)]
      #############################################################################################################
      #Create a summary report
      #############################################################################################################
      #Create sum of qf's (qfFinl and qfSciRevw)
      if(length(grep(pattern = "qf", names(rpt))) > 1){
        qfSum <- as.data.frame(t(colSums(rpt[,grep(pattern = "qf", names(rpt))], na.rm = TRUE)))
      } else{
        qfSum <- as.data.frame(sum(rpt[,grep(pattern = "qf", names(rpt))], na.rm = TRUE))
        names(qfSum) <- grep(pattern = "qf", names(rpt), value = TRUE)
      }
      #Calculate means of all quality metrics
      qmMean <- as.data.frame(t(colMeans(rpt[,grep(pattern = "qm", names(rpt))], na.rm = TRUE)))
      
      #Convert units to percent
      if(length(qmMean) > 0) qmMean <- eddy4R.base::def.unit.conv(qmMean, unitFrom = "-", unitTo = "%", MethGc = FALSE)
      
      #Check if qmAlph and qmBeta are present to order the quality metrics
      if("qmAlph" %in% names(qmMean) & "qmBeta" %in% names(qmMean)){
        #Order quality metrics w/ qmAlph and qmBeta at the front, then order the rest by mean value
        qmMean <- qmMean[,c(grep("qmAlph|qmBeta", x = names(qmMean)), order(qmMean[,grep("qmAlph|qmBeta", x = names(qmMean), invert = TRUE)])+2)]
      }else{
        #Sort by values
        qmMean <- sort(qmMean)
      }
      
      #Create full summary report
      rptSmmy <- data.frame(timeBgn = rpt$timeBgn[1], timeEnd = rpt$timeEnd[length(rpt$timeEnd)], qfSum, qmMean, stringsAsFactors = FALSE)
      
      #############################################################################################################
      
      #If expanded variables are present, order them
      if(base::length(dplyr::setdiff(base::names(rpt),qfqmVarBase))>0){
        rpt <- rpt[,c(1:base::length(qfqmVarBaseSub),base::order(rpt[1,base::which(base::names(rpt) %in% qfqmVarExp)]) + base::length(qfqmVarBaseSub))]
      }
    }
    else rpt <- NULL #Return NULL if no bad qf's are reported
    
    #Final test if it should be summarized or full report
    if(!is.null(rpt) & Vrbs == FALSE) rpt <- rptSmmy
    
    #Return reported values
    return(rpt)
  })
  
  # Remove all empty lists
  if(length(rptQfqm[[idxDp]]) == 0){rptQfqm[[idxDp]] <- NULL}
  else
  {rptQfqm[[idxDp]][sapply(rptQfqm[[idxDp]], is.null)] <- NULL}
  if(length(rptQfqm[[idxDp]]) == 0){rptQfqm[[idxDp]] <- NULL}
}# End of loop around dp's

#Return quality report
return(rptQfqm)
}# End of function