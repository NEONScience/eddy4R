##############################################################################################
#' @title Wrapper function: Create NEON Level 1 data products with quality flags and quality metrics for different aggregation periods 

#' @author
#' David Durden \email{ddurden@battelleecology.org}

#' @description 
#' Wrapper function. Compute NEON Level 1 data products with quality flags and quality metrics for different aggregation periods (e.g. 1 minute).

#' @param inpList A list of including dp0p data and quality flags to perform dp01 calculation and quality tests.

#' @return A list containing dp01 data and quality flags and metrics for all sensors including the following: \cr
#' \code{data}:
  #' \code{mean} The mean of non-NA values in \code{data}
  #' \code{min} The minimum value of non-NA values in \code{data}
  #' \code{max} The maximum value of non-NA values in \code{data}
  #' \code{vari} The variance of non-NA values in \code{data}
  #' \code{num} The number of non-NA values in \code{data}
  #' \code{se} The standard error of the mean of non-NA values in \code{data}
#'\code{qfqm}: 
  #' \code{qm}  A list of data frame's containing quality metrics (fractions) of failed, pass, and NA for each of the individual flag which related to L1 sub-data products if RptExpd = TRUE. [fraction] \cr
  #' \code{qmAlph} A dataframe containing metrics in a  columns of class "numeric" containing the alpha quality metric for L1 sub-data products. [fraction] \cr
  #' \code{qmBeta} A dataframe containing metrics in a columns of class "numeric" containing the beta quality metric for L1 sub-data products. [fraction] \cr
  #' \code{qfFinl} A dataframe containing flags in a columns of class "numeric", [0,1], containing the final quality flag for L1 sub-data products. [-] \cr
  #' \code{qfSciRevw} A dataframe containing flags in a columns of class "numeric", [0,1], containing the scientific review quality flag for L1 sub-data products. [-] \cr

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords average, aggregate, descriptive statistics, NEON QAQC, quality flags, quality metrics

#' @examples 


#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.


#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2017-05-05)
#     original creation
##############################################################################################


wrap.neon.dp01.agr.prd <- function(
 inpList
 ){

rpt <- list()
tmp <- list()

#Grab sub-indices to calculate shorter timescale
tmp$idx <- def.idx.agr(time = wrk$data$time$UTC, PrdAgr = 60, FreqLoca = 20)

tmp$idxSoni <- def.idx.agr(time = DATA$soniAmrs$time, PrdAgr = 120, FreqLoca = 20)

tmp$idxSoniAmrs <- def.idx.agr(time = DATA$soniAmrs$time, PrdAgr = 60, FreqLoca = 40)

numAgr <- 0

for(idxAgr in 1:length(tmp$idxSoniAmrs)){
  # only the first 10 half-hours for standard testing
  #for(idxAgr in 1:10){
  # idxAgr <- 1
  numAgr <- numAgr + 1
  
  #Create a list identifier for the Aggregation loops
  levlAgr <- paste0("numAgr",ifelse(numAgr < 10, paste0("0",numAgr),numAgr))

for(idxSens in names(inpList$data)){
  
  # assign data
  
  if(idxSens == "soniAmrs") {
    
    tmp$data[[idxSens]] <- inpList$data[[idxSens]][tmp$idxSoniAmrs$idxBgn[idxAgr]:tmp$idxSoniAmrs$idxEnd[idxAgr],]
    tmp$qfqm[[idxSens]] <- inpList$qfqm[[idxSens]][tmp$idxSoniAmrs$idxBgn[idxAgr]:tmp$idxSoniAmrs$idxEnd[idxAgr],]
    
  } else if (idxSens == "soni") {
    
    tmp$data[[idxSens]] <- inpList$data[[idxSens]][tmp$idxSoni$idxBgn[idxAgr]:tmp$idxSoni$idxEnd[idxAgr],]
    tmp$qfqm[[idxSens]] <- inpList$qfqm[[idxSens]][tmp$idxSoni$idxBgn[idxAgr]:tmp$idxSoni$idxEnd[idxAgr],]
    
   
  } else {
    
    tmp$data[[idxSens]] <- inpList$data[[idxSens]][tmp$idx$idxBgn[idxAgr]:tmp$idx$idxEnd[idxAgr],]
    tmp$qfqm[[idxSens]] <- inpList$qfqm[[idxSens]][tmp$idx$idxBgn[idxAgr]:tmp$idx$idxEnd[idxAgr],]
    
  }
  


}