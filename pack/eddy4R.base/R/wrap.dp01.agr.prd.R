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
#' Currently none

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights 
#   David Durden (2017-05-05)
#     original creation
#   David Durden (2017-12-12)
#     updating naming conventions
#   Natchaya P-Durden (2018-03-30)
#     applied term name convention; replace levl by lvl
#   Natchaya P-Durden (2018-05-11)
#     rename function from def.agr.ecte.dp01() to def.dp01.agr.ecte()
#   Natchaya P-Durden (2018-05-22)
#     rename function from wrap.neon.dp01() to wrap.dp01()
#     rename function from wrap.neon.dp01.agr.prd() to wrap.dp01.agr.prd()
#   Natchaya P-Durden (2018-05-23)
#     rename function from wrap.neon.dp01.qfqm.ec() to wrap.dp01.qfqm.ecte()
#   David Durden (2021-07-15)
#     dealing with R4.0.3 update bug, tmp$qfqm <- NULL was removing tmp$qfqmOut values, put in failsafe
##############################################################################################


wrap.dp01.agr.prd <- function(
 inpList
 ){

rpt <- list()
tmp <- list()



invisible(lapply(union(names(inpList$tmp$data), names(inpList$qfqm)), function(x) {
  if(x == "amrs") {
  tmp$idx[[x]] <<- eddy4R.base::def.idx.agr(time = inpList$data$amrs$time, PrdAgr = 60, FreqLoca = 40)
#} else if (x == "soni") {
  
  #tmp$idx[[x]] <<- eddy4R.base::def.idx.agr(time = inpList$data$time$UTC, PrdAgr = 120, FreqLoca = 20)
} else {  
  
  tmp$idx[[x]] <<- eddy4R.base::def.idx.agr(time = inpList$data$time$UTC, PrdAgr = 60, FreqLoca = 20)
}}))
  
#Determine the number of iterations by max list of elements  
iter <- max(sapply(names(tmp$idx), function(x) length(tmp$idx[[x]]$idxBgn)))


numAgr <- 0

  
  
for(idxAgr in 1:iter){
  # only the first 10 half-hours for standard testing
  #for(idxAgr in 1:10){
  # idxAgr <- 1
  numAgr <- numAgr + 1
  
  #Create a list identifier for the Aggregation loops
  lvlAgr <- paste0("numAgr",ifelse(numAgr < 10, paste0("0",numAgr),numAgr))

  for(idxSens in names(inpList$tmp$data)){
  #Grab sub-indices to calculate shorter timescale
  
  # assign data
    if(!idxAgr > length(tmp$idx[[idxSens]]$idxBgn)){
    
    tmp$data[[idxSens]] <- inpList$tmp$data[[idxSens]][tmp$idx[[idxSens]]$idxBgn[idxAgr]:tmp$idx[[idxSens]]$idxEnd[idxAgr],]
    }
    
    for(idxVar in base::names(tmp$data[[idxSens]])) {
      
      base::attr(x = tmp$data[[idxSens]][[idxVar]], which = "unit") <-
        base::attr(x = inpList$tmp$data[[idxSens]][[idxVar]], which = "unit")
      
    }; rm(idxVar)
    }
  
  for(idxSens in names(inpList$qfqm[grep(pattern = "time", x = names(inpList$qfqm), invert = TRUE)])){
    
    if(!idxAgr > length(tmp$idx[[idxSens]]$idxBgn)){
      
    tmp$qfqm[[idxSens]] <- inpList$qfqm[[idxSens]][tmp$idx[[idxSens]]$idxBgn[idxAgr]:tmp$idx[[idxSens]]$idxEnd[idxAgr],]
    }
    
    for(idxVar in base::names(tmp$qfqm[[idxSens]])) {
      
      base::attr(x = tmp$qfqm[[idxSens]][[idxVar]], which = "unit") <-
        base::attr(x = inpList$qfqm[[idxSens]][[idxVar]], which = "unit")
      
    }; rm(idxVar)
    
    }
  
  
    tmp$dp01[[lvlAgr]] <- eddy4R.base::wrap.dp01(
      # assign data: data.frame or list of type numeric or integer
      data = tmp$data,
      # if data is a list, which list entries should be processed into Level 1 data products?
      # defaults to NULL which expects data to be a data.frame
      idx = names(tmp$data)
    )
    
    tmp$qfqmOut[[lvlAgr]] <- eddy4R.base::wrap.dp01.qfqm.ecte(qfqm = tmp$qfqm, idx = names(tmp$data), MethMeas = "ecte", RptExpd = FALSE )

tmp$qfqm[names(tmp$qfqm)] <- NULL      
tmp$data <- NULL
invisible(gc())
}

#Format for output
rpt <- eddy4R.base::def.dp01.agr.ecte(inpList = tmp)

return(rpt)
}