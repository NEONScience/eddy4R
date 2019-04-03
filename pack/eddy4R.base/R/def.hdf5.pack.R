##############################################################################################
#' @title Definition function: to package NEON eddy-covariance data product outputs to be written to HDF5 files

#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org} \cr
#' Dave Durden \email{ddurden@battelleecology.org} \cr

#' @description 
#' Definition function to produce a list of dataframes corresponding times for aggregation periods for NEON eddy-covariance data product, and are packaged to be written to the HDF5 file.
#'
#' @param inpList A list of NEON eddy-covariance computed output statistics or quality flags and quality metrics over multiple aggregations periods that need to be combined and formatted for output to HDF5.
#' @param MethMeas A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecte". 
#' @param Dp If MethMeas = "ecte", Dp is which data product is being packaged to be written to the HDF5 file. \cr
#' If MethMeas = "ecse", Dp is which level of data product is being packaged to be written to the HDF5 file, Dp = c("Dp01", "Dp02", "Dp03", "Dp04").
#' @param time Optional. If MethMeas = "ecte", a dataframe including the timeBgn and timeEnd for the aggregated periods should be included in the data to be combined.
#'   
#' @return A list of dataframes of for aggregation periods.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, ECSE, data products, averaging intervals, HDF5

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya Pingintha-Durden (2017-09-07)
#     original creation
#   Natchaya Pingintha-Durden (2017-09-07)
#     added MethMeas and rename function to def.hdf5.dp.pack()
#   Natchaya P-Durden (2018-05-11)
#     rename function from def.hdf5.dp.pack() to def.hdf5.pack()
#   David Durden (2018-12-09)
#     Adding mechanism to deal with quality metrics to be output for Expanded files
##############################################################################################

def.hdf5.pack <- function(
  inpList,
  MethMeas = c("ecte", "ecse")[1],
  time,
  Dp
){
  
  #Initializing lists
  rpt <- list()
  tmp <- list()

## ECTE #####################################################################################
if(MethMeas %in% "ecte"){
  
  #Check if qm is part of the input list
  if(exists('qm', where = inpList[[Dp]]) == TRUE){

  # Add the qm's to tmp list    
  tmp <- lapply(names(inpList[[Dp]]$qm), function(x){
    inpList[[Dp]]$qm[[x]]
  })
  
  #Write the list names
  names(tmp) <- names(inpList[[Dp]]$qm)
  
  #Remove qm from the inpList
  inpList[[Dp]]$qm <- NULL
  }
  
  
  #Looping around variables
  for(idxVar in names(inpList[[Dp]][[1]])) {
    #print(idxVar) #For testing
    #Reformatting data to have data subproducts at a higher hierarchical level than descriptive stats for HDF5 output
    lapply(names(inpList[[Dp]]), function(x) {
      tmp[[idxVar]][[x]] <<- inpList[[Dp]][[x]][,idxVar]
    }) 
    
  };rm(idxVar)
  
  # Combining list of data into datafram
  rpt <- lapply(names(tmp), function(x) data.frame(do.call("cbind", tmp[[x]])))
  
  #Copying names from input
  names(rpt) <- names(tmp)
  
  #If values come in as Posix, they must first be converted to characters
  if(!is.character(time[[Dp]]$timeBgn)){time[[Dp]] <- lapply(time[[Dp]], strftime, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")} 
  
  #Adding time to output dataframe
  rpt <- lapply(rpt, cbind, timeBgn = time[[Dp]]$timeBgn, timeEnd = time[[Dp]]$timeEnd, stringsAsFactors = FALSE)
  
  #Looping around varaibles
  for(idxVar in base::names(inpList[[Dp]][[1]])) {
    #Writing unit attributes to each variable 
    base::attr(x = rpt[[idxVar]], which = "unit") <-
      base::attr(x = inpList[[Dp]][[1]][,idxVar], which = "unit")
    
  }; rm(idxVar)
  
}#end of ECTE
  
## ECSE ####################################################################################
if(MethMeas %in% "ecse"){

  ##Dp01 and Dp02 #############################################################################  
  if (Dp %in% c("Dp01", "Dp02")) { 
    
    for(idxDp in names(inpList)) {
      
      tmp[[idxDp]] <- list()
      
      # first call to lapply, targeting the result data.frames to be created (data sub-products: mean, min, max, vari", numSamp)
      for(idxLvLReso in names(inpList[[idxDp]])) {
        #idxLvLReso <- names(inpList[[idxDp]])[1]
        
        tmp[[idxDp]][[idxLvLReso]] <- lapply(names(inpList[[idxDp]][[idxLvLReso]][[1]]), function(idxStat)
          # second call to lapply, targeting the observations to be combined into the result data.frames
          do.call(rbind, lapply(1:length(inpList[[idxDp]][[idxLvLReso]]), function(idxt) inpList[[idxDp]][[idxLvLReso]][[idxt]][[idxStat]] ))
        )
        
        # assign names to data.frames      
        names(tmp[[idxDp]][[idxLvLReso]]) <- names(inpList[[idxDp]][[idxLvLReso]][[1]])
        
      }
      
    }
    
    #restructure output from tmp: tmp$co2Stor$`000_010_02m`$mean$frt00 to rpt$co2Stor$`000_010_02m`$frt00$mean
    #rpt <- list()
    
    #loop around c("h2oStor", "co2Stor")
    for(idxDp in names(tmp)) {
      # idxDp <- names(tmp)[2]
      
      # create directory
      #dir.create(paste0(Para$Flow$DirOut, "/", Para$Flow$Loc, "/", Para$Flow$VersDp), showWarnings = FALSE)
      
      rpt[[idxDp]] <- list()
      
      for (idxLvLReso in names(tmp[[idxDp]])){
        #idxLvLReso <- names(tmp[[idxDp]])[1] 
        rpt[[idxDp]][[idxLvLReso]] <- list()
        if (Dp == "Dp01") {tmpIdxVar <- names(tmp[[idxDp]][[idxLvLReso]][[1]])}
        if (Dp == "Dp02") {tmpIdxVar <- dimnames(tmp[[idxDp]][[idxLvLReso]][[1]])[[2]]}
  
        for(idxVar in tmpIdxVar) {
          #idxVar <- names(tmp[[idxDp]][[idxLvLReso]][[1]])[1]
          #print(idxVar)  
          lapply(names(tmp[[idxDp]][[idxLvLReso]]), function(idxStat) {
            rpt[[idxDp]][[idxLvLReso]][[idxVar]][[idxStat]] <<- tmp[[idxDp]][[idxLvLReso]][[idxStat]][,idxVar]
            #return(tmpDataList)
            #dataTest <<- cbind(dataTest,idxTestOut)
          })
          
        }#; rm(idxVar)
        
      }#; rm(idxLvLReso)
      
    }#; rm(idxDp)
    
    #adjust timeBgn and timeEnd
    for(idxDp in names(rpt)) {
      # idxDp <- names(tmp)[2]
      
      for (idxLvLReso in names(rpt[[idxDp]])){
        #idxLvLReso <- names(tmp[[idxDp]])[1] 
        
        for(idxVar in names(rpt[[idxDp]][[idxLvLReso]])) {
          #idxVar <- names(rpt[[idxDp]][[idxLvLReso]])
          
          for(idxAgr in c(1:length(rpt[[idxDp]][[idxLvLReso]][[idxVar]][[1]]))){
            
            if(idxAgr == 1){
              
              tmpBgn <- as.character(rpt[[idxDp]][[idxLvLReso]][[idxVar]]$timeBgn[[1]][1])
              tmpEnd <- as.character(rpt[[idxDp]][[idxLvLReso]][[idxVar]]$timeEnd[[1]][1])
              
            } else {
              
              tmpBgn <- c(tmpBgn, as.character(rpt[[idxDp]][[idxLvLReso]][[idxVar]]$timeBgn[[idxAgr]][1]))
              tmpEnd <- c(tmpEnd, as.character(rpt[[idxDp]][[idxLvLReso]][[idxVar]]$timeEnd[[idxAgr]][1]))
            }
            
            
          }; rm(idxAgr)
          
          rpt[[idxDp]][[idxLvLReso]][[idxVar]]$timeBgn <- format(as.POSIXct(tmpBgn, format= "%Y-%m-%d %H:%M:%OS"), "%Y-%m-%dT%H:%M:%OSZ")
          rpt[[idxDp]][[idxLvLReso]][[idxVar]]$timeEnd <- format(as.POSIXct(tmpEnd, format= "%Y-%m-%d %H:%M:%OS"), "%Y-%m-%dT%H:%M:%OSZ")
          
        }; rm(idxVar)
        
      }; rm(idxLvLReso)
      
    }; rm(idxDp)
    
    #rpt <- rpt 
    
  }#end of Dp01 and Dp02 #################################################################

}#end of ECSE
  
  return(rpt)

}
  