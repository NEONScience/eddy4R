##############################################################################################
#' @title Definition function: to package dp01 dp01 outputs to be written to HDF5 files

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org} \cr

#' @description 
#' Definition function to produce a list of dataframes corresponding times for aggregation periods for dp01, and are packaged to be written to the HDF5 file.
#'
#' @param inpList a list of dp01 computed output statistics or dp01 quality flags and quality metrics over multiple aggregations periods that need to be combined and formatted for output to HDF5.
#' @param time a dataframe including the timeBgn and timeEnd for the aggregated periods should be included in the data to be combined.
#' @param Dp01 which data product is being packaged to be written to the HDF5 file
#' @param MethMeas A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecse". [-]
#'  
#' @return A list of dataframes of for aggregation periods.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords NEON, dp01, averaging intervals, HDF5

#' @examples 
#' Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-05-07)
#     original creation
#   Dave Durden (2017-05-22)
#     Updating time formatting for output
#   Natchaya Pingintha-Durden
#     Added MethMeas
##############################################################################################



def.hdf5.dp01.pack <- function(
  inpList,
  time,
  Dp01,
  MethMeas = c("ecte", "ecse")[1]
){

  #Initializing lists
rpt <- list()
tmp <- list()

# ecte #######################################################################################
if (MethMeas == "ecte") {
#Looping around variables
for(idxVar in names(inpList[[Dp01]][[1]])) {
  #print(idxVar) #For testing
  #Reformatting data to have data subproducts at a higher hierarchical level than descriptive stats for HDF5 output
  lapply(names(inpList[[Dp01]]), function(x) {
    tmp[[idxVar]][[x]] <<- inpList[[Dp01]][[x]][,idxVar]
  }) 
  
};rm(idxVar)

# Combining list of data into datafram
rpt <- lapply(names(tmp), function(x) data.frame(do.call("cbind", tmp[[x]])))

#Copying names from input
names(rpt) <- names(tmp)

#If values come in as Posix, they must first be converted to characters
if(!is.character(time[[Dp01]]$timeBgn)){time[[Dp01]] <- lapply(time[[Dp01]], strftime, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")} 

#Adding time to output dataframe
rpt <- lapply(rpt, cbind, timeBgn = time[[Dp01]]$timeBgn, timeEnd = time[[Dp01]]$timeEnd, stringsAsFactors = FALSE)

#Looping around varaibles
for(idxVar in base::names(inpList[[Dp01]][[1]])) {
 #Writing unit attributes to each variable 
  base::attr(x = rpt[[idxVar]], which = "unit") <-
    base::attr(x = inpList[[Dp01]][[1]][,idxVar], which = "unit")
  
}; rm(idxVar)
}#end of MethMeas == "ecte" #################################################################

# ecse #######################################################################################
if (MethMeas == "ecse") { 
  for(idxDp01 in names(inpList)) {
    
    tmp[[idxDp01]] <- list()
    
    # first call to lapply, targeting the result data.frames to be created (data sub-products: mean, min, max, vari", numSamp)
    for(idxLvLReso in names(inpList[[idxDp01]])) {
      #idxLvLReso <- names(inpList[[idxDp01]])[1]
      
      tmp[[idxDp01]][[idxLvLReso]] <- lapply(names(inpList[[idxDp01]][[idxLvLReso]][[1]]), function(idxStat)
        # second call to lapply, targeting the observations to be combined into the result data.frames
        do.call(rbind, lapply(1:length(inpList[[idxDp01]][[idxLvLReso]]), function(idxt) inpList[[idxDp01]][[idxLvLReso]][[idxt]][[idxStat]] ))
      )
      
      # assign names to data.frames      
      names(tmp[[idxDp01]][[idxLvLReso]]) <- names(inpList[[idxDp01]][[idxLvLReso]][[1]])
      
    }
    
  }
  
  #restructure output from tmp: tmp$co2Stor$`000_010_02m`$mean$frt00 to rpt$co2Stor$`000_010_02m`$frt00$mean
  #rpt <- list()
  
  #loop around c("h2oStor", "co2Stor")
  for(idxDp01 in names(tmp)) {
    # idxDp01 <- names(tmp)[2]
    
    # create directory
    #dir.create(paste0(Para$Flow$DirOut, "/", Para$Flow$Loc, "/", Para$Flow$VersDp), showWarnings = FALSE)
    
    rpt[[idxDp01]] <- list()
    
    for (idxLvLReso in names(tmp[[idxDp01]])){
      #idxLvLReso <- names(tmp[[idxDp01]])[1] 
      
      rpt[[idxDp01]][[idxLvLReso]] <- list()
      
      for(idxVar in names(tmp[[idxDp01]][[idxLvLReso]][[1]])) {
        #idxVar <- names(tmp[[idxDp01]][[idxLvLReso]][[1]])[1]
        
        #print(idxVar)  
        lapply(names(tmp[[idxDp01]][[idxLvLReso]]), function(idxStat) {
          rpt[[idxDp01]][[idxLvLReso]][[idxVar]][[idxStat]] <<- tmp[[idxDp01]][[idxLvLReso]][[idxStat]][,idxVar]
          #return(tmpDataList)
          #dataTest <<- cbind(dataTest,idxTestOut)
        })
        
      }#; rm(idxVar)
      
    }#; rm(idxLvLReso)
    
  }#; rm(idxDp01)
  
  #adjust timeBgn and timeEnd
  for(idxDp01 in names(rpt)) {
    # idxDp01 <- names(tmp)[2]
    
    for (idxLvLReso in names(rpt[[idxDp01]])){
      #idxLvLReso <- names(tmp[[idxDp01]])[1] 
      
      for(idxVar in names(rpt[[idxDp01]][[idxLvLReso]])) {
        #idxVar <- names(rpt[[idxDp01]][[idxLvLReso]])
        
        for(idxAgr in c(1:length(rpt[[idxDp01]][[idxLvLReso]][[idxVar]][[1]]))){
          
          if(idxAgr == 1){
            
            tmpBgn <- as.character(rpt[[idxDp01]][[idxLvLReso]][[idxVar]]$timeBgn[[1]][1])
            tmpEnd <- as.character(rpt[[idxDp01]][[idxLvLReso]][[idxVar]]$timeEnd[[1]][1])
            
          } else {
            
            tmpBgn <- c(tmpBgn, as.character(rpt[[idxDp01]][[idxLvLReso]][[idxVar]]$timeBgn[[idxAgr]][1]))
            tmpEnd <- c(tmpEnd, as.character(rpt[[idxDp01]][[idxLvLReso]][[idxVar]]$timeEnd[[idxAgr]][1]))
          }
          
          
        }; rm(idxAgr)
        
        rpt[[idxDp01]][[idxLvLReso]][[idxVar]]$timeBgn <- format(as.POSIXct(tmpBgn, format= "%Y-%m-%d %H:%M:%OS"), "%Y-%m-%dT%H:%M:%OSZ")
        rpt[[idxDp01]][[idxLvLReso]][[idxVar]]$timeEnd <- format(as.POSIXct(tmpEnd, format= "%Y-%m-%d %H:%M:%OS"), "%Y-%m-%dT%H:%M:%OSZ")
        
      }; rm(idxVar)
      
    }; rm(idxLvLReso)
    
  }; rm(idxDp01)

#rpt <- rpt 
  
}#end of MethMeas == "ecse" #################################################################
return(rpt)
}
