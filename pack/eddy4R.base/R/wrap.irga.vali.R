##############################################################################################
#' @title Wrapper function: Validation processing for IRGA

#' @author
#' Natchaya P-Durden \email{eddy4R.info@gmail.com}

#' @description Wrapper function to apply IRGA validation.

#' @param data List consisting of \code{ff::ffdf} file-backed objects containing the dp0p input IRGA.
#' @param qfqmFlag List consisting of \code{ff::ffdf} file-backed objects containing the IRGA quality flags.
#' @param DateProc A vector of class "character" containing the processing date.

#' @return 
#' The returned object consistes of \code{inpList}, with the derived variables added to the respective list entry, and all list levels sorted alphabetically.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.

#' @keywords derived, irgaTurb, post-processing, pre-processing, validation

#' @examples
#' Currently none.

#' @seealso Currently none.

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2018-11-14)
#     original creation
##############################################################################################

wrap.irga.vali <- function(
  data,
  qfqmFlag,
  DateProc
) {
  #pre-processing date
  DatePre <- base::as.Date(DateProc) - 1
  #post-processing date
  DatePost <- base::as.Date(DateProc) + 1
  #grab 3 days window of irga data and qfqmFlag (pre-processing, processing, and post-processing date)
  #get indecies
  idxSub <- which(as.Date(data$irgaTurb$time[]) == DatePre |
                    as.Date(data$irgaTurb$time[]) == DateProc |
                    as.Date(data$irgaTurb$time[]) == DatePost)
  #subset data
  subData <- data$irgaTurb[][min(idxSub):max(idxSub),]
  #subset qfqmFlag
  subQfqmFlag <- qfqmFlag$irgaTurb[][min(idxSub):max(idxSub),]
  
  nameQf <- c("qfIrgaTurbValiGas01", "qfIrgaTurbValiGas02", "qfIrgaTurbValiGas03", "qfIrgaTurbValiGas04", "qfIrgaTurbValiGas05")
  #assign list
  tmp <- list()
  rpt <- list()
  
  for (idxNameQf in nameQf){
    #idxNameQf <- nameQf[2]
    #preparing the qfIrgaTurbValiGas01 to 05 data for def.idx.agr()
    #replace NA to the qf which are not equal to 1
    subQfqmFlag[[idxNameQf]] <- ifelse(subQfqmFlag[[idxNameQf]] != 1, NA, subQfqmFlag[[idxNameQf]])
    
    #determine when validation occur
    #if there is at least one measurement
    if(length(which(!is.na(subQfqmFlag[[idxNameQf]]))) > 0){
      #determine the beginning and ending indicies of each validation
      idxVali <- eddy4R.base::def.idx.agr(time = subData$time, PrdAgr = (90), FreqLoca = 20, MethIdx = "specEnd", data = subQfqmFlag[[idxNameQf]], CritTime = 0)
      #delete row if last timeBgn and timeEnd is NA
      idxVali <- idxVali[rowSums(is.na(idxVali)) != 2,]
      #if last timeEnd is NA, replce that time to the last time value in data$time
      idxVali$timeEnd <- as.POSIXct(ifelse(is.na(idxVali$timeEnd), subData$time[length(subData$time)], idxVali$timeEnd), origin = "1970-01-01", tz = "UTC")
      
      for (idxAgr in 1:length(idxVali$timeBgn)){
        #idxAgr <- 1
        inpTmp <- data.frame(rtioMoleDryCo2 = subData$rtioMoleDryCo2[idxVali$idxBgn[idxAgr]:idxVali$idxEnd[idxAgr]]) 
        
        #dp01 processing
        tmp[[idxNameQf]][[idxAgr]] <- eddy4R.base::wrap.dp01(
          # assign data: data.frame or list of type numeric or integer
          data = inpTmp
        )
        #units:
        attributes(tmp[[idxNameQf]][[idxAgr]]$mean$rtioMoleDryCo2)$unit <- attributes(data$irgaTurb$rtioMoleDryCo2)$unit
      }#end for each idxAgr
      #report only validation which occured on DateProc
      #assign time window 
      timeMin <- base::as.POSIXlt(paste(DateProc, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      timeMax <- base::as.POSIXlt(paste(DateProc, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      #determine index when timeEnd fall in Date Proc
      idxTime <- which(idxVali$timeEnd >= timeMin &  idxVali$timeEnd <= timeMax)
      if (!is.na(idxTime)){
        #report data
        rpt[[idxNameQf]] <- tmp[[idxNameQf]][[idxTime]]
        #report time

        rpt[[idxNameQf]]$timeBgn <- data.frame(rtioMoleDryCo2 = idxVali$timeBgn[idxTime])
        rpt[[idxNameQf]]$timeEnd <- data.frame(rtioMoleDryCo2 = idxVali$timeEnd[idxTime])
      }
    }#end for at least there is one measurement
  }#end of each qf in nameQf

}