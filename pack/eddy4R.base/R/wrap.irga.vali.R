##############################################################################################
#' @title Wrapper function: Validation processing for IRGA

#' @author
#' Natchaya P-Durden \email{eddy4R.info@gmail.com}

#' @description Wrapper function to apply IRGA validation.

#' @param data List consisting of \code{ff::ffdf} file-backed objects containing the dp0p input IRGA.
#' @param qfqmFlag List consisting of \code{ff::ffdf} file-backed objects containing the IRGA quality flags.
#' @param gasRefe List containing the values of the reference gases. [mol mol-1] 
#' @param DateProc A vector of class "character" containing the processing date.

#' @return 
#' The returned object consistes of descriptive statistics (mean, min, max, vari, numSamp, se) of CO2 dry mole concentration during performing validation and CO2 dry mole concentration of reference gases.

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
#   Natchaya P-Durden (2018-11-27)  
#     including the reference gases into the output 
##############################################################################################

wrap.irga.vali <- function(
  data,
  qfqmFlag,
  gasRefe,
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
  rptTmp <- list()
  rpt <- list()
  
  #statistical names (will be used when no validation occured at all)
  NameStat <- c("mean", "min", "max", "vari", "numSamp", "se")
  
  for (idxNameQf in nameQf){
    #idxNameQf <- nameQf[2]
    #preparing the qfIrgaTurbValiGas01 to 05 data for def.idx.agr()
    #replace NA to the qf which are not equal to 1
    subQfqmFlag[[idxNameQf]] <- ifelse(subQfqmFlag[[idxNameQf]] != 1, NA, subQfqmFlag[[idxNameQf]])
    
    #determine when validation occur
    #if there is at least one measurement
    if(length(which(!is.na(subQfqmFlag[[idxNameQf]]))) > 0){
      #determine the beginning and ending indicies of each validation
      idxVali <- eddy4R.base::def.idx.agr(time = subData$time, PrdAgr = 90, FreqLoca = 20, MethIdx = "specEnd", data = subQfqmFlag[[idxNameQf]], CritTime = 0)
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
      }#end for each idxAgr
      #report only validation which occured on DateProc
      #assign time window 
      timeMin <- base::as.POSIXlt(paste(DateProc, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      timeMax <- base::as.POSIXlt(paste(DatePost, " ", "00:01:29.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      #determine index when timeEnd fall in Date Proc
      idxTime <- which(idxVali$timeEnd >= timeMin &  idxVali$timeEnd < timeMax)
      if (!is.na(idxTime)){
        #report data
        rptTmp[[idxNameQf]] <- tmp[[idxNameQf]][[idxTime]]
        #report time
        rptTmp[[idxNameQf]]$timeBgn <- data.frame(rtioMoleDryCo2 = idxVali$timeBgn[idxTime])
        rptTmp[[idxNameQf]]$timeEnd <- data.frame(rtioMoleDryCo2 = idxVali$timeEnd[idxTime])
        #units:
        attributes(rptTmp[[idxNameQf]]$mean$rtioMoleDryCo2)$unit <- attributes(data$irgaTurb$rtioMoleDryCo2)$unit
      } else {
        for(idxStat in NameStat){
          #report data
          rptTmp[[idxNameQf]][[idxStat]] <- data.frame(rtioMoleCo2 = NaN)
          #report time
          rptTmp[[idxNameQf]]$timeBgn <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateProc, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
          rptTmp[[idxNameQf]]$timeEnd <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateProc, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
          #units:
          attributes(rptTmp[[idxNameQf]]$mean$rtioMoleDryCo2)$unit <- attributes(data$irgaTurb$rtioMoleDryCo2)$unit
        }#end idxStat
        
      }#end idxTime
      #end for at least there is one measurement
    } else {
      for(idxStat in NameStat){
        #report data
        rptTmp[[idxNameQf]][[idxStat]] <- data.frame(rtioMoleDryCo2 = NaN)
      }#end idxStat
      #report time
      rptTmp[[idxNameQf]]$timeBgn <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateProc, " ", "00:00:00.000", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
      rptTmp[[idxNameQf]]$timeEnd <- data.frame(rtioMoleDryCo2 = base::as.POSIXlt(paste(DateProc, " ", "23:59:59.950", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
    }
  }#end of each qf in nameQf
  
  #return results as dataframe
  outTmp00 <-list()
  outTmp01 <- list()
  for (idxGas in names(rptTmp)){
  #for (idxGas in c("qfIrgaTurbValiGas02", "qfIrgaTurbValiGas03", "qfIrgaTurbValiGas04")){
    #idxGas <- names(rptTmp)[2]
    for (idxStat in names(rptTmp[[idxGas]])){
      #idxStat <- names(rptTmp[[idxGas]])[1]
      outTmp00[[idxStat]] <- data.frame(rptTmp[[idxGas]][[idxStat]]$rtioMoleDryCo2)
     }
    outTmp01[[idxGas]] <- do.call(cbind, outTmp00)
  }
  rpt$rtioMoleDryCo2Vali <- do.call(rbind, outTmp01)
  
  #remove
  rm(outTmp00, outTmp01, rptTmp)
  
  #assign column names
  colnames(rpt$rtioMoleDryCo2Vali) <- c("mean", "min", "max", "vari", "numSamp", "se", "timeBgn", "timeEnd")
  #remove row names
  rownames(rpt$rtioMoleDryCo2Vali) <- NULL
  
  #add gasRefe values into rpt
  #create temporary dataframe
  tmpGasRefe <- data.frame(matrix(ncol = 1, nrow = 5))
  #assign column name
  colnames(tmpGasRefe) <- "rtioMoleDryCo2Refe"
  #add values to tmpGasRefe
  for (idxRow in 1:nrow(tmpGasRefe)){
    if (idxRow == 2){
      #add zero gas
      tmpGasRefe[idxRow,1] <- 0
    }else{
      #get location in gasRefe
      if (idxRow == 1){
        loc <- idxRow
      } else{
        loc <- idxRow-1
      }
      #test time condition for picking the right value
      if (gasRefe$rtioMoleDryCo2RefeTime01[[DateProc]][[loc]] == gasRefe$rtioMoleDryCo2RefeTime02[[DateProc]][[loc]]){
        tmpGasRefe[idxRow,1] <- gasRefe$rtioMoleDryCo2Refe01[[DateProc]][[loc]]
      } else {
        if (rpt$rtioMoleDryCo2Vali$timeBgn[idxRow] >= gasRefe$rtioMoleDryCo2RefeTime02[[DateProc]][[loc]]){
          tmpGasRefe[idxRow,1] <- gasRefe$rtioMoleDryCo2Refe02[[DateProc]][[loc]]
        } else {
          tmpGasRefe[idxRow,1] <- gasRefe$rtioMoleDryCo2Refe01[[DateProc]][[loc]]
        }
      }
    }
  }# end for loop
 
  #add gasRefe values into rpt
  rpt$rtioMoleDryCo2Vali <- cbind(rpt$rtioMoleDryCo2Vali, tmpGasRefe)
  #reorder column
  rpt$rtioMoleDryCo2Vali <- rpt$rtioMoleDryCo2Vali[,c(1:6, 9, 7, 8)]
  #return results
  return(rpt)
}