#' @title Wrapper Function to read threshold table from CI-Parameter-Repo

#' @author 
#' Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}
#' David Durden \email{ddurden@battelleecology.org}

#' @description Workflow to read ECSE threshold table from CI-Parameter-Repo 

#' @param DpName Character: Name of data product. [-]
#' @param DirInp Character: Input directory. [-]
#' @param DirOut Character: Output directory. [-]
#' @param FileWrte Logical: Determines if file is writing out. Default to FALSE. [-]

#' @return A dataframe consisting of the threshold values to be used for the provided site.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @keywords thresholds, QAQC, plausibility

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Natchaya P-Durden (2017-02-14)
#     original creation
#   Dave Durden (2017-06-18)
#     modularizing and adapting for ECTE & ECSE
#   Dave Durden (2017-11-23)
#     modularizing the code to grab all thresholds passed in the data.frame
#   Dave Durden (2017-11-25)
#     building a function that can be called by workflow
#   Natchaya P-Durden (2018-03-28)
#     updated the function header
#   Ke Xu (2018-04-19)
#     applied term name convention; replaced dataIn by dataInp
##############################################################################################
wrap.para.thsh <- function(
  DpName = c("IrgaTurb","MfcSampTurb","Soni","Amrs"),
  DirInp = "~/eddy/data/Thresholds_EC/CI-Parameter-Repo/ParaSci/Ecte/Dp0p",
  DirOut = "~/eddy/data/Thresholds_EC/threshold_ecte",
  FileWrte = FALSE
){

library(eddy4R.base)
rlog = Logger.Singleton$new() #class defined in eddy4R.base
rlog$debug("in function wrap.para.thsh(...)")
  
  
#DpName <- c("IrgaTurb","MfcSampTurb","Soni","Amrs")#Only MVP sensors included currently

#DpName <- c("CrdCo2","CrdH2o", "EnvHut", "Irga", "IrgaMfcSamp",
#            "IrgaPresValiRegIn", "IrgaPresValiRegOut", "MfcVali",
#            "Mfm", "PresInlt", "Pump", "TempAirLvl", "TempAirTop")
rpt <- list()


#loop throug data product
for (idxDp in DpName) {
  
#idxDp<- DpName[1] 
  
DirInp00 <- paste0(DirInp,"/",idxDp)
#read in all file list in DirInp
fileList00 <- list.files(path = DirInp00, pattern = ".csv")
#reating full path filenames
fileList01 <- paste(DirInp00,fileList00, sep = "/")
#create a vector of variable names from the filenames
varName <- gsub(".csv","",fileList00)
#read in the data into a list of dataframes
dataList <- lapply(fileList01, read.table, header = T, 
                   stringsAsFactors = F, sep = ",", row.names = NULL)
#using the data names provided with the data to name the lists
names(dataList) <- varName
dataOutList <- list()
lapply(names(dataList), function(x) dataOutList[[x]] <<- eddy4R.base::def.para.thsh(dataInp=dataList[[x]], site="Neon"))
#assign name of data list
names(dataOutList) <- varName

#Remove the CI test spreadsheets from grabbing thresholds
dataOutList <- dataOutList[grep(pattern = "Test",x = names(dataOutList), value = TRUE, invert = TRUE)]

#row bind all dataList into one dataframe
dataOut <- do.call("rbind", dataOutList)
idxOrd <- order(row.names(dataOut))
#Changing the order of the variables to alphabetical order using the index
dataOut <- dataOut[idxOrd,]

#replace first row names charecter to lowercase
varName <- paste(tolower(substr(row.names(dataOut), 1, 1)), substr(row.names(dataOut), 2, nchar(row.names(dataOut))), sep="")

#Convert data types
dataOut <- as.data.frame(sapply(dataOut, type.convert), stringsAsFactors = FALSE)

#Column bind the variable names
dataOut <- cbind(varName=varName, dataOut)
#replace first idxDp charecter to lowercase
idxDp00 <- paste(tolower(substr(idxDp, 1, 1)), substr(idxDp, 2, nchar(idxDp)), sep="")

if(FileWrte == TRUE){
DirOut00 <- paste0(DirOut ,"/")
  
if (dir.exists(DirOut00) == FALSE) dir.create(DirOut00, recursive = TRUE)   

#output file
write.csv(dataOut,paste0(DirOut00, "/", idxDp00, ".csv"),row.names=F,sep=",")
}

#Prepare reporting list
rpt[[idxDp00]] <- dataOut

}

#Return reporting list
return(rpt)
  
}