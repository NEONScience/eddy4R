rm(list=ls())
#clear Consoule
cat("\014")

#source('~/eddy/docker/ecseDataProc/def.conv.thrs.ecse.R')
DirIn <- "~/eddy/data/Thresholds_EC/CI-Parameter-Repo/ParaSci/Ecte/Dp0p"


dpName <- c("Irga","IrgaMfcSamp","Soni","SoniAmrs")#Only MVP sensors included currently

#dpName <- c("CrdCo2","CrdH2o", "EnvHut", "Irga", "IrgaMfcSamp",
#            "IrgaPresValiRegIn", "IrgaPresValiRegOut", "MfcVali",
#            "Mfm", "PresInlt", "Pump", "TempAirLvl", "TempAirTop")
DirOut <- "~/eddy/data/Thresholds_EC/threshold_ecte"

DirOut00 <- paste0(DirOut ,"/")

if (dir.exists(DirOut00) == FALSE) dir.create(DirOut00, recursive = TRUE)

#loop throug data product
for (idxDp in dpName) {
  
#idxDp<- dpName 
  
DirIn00 <- paste0(DirIn,"/",idxDp)
#read in all file list in DirIn
fileList00 <- list.files(path = DirIn00, pattern = ".csv")
#reating full path filenames
fileList01 <- paste(DirIn00,fileList00, sep = "/")
#create a vector of variable names from the filenames
varName <- gsub(".csv","",fileList00)
#read in the data into a list of dataframes
dataList <- lapply(fileList01, read.table, header = T, 
                   stringsAsFactors = F, sep = ",", row.names = NULL)
#using the data names provided with the data to name the lists
names(dataList) <- varName
dataOutList <- list()
lapply(names(dataList), function(x) dataOutList[[x]] <<- def.conv.thrs.ecse(dataIn=dataList[[x]], site="Neon"))
#assign name of data list
names(dataOutList) <- varName

#row bind all dataList into one dataframe
dataOut <- do.call("rbind", dataOutList)
idxOrd <- order(row.names(dataOut))
#Changing the order of the variables to alphabetical order using the index
dataOut <- dataOut[idxOrd,]
#replace first row names charecter to lowercase
row.names(dataOut) <- paste(tolower(substr(row.names(dataOut), 1, 1)), substr(row.names(dataOut), 2, nchar(row.names(dataOut))), sep="")
dataOut <- cbind(varName=row.names(dataOut), dataOut)
#replace first idxDp charecter to lowercase
idxDp00 <- paste(tolower(substr(idxDp, 1, 1)), substr(idxDp, 2, nchar(idxDp)), sep="")
#output file
write.csv(dataOut,paste0(DirOut00, "/", idxDp00, ".csv"),row.names=F,sep=",")

}

