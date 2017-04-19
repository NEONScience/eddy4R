def.ecte.data.cmbn <- function(date, site = "CPER", levlTowr) {
  
  
  dateFileIn <- gsub(pattern = "-", replacement = "", x = date)
  
  # Set the working directory
  dirInp <- paste0("~/eddy/data/",site,"/out/dp00/",dateFileIn)
  
  if (dir.exists(dirInp) == FALSE) stop("the input file directory does not exist")
  
  # make sure that fractional seconds can be seen from the console
  options(digits.secs=3)
  
  #Output directory
  dirOut <- paste0("~/eddy/data/L0prime_gold/",site,"/",date)
  
  #Check to see if the directory exists, if not create the directory. Recursive required to write nested file directories
  if (dir.exists(dirOut) == FALSE) dir.create(dirOut, recursive = TRUE)
  
  #Grab names of input files
  fileInp <- list.files(path = dirInp, pattern = ".csv", recursive = TRUE, full.names = TRUE)
  
  # List of dp numbers to dp names
  listDpNum <- list(soni = "00007", soniAmrs = "00010",irga = "00016", irgaMfcSamp = "00012", irgaSndValiNema = "00009")
  
  #Separate the input filenames inot a list by data product
  fileList <- lapply(listDpNum, function(x) {fileInp[grep(pattern = x, x = fileInp)]})
  
  
  
  names(fileList[["soni"]]) <- c("veloXaxs","veloYaxs","veloZaxs","idx","diag","veloSoni")
  
  names(fileList[["soniAmrs"]]) <- c("idx","accXaxs","accYaxs","accZaxs","accXaxsDiff","accYaxsDiff","accZaxsDiff","avelYaxs","avelXaxs","avelZaxs","angYaxs","angXaxs","angZaxs","diag32")
  
  names(fileList[["irga"]]) <- c("idx", "diag01", "tempRefe", "tempIn", "tempOut", "presAtm","presDiff", "powrH2oSamp", "powrH2oRefe", "asrpH2o", "densMoleH2o", "rtioMoleDryH2o", "powrCo2Samp", "powrCo2Refe", "asrpCo2",  "densMoleCo2", "rtioMoleDryCo2", "diag02", "potCool", "ssiCo2", "ssiH2o")
  
  names(fileList[["irgaMfcSamp"]]) <- c("presAtm", "temp", "frt", "frt00", "frtSet00")
  
  names(fileList[["irgaSndValiNema"]]) <- c("qfGas01", "qfGas02", "qfGas03", "qfGas04", "qfGas05")
  
  
  
  dataList <- list()
  
  
  for (idx in names(fileList)){
    
    tmp <- list()
    
    #Read in data from the list of files
    lapply(fileList[[idx]], function(x) tmp[[idx]][[x]] <<- read.table(x, header = T, stringsAsFactors = F, sep = ",", row.names = NULL))
    
    names(tmp[[idx]]) <- names(fileList[[idx]])  
    
    if(idx == "soniAmrs"){Freq <- 40
    } else if(idx == "irgaSndValiNema"){Freq <- 0.2
    } else {Freq <- 20}
    
    timeRglr <- seq.POSIXt(
      from = base::as.POSIXlt(paste(date, " ", "00:00:00.0001", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      to = base::as.POSIXlt(paste(date, " ", "23:59:59.9752", sep=""), format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
      by = 1/Freq
    )
    
    dataList[[idx]] <- lapply(names(tmp[[idx]]), function(x){
      
      # timeTest <- base::as.POSIXlt(dataList[[idx]][[x]][,"time"], format="%Y-%m-%d %H:%M:%OS", tz="UTC")
      # print(head(timeTest)) 
      # perform regularization
      # dataTest[[idx]][[x]] <<- 
      eddy4R.base::def.rglr(
        timeMeas = base::as.POSIXlt(tmp[[idx]][[x]]$time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
        dataMeas = tmp[[idx]][[x]],
        # unitMeas = attributes(dataList$soniAmrs)$unit,
        BgnRglr = as.POSIXlt(min(timeRglr)),
        EndRglr = as.POSIXlt(max(timeRglr)),
        FreqRglr = Freq,
        MethRglr = "CybiEc"
      )$dataRglr$data 
      
    })
    
    names(dataList[[idx]]) <- names(tmp[[idx]])
    
    
    dataList[[idx]] <- as.data.frame(do.call("cbind", dataList[[idx]]))
    
    dataList[[idx]] <- cbind(dataList[[idx]], time = strftime(timeRglr, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
    
    rm(tmp)                   
  }
  
  #Save the dp00 data as an Rdata binary file
  save(dataList, file = paste0(dirOut,"/","ECTE_dp00_",site,"_",date,".RData"))
  
  return(dataList)
  
}