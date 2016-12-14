##############################################################################################
#' @title Concatenate L0+ data downloaded from CI data extractor

#' @author 
#' Cove Sturtevant \email{csturtevant@neoninc.org}

#' @description 
#' Definition function. Concatenate the 2-hour files output into individual folders from CI's data exporter and 
#' regularized the time series using the default method used by NEON cyberinfrastructure

#' @param \code{idDp} Character string containing full data product ID (e.g. "NEON.DOM.SITE.DP0.00004.001.00474.000.035.000")
#' @param \code{timeRng} A 2-element vector of POSIX format date-time in UTC, where the first (second) element is 
#' the starting (ending) time of the downloaded data as it was extracted, and the second element is the ending
#' time. Time bracket is open on the right - i.e. doesn't contain the ending time. 
#' @param \code{Freq} Single numeric value. The expected data frequency [Hz]
#' @param \code{DirMain} A character string containing the main directory path in which are located the individual 2-hour folders

#' @references 
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16. \cr

#' @return A data from of:\cr
#' \code{time} a POSIX time vector corresponding with the points in \code{data}
#' \code{data} the concatenated data, with NA values where no matching data was found

#' @keywords NEON, SOM, extract, L1, L0

#' @examples
#' DirMain <- "C:/test" # Directory where the folders for each 2-hour block of data reside. NO ending slash.
#' timeRng <- c("2016-09-20T18:00Z","2016-09-27T20:00Z") # Date range of data to grab [min,max). Format e.g. 2016-06-01T00:00Z
#' timeRng <- base::as.POSIXct(timeRng,tz="GMT",format="%Y-%m-%dT%H:%MZ")
#' Freq <- 1/60/2 # Hz - expected data frequency
#' idDp <- "NEON.D10.CPER.DP1.00001.001.00340.000.030.002"
#' data <- def.ccat.dp.sbox(idDp=idDp,timeRng=timeRng,Freq=Freq,DirMain=DirMain)


#' @seealso def.extr.neon.dp.sbox.R, flow.extr.neon.dp.sbox.R

# changelog and author contributions / copyrights
#   Cove Sturtevant (2016-07-15)
#     original creation
#   Cove Sturtevant (2016-12-09)
#     adjustment to accomodate all DP levels
#   Cove Sturtevant (2016-12-14)
#     Turned worklow into a definition function to be called within flow.extr.neon.dp.sbox
##############################################################################################
def.ccat.dp.sbox <- function(idDp,timeRng,Freq,DirMain){
  
# Get a listing of all the folders in the directory (folder names are dates)
listFldr <- base::dir(DirMain)

# Convert dates to POSIXct (1/5 size of POSIXlt)
timeFldr <- base::as.POSIXct(listFldr,tz="GMT",format="%Y-%m-%dT%H_%M_%SZ")

# Find folders containing data within our chosen date range
posFldr <- base::which((timeFldr >= timeRng[1]) & (timeFldr < timeRng[2]))

# Intialize the data output 
time <-  base::as.POSIXct(base::seq.POSIXt(from=base::trunc.POSIXt(timeRng[1],units="secs"),
                                         to=timeRng[2]-1/Freq,by=1/Freq)) # time series in POSIXct
data <- base::matrix(data=NA,nrow=length(time),ncol=1) # initialize

# Do a run through all the folders and grab the data
base::print(base::paste("Reading",base::as.character(1),"of",base::as.character(max(posFldr)),collapse=" "))
for (idxFldr in posFldr) {
  # Show progress
  if((idxFldr %% 50) == 0) {
    base::print(base::paste("Reading",base::as.character(idxFldr),"of",base::as.character(max(posFldr)),collapse=" "))
  }
  
  # Contruct the full file name, including path
  nameFile <- base::paste0(DirMain,"/",listFldr[idxFldr],"/",idDp,".csv",collapse="")
  
  # Open the file, regularize the data, and put it away
  dataIdx <- base::try(utils::read.csv(file=nameFile,header=TRUE),silent=TRUE)
  if(base::class(dataIdx) != "try-error") {
    tsMeas <- base::strptime(dataIdx$MEAS_RDOT_STRT_DATE,format="%d-%b-%y %I.%M.%OS %p",tz="GMT")
    
    # Make sure there is data
    if(base::length(tsMeas) == 0) {next}
    
    # Regularize time series
    dataRglr <- eddy4R.base::def.rglr(timeMeas=as.POSIXlt(tsMeas),dataMeas=data.frame(data=dataIdx$N_VAL),FreqRglr=Freq,MethRglr="cybiDflt")
    
    # Find where this data goes in the overall output 
    posBgn <- base::which(time == dataRglr$timeRglr[1])
    
    # Put it away
    data[posBgn:(posBgn+length(dataRglr$timeRglr)-1)] <- dataRglr$dataRglr[[1]]
  }
}

return(data.frame(time=time,data=data[,1]))

} # End function

