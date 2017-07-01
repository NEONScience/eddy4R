##############################################################################################
#' @title Definition function: Soni flags for CSAT3

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function to interpret the Soni (Campbell Sci. CSAT3) sensor flags from \code{diag16}. Flags output are important for the NEON QFQM framework and described in the NEON.DOC.000807. This code was built for flags outlined in firmware version 3 of the CSAT3; however, flag detemination may be transferable to other firmware versions.
#' @param diag16 The 16-bit diagnostic stream that is output from the Soni (CSAT3). 

#' @return A dataframe (\code{qfSoni}) of sensor specific AMRS quality flags as described in NEON.DOC.000807.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000807) \cr
#' Campbell Scientific CSAT3 reference manual

#' @keywords NEON, soni, sonic anemometer, CSAT3, qfqm

#' @examples 
#' diag16 <- as.integer(rep(135, 36000))

#' pos <- runif(20,1, 36000) # inserting error positions for other flags
#' diag16[pos] <- as.integer(c(32768,16384,8192,4096,61442, 61441,61440,61503, -99999, NaN)) # filling with numbers that would indicate flags soni flags
#' 
#' eddy4R.qaqc::def.qf.soni(diag16 = diag16)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-02-08)
#     original creation
#   Dave Durden (2017-05-12)
#     fixing bug in order of qfSoni flags
##############################################################################################


def.qf.soni <- function(diag16){

  if(base::is.null(diag16)) {
    stop("Input 'diag16' is required")
  } 

  if(!(base::is.integer(diag16)|base::is.numeric(diag16))) {
  stop("Input 'diag16' is required as an integer or numeric")
  } 
 
  diag16[base::is.na(diag16)] <- -99999 # Convert NAs or NaNs to -99999
  
# Turn the diag16 into a matrix of 32 bits separated into columns for the timeseries of diagnostic values  
qfSoni <- t(base::sapply(diag16,function(x){ base::as.integer(base::intToBits(x))}))


#Create output dataframe using the flags defined in L0p output documentation (bits 12-15)
qfSoni <- base::data.frame(qfSoni[,c(13:16)])

#Check for base-10 described flags
qfSoniUnrs <- base::as.integer(base::ifelse(diag16 == -99999, 1, 0))
qfSoniData <- base::as.integer(base::ifelse(diag16 == 61503, 1, 0))
qfSoniTrig <- base::as.integer(base::ifelse(diag16 == 61440, 1, 0))
qfSoniComm <- base::as.integer(base::ifelse(diag16 == 61441, 1, 0))
qfSoniCode <- base::as.integer(base::ifelse(diag16 == 61442, 1, 0))

# Create the output data frame
qfSoni <- base::data.frame( qfSoniUnrs,qfSoniData,qfSoniTrig,qfSoniComm, qfSoniCode, qfSoni[,1:4])

#Provide column names to the output
base::names(qfSoni) <- c("qfSoniUnrs", "qfSoniData", "qfSoniTrig", "qfSoniComm", "qfSoniCode", "qfSoniSgnlLow", "qfSoniSgnlHigh", "qfSoniSgnlPoor", "qfSoniTemp")


#return dataframe
return(qfSoni)

}
