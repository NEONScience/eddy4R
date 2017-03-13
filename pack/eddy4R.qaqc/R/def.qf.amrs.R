##############################################################################################
#' @title Definition function: AMRS flags for XSens

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function to interpret the AMRS (Attitude and Reference Motion System) sensor flags from \code{diag32}. Flags output are a reduced set that were deemed important for the NEON QFQM framework and described in the NEON.DOC.000807.
#' @param \code{diag32} The 32-bit diagnostic stream that is output from the XSens AMRS. 
#' @param \code{MethQf} Switch for quality flag determination for the XSens AMRS, diag32 provides ones for passing quality by default the equals "xsen". The "qfqm" method changes the ones to zeros to match the NEON QFQM logic for certain flags that are set high when good, qfAmrsVal & qfAmrsFilt.
#' 
#' @return A dataframe (\code{qfSoniAmrs}) of sensor specific AMRS quality flags as described in NEON.DOC.000807.

#' @references 
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
#' NEON Algorithm Theoretical Basis Document:Eddy Covariance Turbulent Exchange Subsystem Level 0 to Level 0’ data product conversions and calculations (NEON.DOC.000807)
#' XSens AMRS reference manual

#' @keywords NEON, AMRS, qfqm

#' @examples 
#' diag32 <- as.integer(rep(135, 72000))

#' def.qf.amrs(diag32 = diag32)
#' 
#' pos <- runif(15,1, 72000) # inserting error positions for other flags
#' diag32[pos] <- as.integer(c(262279,524423, 1048710)) # filling with numbers that would indicate flags for qfAmrsVal, qfAmrsVelo, and qfAmrsRng

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-02-08)
#     original creation
##############################################################################################


def.qf.amrs <- function(diag32, MethQf = c("qfqm","xsen")[1]){

  if(base::is.null(diag32)) {
    stop("Input 'diag32' is required")
  } 

  if(!(base::is.integer(diag32)|is.numeric(diag32))) {
  stop("Input 'diag32' is required as an integer or numeric")
  } 
 
# Turn the diag32 into a matrix of 32 bits separated into columns for the timeseries of diagnostic values  
qfAmrs <- t(base::sapply(diag32,function(x){ base::as.integer(base::intToBits(x))}))


#Create output dataframe using the flags defined in L0p output documentation
qfAmrs <- base::data.frame(qfAmrs[,c(1,2,18:20)])

# Combine No velocity update flags in to one flag
pos <- which(qfAmrs[,3] == 1 | qfAmrs[,4] == 1) # Save positions of flagged data
qfAmrsVelo <- as.integer(rep(0, nrow(qfAmrs))) # Create initialized vector
qfAmrsVelo[pos] <- as.integer(1) # For positions with flagged data put together

# Create the output data frame
qfAmrs <- base::data.frame(qfAmrs[,c(1:2)], qfAmrsVelo,qfAmrs[,5])

#Provide column names to the output
base::names(qfAmrs) <- c("qfAmrsVal", "qfAmrsFilt", "qfAmrsVelo", "qfAmrsRng")


if (MethQf == "qfqm"){
#Change defined flags with 1 values to 0 and 0 to 1 to fit the NEON qfqm framework
base::lapply(base::names(qfAmrs[,names(qfAmrs) %in% c("qfAmrsVal", "qfAmrsFilt")]), function(x) {
  pos <- which(qfAmrs[,x] == 1)
  qfAmrs[pos,x] <<- base::as.integer(0)
  qfAmrs[-pos,x] <<- base::as.integer(1)
  qfAmrs[,x] <<- base::as.integer(qfAmrs[,x])
  })}

#return dataframe
return(qfAmrs)

}
