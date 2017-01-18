##############################################################################################
#' @title Definition function: irga flags for LI72000

#' @author 
#' Dave Durden \email{ddurden@battelleecology.org}

#' @description 
#' Definition function to interpret the irga sensor flags from \code{diag01}.
#' @param \code{diag01} The 32-bit diagnostic stream that is output from the LI7200. 

#' @return A dataframe (\code{qfIrga}) of sensor specific irga quality flags as described in NEON.DOC.000807.

#' @references NEON.DOC.000807, Licor LI7200 reference manual

#' @keywords NEON, irga, qfqm

#' @examples 
#' diag01 <- rep(8191, 72000)
#' def.qf.irga(diag01 = diag01)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Dave Durden (2017-01-15)
#     original creation
##############################################################################################


def.qf.irga <- function(diag01){

  if(base::is.null(diag01)) {
    stop("Input 'diag01' is required")
  } 

  if(!(base::is.integer(diag01)|is.numeric(diag01))) {
  stop("Input 'diag01' is required as an integer or numeric")
  } 
 
# Turn the diag01 into a matrix of 32 bits separated into columns for the timeseries of diagnostic values  
qfIrga <- t(sapply(diag01,function(x){ as.integer(intToBits(x))}))

# Function to aggregate bits to base 10 representation
bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}

#Calculate the IRGA AGC value based on the first 4 bits (0-3) of the binary
qfIrgaAgc <- sapply(seq_len(nrow(qfIrga)), function(x) (bitsToInt(qfIrga[x,1:4])*6.67)/100)

#Create outpu dataframe
qfIrga <- data.frame(qfIrgaAgc, qfIrga[,5:13])


#Provide column names to the output
names(qfIrga) <- c("qfIrgaAgc", "qfIrgaSync", "qfIrgaPll", "qfIrgaChop","qfIrgaDetc", "qfIrgaPres", "qfIrgaAux", "qfIrgaTempIn", "qfIrgaTempOut", "qfIrgaHead")


#Change all the 1 values to 0 and 0 to 1 to fit the NEON qfqm framework
lapply(names(qfIrga[!names(qfIrga) == "qfIrgaAgc"]), function(x) {
  pos <- which(qfIrga[,x] == 1)
  qfIrga[pos,x] <<- as.integer(0)
  qfIrga[-pos,x] <<- as.integer(1)
  qfIrga[,x] <<- as.integer(qfIrga[,x])
  })

#return dataframe
return(qfIrga)

}
