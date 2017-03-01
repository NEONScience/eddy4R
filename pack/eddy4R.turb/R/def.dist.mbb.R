##############################################################################################
#' @title Definition function: Optimal Window Length for Moving Block Bootstrap (MBB)

#' @author
#' Kenny Pratt \email{eddy4R.info@gmail.com}
 

#' @description 
#' Function defintion. Window Length for MBB

#' @param \code{scalEddy}  A vector containing distances or times and of class "numeric". [m] or [s]
#' @param \code{data} A vector containing the input data. Of class "numeric" or "integer". [user-defined]
#' @param \code{veloXaxs} Instantaneous wind speed. Only supplied when users define scalEddy in terms of time. If provided, of class "numeric", otherwise NULL. [m/s]


#' @return Optimal Length for the Moving Block Bootstrap.

#' @references 
#' S. Salesky, M. Chamecki, N. Dias (2012) Estimating random error in eddy covariance fluxes and other turbulence statistics: the filtering method .  Boundary-Layer Meteorology., 144, 113-135. \cr 


#' @keywords Moving Block Bootstrap, Optimal Window Length

#' @examples Currently none

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Kenny Pratt(2017-02-28)
#     original creation
##############################################################################################
def.dist.mbb <- function(
  data
) {

# THIS IS THE RIGOROUS WAY TO COMPUTE b_opt
require(EMD)
#calculate auto-correlation function
lag <- 10; crit <- 1; 
while(crit > 0) {
  lag <- lag * 2
  rptCorr <- stats::acf(data, lag.max = lag, type = "correlation", plot = FALSE, na.action = na.fail, demean = TRUE)
  crit <- base::min(rptCorr$acf)
  
}

#Ensure that length of autocorrelation function is at least twice as large as zero crossing
rptCorr <- stats::acf(data, lag.max = 2.1*lag, type = "correlation", plot = FALSE, na.action = na.fail, demean = TRUE)
#data needs have extrema, otherwise it will not identify the zero crossing
#hence attaching sin(1:10) to the end
posZero <- EMD::extrema(y=c(rptCorr$acf, sin(1:10)))$cross[1,2]

m = posZero
M = 2*m
k_m = seq(-M,M)/M

for (ii in 1:length(k_m)){
  
  if (abs(k_m[ii]) < .5) {
    
    lambda[ii] = 1
    
  } else
    
    lambda[ii] = 2*(1-abs(k_m[ii]))
  
}

twoSidedAcf = c(rptCorr$acf[M:1],1,rptCorr$acf[1:M])

hzero = base::sum(lambda*twoSidedAcf)
D = (4/3)*hzero^2
H = base::sum(lambda*abs(seq(-2*posZero,2*posZero))*twoSidedAcf)

bopt <- (((2*H^2)/D)^(1/3))*length(data)^(1/3)

return(bopt)

}