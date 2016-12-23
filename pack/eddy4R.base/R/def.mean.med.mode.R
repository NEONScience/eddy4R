##############################################################################################
#' @title Definition function: Calculate descriptive statistics based on arithmetic mean, median and mode

#' @author 
#' Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' Natchaya P-Durden

#' @description Calculate descriptive statistics based on arithmetic mean, median and mode.

#' @param \code{test} A vector containing the test data. Of class "numeric" or "integer", and of the same length as \code{refe}. [same units as reference data]
#' @param \code{refe} A vector containing the reference data. Of class "numeric" or "integer". Defaults to \code{refe = 0}. [same units as test data]
#' @param \code{Perc} Report results in the same units as \code{refe} and \code{test} (\code{Perc = FALSE}) or as percentages (\code{Perc = TRUE})? Of class "logical", defaults to \code{Perc = FALSE}.

#' @return \code{statLoc} A list object of class "numeric" [1, 1:3] containing mean, median, and mode. \cr
#' @return \code{statDis} A list object of class "numeric" [1, 1:3] containing standard deviation, median absolute deviation, and mode absolute deviation. \cr
#' @return \code{NumSamp} Sample size which not included NaNs. \cr

#' @references
#' License: Terms of use of the NEON FIU algorithm repository dated 2015-01-16 \cr
#' Croux, C., and Rousseeuw, P. J.: Time-efficient algorithms for two highly robust estimators of scale, Computational Statistics, 1, 411-428, 1992. \cr
#' http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode \cr

#' @keywords descriptive statistics

#' @examples 
#' #works
#' def.mean.med.mode(test = rnorm(100))
#' 
#' #returns error message
#' def.mean.med.mode(test = rnorm(10), refe = 0, Perc = TRUE)

#' @seealso
#' \code{\link[eddy4R.base]{def.mode}}

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2015-11-21)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Natchaya P-Durden (2016-02-22)
#     Initail naming convention for eddy4R
##############################################################################################
#calculate descriptive statistics based on arithmetic mean, median and mode

def.mean.med.mode <- function(test, refe = 0, Perc = FALSE) {
  
  #stop if parameter combination not 
  if(refe == 0 & Perc == TRUE) stop("Cannot calculate percentage when refe = 0.")
    
  #assign list for results
    rpt <- list()
    rpt$statLoc <- list()
    rpt$statDis <- list()
    
  #sample size (omitting NAs) for sample size correction
    if(length(refe) == 1) rpt$NumSamp <- length(which(!is.na(test)))
    if(length(refe) >  1) rpt$NumSamp <- length(which(!is.na(refe) & !is.na(test)))
  
  #calculate differences
    if(Perc == FALSE) diff <- test - refe
    if(Perc == TRUE)  diff <- (test - refe) / abs(refe) * 100
    
  #calculate measures of location
    #arithmetic mean
    rpt$statLoc$mean <- mean(diff)
    
    #median
    rpt$statLoc$med  <- stats:::median(diff, na.rm = TRUE)
    
    #mode
    rpt$statLoc$mode <- eddy4R.base:::def.mode(x = diff)
  
  #calculate measures of dispersion
    #standard deviation
    rpt$statDis$sd <- sd(diff)
    
    #median absolute deviation incl. scale factor ot 1 sd and sample size correction after Croux (1992)
    rpt$statDis$madMed <- stats:::mad(diff, na.rm = TRUE) * rpt$NumSamp / (rpt$NumSamp - 0.8)
    
    #mode absolute deviation incl. scale factor to 1 sd and sample size correction (derived in appendix)
    rpt$statDis$madMod <- eddy4R.base:::def.mode(abs(diff - rpt$statLoc$mod)) * (3.71 - 0.99 * log10(rpt$NumSamp) + 0.31 * log10(rpt$NumSamp)^2)

  #prepare output
    rpt$statLoc <- data.frame(rpt$statLoc)
    rpt$statDis <- data.frame(rpt$statDis)
    if(Perc == TRUE) {
      dimnames(rpt$stata$loc)[[2]] <- paste(dimnames(rpt$statLoc)[[2]], "%", sep="")
      dimnames(rpt$statDis)[[2]] <- paste(dimnames(rpt$statDis)[[2]], "%", sep="")
    }
    
  #return output
    return(rpt)
  
# end function def.mean.med.mode
}

# ##############################################################################################
# #Appendix: Derivation of mode absolute deviation (i) scale factor to 1 sigma and (ii) sample-size correction
# #analogous to Croux (1992) for median absolute deviation
# 
#   #definitions
#     #define sample size
#     sizeSamp <- sapply(seq(from = 1, to = 6, by = 0.01), function(x) 10^x)
#     
#     #number of repetitions for each sample size
#     reps <- 1
#   
#   #calculations
#     #calculate standard deviation and mode absolute deviation for normal distribution with mean = 0 and sd = 1
#     rpt <- lapply(rep(sizeSamp, reps), function(x) def.mean.med.mode(test = rnorm(x), refe = 0, Perc = FALSE))
#       
#     #combine results from repetitions into single list
#     name <- names(rpt[[1]])
#     rpt <- lapply(name, function(x) do.call(rbind, lapply(rpt, "[[", x)))
#     names(rpt) <- name; rm(name)
#   
#   #regression
#     #assign data  
#     idep <- log10(rpt$NumSamp[,1])
#     depe <- rpt$statDis$sd / rpt$statDis$madMod
#   
#     #overview plot
#     plot(depe ~ idep, ylim = c(0, max(depe)))
#     abline(h=1, lty=2)
#   
#     #determine coefficients via least-squares regression
#     LM <- lm(depe ~ idep + I(idep^2))
#     summary(LM)
#     lines(LM$fitted.values ~ idep, col=5)
#   
#     #regression results:
#     #(i) Intercept provides scale factor to 1 sigma
#     #(ii) 1st and 2nd order coefficients provide sample-size correction
#   #   Coefficients:
#   #     Estimate Std. Error t value Pr(>|t|)    
#   #   (Intercept)  3.71969    0.18212  20.425   <2e-16 ***
#   #     idep        -0.99409    0.11513  -8.635   <2e-16 ***
#   #     I(idep^2)    0.31366    0.01619  19.368   <2e-16 ***
#   #     ---
#   #     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#   #   
#   #   Residual standard error: 0.6823 on 498 degrees of freedom
#   #   Multiple R-squared:  0.879,	Adjusted R-squared:  0.8786 
#   #   F-statistic:  1810 on 2 and 498 DF,  p-value: < 2.2e-16
#   
