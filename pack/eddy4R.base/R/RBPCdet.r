##############################################################################################
#' @title RMSD, bias, precision and coefficient of determination - incl. deadband

#' @author Stefan Metzger \email{eddy4R.info@gmail.com} \cr
#' @author Hongyan Luo \email{eddy4R.info@gmail.com}

#' @description Function defintion.RMSD, bias, precision and coefficient of determination - incl. deadband.  

#' @param \code{refe}  Variable of class numeric. Reference data.Same unit as test data. 
#' @param \code{test}  Variable of class numeric. Test data.Same unit as reference data.
#' @param \code{perc}  Variable of class logical. It describe if the output is in percentage.
#' @param \code{deba}  Variable of class numeric. Numbers of the deadband around zero denominator
#' @param \code{debaRltv}  Variable of class logical.Absolute or relative (percentage) deadband around zero. True indicates deba in relative (percentage), False indicates deba in absolute numbers. 
#' 

#' @return Currently none

#' @references Currently none
#' license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16.


#' @keywords RMSD, bias, precision, R-square

#' @examples 
#' #refe dataset
#' df1 <- c(runif(200, min=0, max=360))
#' #test dataset
#' df2 <- c(runif(200, min=0, max=360))
#' #run function
#' def.rmsd.diff.prcs.rsq(refe=df1,test=df2,perc = FALSE,deba=NULL, debaRltv=FALSE)

#' @seealso Currently none

#' @export

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-19)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging
#   Hongyan Luo (2016-06-10)
#     adjust to eddy4R terms
#############################################################################################

#RETURNS RESIDUAL STANDARD ERROR AND COEFFICIENT OF DETERMINATION FOR TWO DATA SERIES

RBPCdet2 <- function(
  refe,
  test,
  perc = FALSE,
  db=NULL, #for percentage returns: deadband around zero denominator
  db_rel=FALSE #absolute or relative (percentage) deadband around zero
  ) {
  
  
#omit deadband around zero
  if(!is.null(db)) {
    if(db_rel == TRUE) {
      dbz <- max(abs(refe), na.rm=T) * db / 100
      whr_n <- which(refe > -dbz & refe < dbz)
    } else {
      whr_n <- which(refe > -db & refe < db)
    }
    if(length(whr_n) > 0) { 
      refe <- refe[-whr_n]
      test <- test[-whr_n]
    }      
  }  
  
#sum of squared errors and coefficient of determination
  
  #sum of sqared residuals
    SSres <- sum((test - refe)^2, na.rm=TRUE)
    
  #sum of squared deviations from data series mean
    SSdev <- sum((refe - mean(refe, na.rm=TRUE))^2, na.rm=TRUE)

  #coefficient of determination
    Rsqu <- 1-(SSres/SSdev)
      
#Root mean sququred deviation, bias, precision
  if(perc == FALSE) {
    
  #absoute values
    #RMSD<-sqrt(SSres / length(refe))
    RMSD <- sqrt(mean((test - refe)^2, na.rm=TRUE))
    BIAS <- mean(test - refe, na.rm=TRUE)
    PREC <- sqrt(RMSD^2 - BIAS^2)
    
  } else {
  
  #percentage values  
    RMSD <- sqrt(mean(((test - refe) / refe * 100)^2, na.rm=TRUE))
    BIAS <- mean((test - refe) / refe * 100, na.rm=TRUE)
    PREC <- sqrt(RMSD^2 - BIAS^2)
    
  }

#prepare output
  output <- cbind(RMSD, BIAS, PREC, Rsqu, length(na.omit(test - refe)))
  if(perc == FALSE) dimnames(output)[[2]] <- c("RMSD", "BIAS", "PREC", "RSQ", "N")
  if(perc == TRUE) dimnames(output)[[2]] <- c("RMSD%", "BIAS%", "PREC%", "RSQ", "N")

#return output
  return(output)
    
}



##############################################################################################
#' @title RMSD, bias, precision and coefficient of determination

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2014-11-19)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description RMSD, bias, precision and coefficient of determination.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

#old version without deadband
RBPCdet <- function(ref, test, perc = FALSE) {
  #RETURNS RESIDUAL STANDARD ERROR AND COEFFICIENT OF DETERMINATION FOR TWO DATA SERIES
  #sum of squared errors and coefficient of determination
  #sum of sqared residuals
  SSres <- sum((test - ref)^2, na.rm=TRUE)
  #sum of squared deviations from data series mean
  SSdev<-sum((ref - mean(ref, na.rm=TRUE))^2, na.rm=TRUE)
  #residual standard error
  #    Rmsd<-sqrt(SSres / length(ref))
  if(perc == FALSE) {
    Rmsd<-sqrt(mean((test - ref)^2, na.rm=TRUE))
    BIAS<-mean(test - ref, na.rm=TRUE)
    PREC<-sqrt(Rmsd^2 - BIAS^2)
  } else {
    Rmsd<-sqrt(mean(((test - ref) / ref * 100)^2, na.rm=TRUE))
    BIAS<-mean((test - ref) / ref * 100, na.rm=TRUE)
    PREC<-sqrt(Rmsd^2 - BIAS^2)
  }
  #coefficient of determination
  Rsqu<-1-(SSres/SSdev)
  #return result
  return(c(Rmsd, BIAS, PREC, Rsqu))
}
