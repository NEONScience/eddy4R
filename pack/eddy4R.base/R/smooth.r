##############################################################################################
#' @title Kernel smoother

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-07-23)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Kernel smoother.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

#smooth data with a Kernel smoother or binning
#also check:
#?decimate
#?aggregate

########################################################
#KERNEL SMOOTHER
########################################################
kernsmo <- function(data, spans) {
#http://www.stat.rutgers.edu/home/rebecka/Stat565/lab42007.pdf: We reduce variance by applying a frequency domain smoothing filter. The option spans controls the degree of smoothing. For example; you can apply a long smoothing filter with spans=c(15), or two short filters in succession with spans=c(3,3).

#configure kernel
  kernel <- kernel("modified.daniell", spans)
  #df <- df.kernel(kernel)
  #bandwidth <- bandwidth.kernel(kernel)

#apply kernel smoothing
  if(is.null(ncol(data))) {
    out <- kernapply(data, kernel, circular = TRUE)
  } else {
    for(i in 1:ncol(data)) {
      if(i == 1) out <- kernapply(data[,i], kernel, circular = TRUE)
      if(i > 1)  out <- cbind(out, kernapply(data[,i], kernel, circular = TRUE))
    }
    dimnames(out) <- dimnames(data)
  }

#return results
  return(out)
########################################################
}
########################################################



##############################################################################################
#' @title Bin data

# type (one of function defintion, function wrapper, workflow, demo): function defintion

# license: Terms of use of the NEON FIU algorithm repository dated 2015-01-16

#' @author Stefan Metzger \email{eddy4R.info@gmail.com}

# changelog and author contributions / copyrights
#   Stefan Metzger (2011-07-23)
#     original creation
#   Stefan Metzger (2015-11-28)
#     re-formualtion as function() to allow packaging

#' @description Bin data.

#' @param Currently none

#' @return Currently none

#' @references Currently none

#' @keywords Currently none

#' @examples Currently none

#' @seealso Currently none

#' @export
##############################################################################################

########################################################
#BIN DATA
########################################################
binning <- function(
  IDE,
  DEP,
  MM=NULL,
  bins=23,
  bwd=c("lin", "log10", "exp10", "logE", "expE")[3],
  mfun=c("mean", "median")[1]
) {
#aggregation with binning 
#  IDE: independent variable, frequency, wavenumber etc.
#  DEP: dependent variable, vector or matrix of same length as IDE
#  MM: min and max range
#  bins: number of bins
#  bwd: c("lin", "log10", "exp10", "logE", "expE") bin width distribution as function of IDE

#prepare variables
  ide <- IDE
  dep <- as.matrix(DEP)
  mm <- MM

#define boundary
  if(is.null(MM)) mm <- range(ide)
    if(bwd == "lin") bou <- seq(mm[1], mm[2], length.out=(bins + 1))
    if(bwd == "log10") bou <- log10(seq(10^(mm[1]), 10^(mm[2]), length.out=(bins + 1)))
    if(bwd == "exp10") bou <- 10^(seq(log10(mm[1]), log10(mm[2]), length.out=(bins + 1)))
    if(bwd == "logE") bou <- log(seq(exp(mm[1]), exp(mm[2]), length.out=(bins + 1)))
    if(bwd == "expE") bou <- exp(seq(log(mm[1]), log(mm[2]), length.out=(bins + 1)))
      if(is.null(MM)) bou[c(1,length(bou))] <- c(0,Inf)

#actual binning
  for(i in 1:(length(bou)-1)) {
    whr_bin <- which(ide > bou[i] & ide <= bou[i+1])
      if(mfun == "median") {
	dummy.ide <- median(ide[whr_bin], na.rm=T)
	dummy.dep <- sapply(1:ncol(dep), function(x) median(dep[whr_bin,x], na.rm=T))
      } else {
	dummy.ide <- mean(ide[whr_bin], na.rm=T)
	dummy.dep <- sapply(1:ncol(dep), function(x) mean(dep[whr_bin,x], na.rm=T))
      }
	if(i == 1) {
	  bin.ide <- dummy.ide
	  bin.dep <- dummy.dep
	} else {
	  bin.ide <- c(bin.ide, dummy.ide)
	  bin.dep <- rbind(bin.dep, dummy.dep)
	}
  }
  if(!is.null(MM)) bin.ide <- sapply(1:(length(bou)-1), function(x) mean(bou[x:(x+1)]))

#generate output
  export <- list(bin.ide, bin.dep)
  attributes(export)$names <- c("IDE","DEP")

#return result
  return(export)
########################################################
}
########################################################
