#http://www.mail-archive.com/r-help@r-project.org/msg85070.html:
#This is also called "Deming regression" and perhaps many other things.
#It is a lively topic in the validation of competing assay methods in the
#laboratory.  I have a function 'deming.R' that does a generalized form
#of this, based on the (very nice) article below, the code is attached.
#(The attachments will be stripped by R-help, but the original requestor
#will get them.)  If someone thinks this to be of general enough interest
#to package up on CRAN I'm happy donate the code to them -- I won't have
#the time for some while.
#   Terry T.
#BD Ripley and M Thompson, Regression techniques for the detection
#of analytical bias, Analyst 112:377-383, 1987.


# Generalized Deming regression, based on Ripley, Analyst, 1987:377-383.
#
eiv <- function(
  x,
  y,
  xstd,
  ystd,
  jackknife=T,
  dfbeta=F,
  scale=T,
  intercept=F
){
  Call <- match.call()
  n <- length(x)
  p2 <- ifelse(intercept == F, 1, 2)	#degrees of freedum in model
  if (length(y) !=n) stop("x and y must be the same length")
  if (length(xstd) != length(ystd)) 
    stop("xstd and ystd must be the same length") 
  
  # Do missing value processing
  nafun <- get(options()$na.action)
  if (length(xstd)==n) {
    tdata <- nafun(data.frame(x=x, y=y, xstd=xstd, ystd=ystd))
    x <- tdata$x
    y <- tdata$y
    xstd <- tdata$xstd
    ystd <- tdata$ystd
  } else {
    tdata <- nafun(data.frame(x=x, y=y))
    x <- tdata$x
    y <- tdata$y
    if (length(xstd) !=2) stop("Wrong length for std specification")
    xstd <- xstd[1] + xstd[2]*x
    ystd <- ystd[1] + ystd[2] * y
  }
  
  if (any(xstd <=0) || any(ystd <=0)) stop("Std must be positive")
  
  #optimality criteria for fit through zero
  minfun0 <- function(beta, x, y, xv, yv) {
    w <- 1/(yv + beta^2*xv)
    alphahat <- 0  #constrain to zero
    sum(w*(y-(alphahat + beta*x))^2)
  }
  
  #optimality criteria for free regression
  minfun <- function(beta, x, y, xv, yv) {
    w <- 1/(yv + beta^2*xv)
    alphahat <- sum(w * (y - beta*x))/ sum(w)
    sum(w*(y-(alphahat + beta*x))^2)
  }
  
  #function for slope calculation
  afun <-function(beta, x, y, xv, yv) {
    w <- 1/(yv + beta^2*xv)
    sum(w * (y - beta*x))/ sum(w)
  }
  
  #actual fit
  fit  <- optimize(minfun, c(.1, 10), x=x, y=y, xv=xstd^2, yv=ystd^2)
  coef <- c(intercept=afun(fit$minimum, x, y, xstd^2, ystd^2), slope=fit$minimum)
  
  #switch between free and constrained regression
  if(intercept == F) {
    fit0 <- optimize(minfun0, coef[2]*c(.5, 1.5), x=x, y=y, xv=xstd^2, yv=ystd^2)
    coef <- c(intercept=0, slope=fit0$minimum)
  }
  
  #calculate scale
  #weights (in units Y^2)     
  w <- 1 / (ystd^2 + (coef[2] * xstd)^2)
  
  #imputed "true" value of x
  u <- w * (ystd^2 * x + xstd^2 * coef[2] * (y-coef[1]))
  
  #fitted values (imputed "true" value of y)
  fit <- coef[1] + coef[2] * u 
  
  
  #non-scaled residuals
  #      print(sqrt(mean((x-u)^2 + (y-fit)^2)))
  
  
  if (is.logical(scale) & scale) {
    
    #scaled residuals in x, y and combined (only makes sense if X and Y carry the same units!)
    err1 <- (x - u) / xstd
    err2 <- (y - fit) / ystd
    #sigma <- sum(err1^2 + err2^2) / (n-p2)
    
    #weighted sum of squares / R-square
    #http://tolstoy.newcastle.edu.au/R/help/06/08/33441.html
    #in lm: values in weights being inversely proportional to the variances
    #in weighted regression, u and fit beeing the best guess of the 'true' X and Y value
    
    #in residuals
    SSEx <- sum(err1^2) 
    SSEy <- sum(err2^2)
    SSE <- SSEx + SSEy
    
    
    ##------------------------------
    ##INSERTION
    
    #percentage error
    #unweighted
    whr <- 
      1:length(x)
    #	      which(abs(x) > .20 & abs(y) > .20)
    err1n <- ((x - u) / u)[whr]
    err2n <- ((y - fit) / fit)[whr]
    SSExn <- sum(err1n^2) 
    SSEyn <- sum(err2n^2)
    SSEn <- SSExn + SSEyn
    
    #	      SSEn <- sum(rowMeans(cbind(err1n^2, err2n^2)))
    RSEn <- sqrt(SSEn / (2*n-p2)) * 100
    #	      RSEn <- sqrt(median(c(err1n^2, err2n^2))) * 100
    mema <- eddy4R.base::def.med.mad(test=c(x[whr], y[whr]), refe=c(u[whr], fit[whr]), Perc=T)
    MADn <- sqrt(mema[1]^2 + mema[2]^2)
    rm(err1n, err2n, mema, SSExn, SSEyn, SSEn)
    
    #weighted
    err1n <- (x - u) / u / xstd
    err2n <- (y - fit) / fit / ystd
    SSExn <- sum(err1n^2) 
    SSEyn <- sum(err2n^2)
    SSEn <- SSExn + SSEyn
    RSEnw <- sqrt(SSEn / (sum(w) * (n-p2)/n)) * 100
    rm(err1n, err2n, SSExn, SSEyn, SSEn)
    
    ##------------------------------
    
    
    #in variables
    if(intercept == T) {
      m <- sum(w * u   / sum(w))	#weighted average (must use same weights as in regression fct.)
      SSFx <- sum(((u   - m) / xstd)^2)
      m <- sum(w * fit / sum(w))	#weighted average
      SSFy <- sum(((fit - m) / ystd)^2)
    } else {
      SSFx <- sum((u / xstd)^2)
      SSFy <- sum((fit / ystd)^2)
    }
    SSF <- SSFx + SSFy
    
    #R-square
    RSQ <- SSF / (SSF + SSE)
    
    #F-statistic
    #degrees of freedom between models
    DFnum <- 1
    #degrees of freedom of sample size
    DFden <- n - p2
    #numerator
    num <- SSF / DFnum
    #denominator
    den <- SSE / DFden
    #value of F-statistic
    fsta <- num / den
    #false-rejection probability
    pval <- pf(fsta, DFnum, DFden, lower.tail=FALSE)
    #prepare output
    FSTA <- c(fsta, DFnum, DFden, pval)
    names(FSTA) <- c("value", "numdf", "dendf", "p-value")
    
    #scaled residuals after Ripley (the same):
    r <- (y - (coef[1] + coef[2]*x)) * sqrt(w)
    SSE <- sum(r^2)
    sigma <- SSE / (n-p2)
    
    #residual standard error
    RSE <- sqrt(SSE / (sum(w) * (n-p2)/n))
    
  } else sigma <- scale^2
  
  ##Z-test for significance: 5 % significance rejects values outside [-1.96, 1.96]
  ##what does that do, how does it work, how to interpret?
  
  #weighted mean of x
  x_tilde <- sum(w*x) / sum(w)
  
  #test for a=0
  test2 <- coef[1] * sqrt(sum(w*x^2) / sum(w*(x-u)^2) / sigma)
  #test2 <- coef[1] / sqrt(sum(w*x^2) / sum(w) * sum(w * (x - x_tilde)))	#following Ripley
  
  #test for beta=1
  test1 <- (coef[2] - 1) * sqrt(sum(w * (x-u)^2) / sigma)
  #test1 <- (coef[2] - 1) * sqrt(sum(w * (x - x_tilde)))	#following Ripley
  
  rlist <- list(coefficients=coef, test1=test1, test0=test2, scale=sigma,
                err1=err1, err2=err2, u=u, r=r, MADn=MADn, SSE=SSE, RSE=RSE, RSEn=RSEn, RSEnw=RSEnw, RSQ=RSQ, FSTA=FSTA, weights=w, fitted.values=fit)
  
  if (jackknife) {
    delta <- matrix(0., nrow=n, ncol=2)
    if(intercept == F) {
      for (i in 1:n) {
        fit <- optimize(minfun0, c(.5, 1.5)*coef[2], 
                        x=x[-i], y=y[-i], xv=xstd[-i]^2, yv=ystd[-i]^2)
        delta[i,] <- coef - c(0, fit$minimum)
      }
    } else {
      for (i in 1:n) {
        fit <- optimize(minfun, c(.5, 1.5)*coef[2], 
                        x=x[-i], y=y[-i], xv=xstd[-i]^2, yv=ystd[-i]^2)
        ahat <- afun(fit$minimum, x[-i], y[-i], xstd[-i]^2, ystd[-i]^2)
        delta[i,] <- coef - c(ahat, fit$minimum)
      }
    }
    rlist$variance <- t(delta) %*% delta
    if (dfbeta) rlist$dfbeta <- delta
  }
  rlist$call <- Call
  class(rlist) <- 'eiv'
  rlist
}


print.eiv <- function(x=rlist, ...) {
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  if (is.null(x$variance)) {
    table <- matrix(0., nrow=2, ncol=3)
    table[,1] <- x$coefficients
    table[,2] <- c(x$test0, x$test1)
    table[,3] <- pnorm(-2*abs(table[,2]))
    dimnames(table) <- list(c("Intercept", "Slope"),
                            c("Coef", "z", "p"))
  }
  else {
    table <- matrix(0., nrow=2, ncol=4)
    table[,1] <- x$coefficients
    table[,2] <- sqrt(diag(x$variance))
    table[,3] <- c(x$test0, x$test1)
    table[,4] <- pnorm(-2*abs(table[,3]))
    dimnames(table) <- list(c("Intercept", "Slope"),
                            c("Coef", "se(coef)", "z", "p"))
  }
  print(table, ...)
  cat("\n   Normalized median residual standard error [%]: ", format(x$MADn, ...), "\n")
  cat("\n   Weighted adjusted R-squared: ", format(x$RSQ, ...), "\n")
  cat("\n   p-value: ", format(x$FSTA[4], ...), "\n")
  invisible(x)
}