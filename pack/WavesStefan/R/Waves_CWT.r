# Package: Waves
# Version: 0.2
# Date: 2011-10-29
# Title: TU Bayreuth Wavelet Analysis
# Author@R: c(person("Baltasar", "Trancon y Widemann", email =
        # "Baltasar.Trancon@uni-bayreuth.de"))
# Author: Baltasar Trancon y Widemann <Baltasar.Trancon@uni-bayreuth.de>
# Maintainer: Baltasar Trancon y Widemann
        # <Baltasar.Trancon@uni-bayreuth.de>
# Depends: R (>= 2.10.1), methods, graphics, stats
# LazyLoad: yes
# Collate: filled.contour.R Utilities.R Wavelets.R CWT.R
# Suggests:
# Description: Fix me!
# License: LGPL-3
# Built: R 2.13.1; ; 2011-10-30 18:02:16 UTC; windows

#--------------------------------------------------------------------  
#marked changes by Stefan Metzger (stefan.met@gmail.com), 2015-03-17
#fixes error message after moving to R 3.x:
#	Error in .Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels),  : there is no .Internal function 'filledcontour'
#fix on http://stackoverflow.com/questions/16812528/filled-contour-in-r-3-0-x-throws-error did not work: then gives an error message about plotting area too large
#so, instead of Baltasar's filled.contour.R, modified version now...
#	-calls .internal filled.contour() (i.e., Baltasar's filled.contour.R is not loaded in the first place)
#	-omits extra.space and extra.plot arguments to dodge warning messages of .internal filled.contour()
#	-consequently, does not plot global variance anymore (would require setup of graphics device similar to Baltasar's filled.contour.R)
#--------------------------------------------------------------------  


setClass("CWT",
         representation(series = "ts",
                        spectrum = "matrix",
                        scale = "numeric",
                        period = "numeric",
                        time = "numeric",
                        dt = "numeric",
                        dj = "numeric",
                        mean = "numeric",
                        wavelet = "Wavelet",
                        coi = "numeric"),
         prototype(series = ts(),
                   spectrum = matrix(),
                   scale = numeric(),
                   time = numeric(),
                   dt = 1,
                   dj = 1,
                   wavelet = NULL,
                   coi = numeric()))

setMethod("plot", signature(x = "CWT", y = "missing"),
          function(x, ...) plot.cwt(cwt = x, cwt2 = NULL, ...))

minscale.cwt <- function(x, wavelet)
  2 * deltat(x) / wavelet@fourier

nscale.cwt <- function(x, wavelet, scale0, dj)
  floor(log(length(x) * deltat(x) / scale0 / wavelet@fourier, base = 2) / dj)

cwt <- function(x, wavelet,
                scale0 = minscale.cwt(x, wavelet),
                dj = 0.25,
                jmax = nscale.cwt(x, wavelet, scale0, dj)) {
  x <- as.ts(x)
  n <- length(x)
  dt <- deltat(x)
  mx <-  mean(x)
  xm <- x - mx
  npad <- 2^ceiling(log(length(x), base = 2))
  if (n < npad)
    xm <- c(xm, rep(0, npad - n))
  w <- fft(xm) / npad
  freq0 <- 2 * pi / npad / dt
  freq <- freq0 * c(0:(npad / 2), -((npad / 2 - 1):1))
  scale <- sapply(0:jmax, function(j) scale0 * 2^(j * dj))
  spectrum <- matrix(ncol = length(scale), nrow = n)
  for (j in 0:jmax) {
    d <- daughter(wavelet, scale = scale[j + 1], freq = freq) / sqrt(dt)
    spectrum[,j + 1] <- fft(w * d, inv = T)[1:n]
  }
  new("CWT",
      spectrum = spectrum,
      series = x,
      scale = scale,
      period = scale * wavelet@fourier,
      time = as.vector(time(x)),
      dt = dt,
      dj = dj,
      mean = mx,
      wavelet = wavelet,
      coi = dt * c(1E-5,1:((n+1)/2-1),rev((1:(n/2-1))),1E-5) / wavelet@efold * wavelet@fourier
      )
}

compatible.cwt <- function(cwt1, cwt2, raise.error = FALSE) {
  if (any(dim(cwt1@spectrum) != dim(cwt2@spectrum))) {
    if (raise.error) stop("incompatible cwts: shapes of spectra differ")
    FALSE
  }
  if (cwt1@dt != cwt2@dt) {
    if (raise.error) stop("incompatible cwts: dt values differ")
    FALSE
  }
  if (cwt1@dj != cwt2@dj) {
    if (raise.error) stop("incompatible cwts: dj values differ")
    FALSE
  }
  if (cwt1@wavelet@cdelta != cwt2@wavelet@cdelta) {
    if (raise.error) stop("incompatible cwts: cdelta values differ")
    FALSE
  }
  if (raise.error)
    invisible(TRUE)
  else
    TRUE
}

var.cwt <- function(cwt, mask.coi = FALSE, aggregate = sum) {
  cov.cwt.internal(cwt, cwt, mask.coi = mask.coi, aggregate = aggregate)
}

cov.cwt <- function(cwt1, cwt2, mask.coi = FALSE, aggregate = sum) {
  compatible.cwt(cwt1, cwt2, raise.error = TRUE)
  cov.cwt.internal(cwt1, cwt2, mask.coi = mask.coi, aggregate = aggregate)
}

cov.cwt.internal <- function(cwt1, cwt2, mask.coi = FALSE, aggregate = sum) {
  spec1 <- spectrum.cwt(cwt1, mask.coi = mask.coi)
  spec2 <- spectrum.cwt(cwt2, mask.coi = mask.coi)
  energy <- matrix(Re(spec1 * Conj(spec2)) / cwt1@scale[col(spec1)],
                   ncol = ncol(spec1), nrow = nrow(spec1))
  weight <- cwt1@dj * cwt1@dt / (length(cwt1@series) * cwt1@wavelet@cdelta)
  weight * aggregate(energy, na.rm = T)
}

recon.cwt <- function(cwt, mask.coi = FALSE) {
  spec <- spectrum.cwt(cwt, mask.coi = mask.coi)
  recon <- vector(length = nrow(spec))
  a <- cwt@dj * sqrt(cwt@dt) / Re(mother(cwt@wavelet, time = 0)) / cwt@wavelet@cdelta
  for (i in 1:length(recon))
    recon[i] <- a * sum(Re(spec[i,]) / sqrt(cwt@scale), na.rm = T)
  x <- cwt@series
  ts(recon + cwt@mean, start = start(x), frequency = frequency(x))
}

plot.cwt <- function(cwt, cwt2 = NULL,
                     nlevels = 20L, contour = TRUE,
                     shortage = max(1, ceiling(nrow(cwt) / 1000)),
                     shortop = mean,
                     tfactor = 1, tunit = NULL,
                     ar = NULL, coi = TRUE, global = is.null(cwt2),
                     palette = function(nlevels) rev(rainbow(nlevels, start=0, end=4/6)),
                     period.weight = NULL,
                     rescale.method = "linear",
                     ...) {
  dotargs <- list(...)
  if (is.null(cwt2))
    spectr <- Mod(cwt@spectrum)^2
  else
    spectr <- Re(cwt@spectrum * Conj(cwt2@spectrum))
  mycol <- palette(nlevels)
  myx <- shorten(cwt@time, by=shortage, op=shortop) / tfactor
  myy <- log(cwt@period / tfactor, base=2)
  myz <- shorten(spectr, by=shortage, op=shortop)
  if (!is.null(period.weight))
    for (i in 1:ncol(myz))
      myz[,i] <- myz[,i] * period.weight(cwt@period[i])
  if (rescale.method == "quantile") {
    qs <- equalize(myz, k = nlevels, centered = !is.null(cwt2))
    myz <- matrix(interpol(qs, 0:nlevels)(as.vector(myz)),
                  nrow = nrow(myz), ncol = ncol(myz))
  }
  else if (rescale.method == "linear") {
    r <- smart.range(myz)
    qs <- seq(r[1], r[2], length = nlevels + 1)
    myz <- (myz - r[1]) / (r[2] - r[1]) * nlevels
  }
  else if (rescale.method == "log") {
    eps <- 1/sd(abs(as.vector(myz)))
    f <- function(x) sign(x) * log(abs(x) * eps + 1, base = 2)
    g <- function(x) sign(x) * (2^abs(x) - 1) / eps
    r <- sapply(smart.range(myz), f)
    qs <- sapply(seq(r[1], r[2], length = nlevels + 1), g)
    myz <- (matrix(sapply(myz, f), nrow = nrow(myz)) - r[1]) / (r[2] - r[1]) * nlevels
  }
  if (coi) {
    myx.coi <- c(min(myx), min(myx), myx, max(myx), max(myx))
    myy.coi <- c(max(myy), min(myy), log(shorten(cwt@coi, by=shortage) / tfactor, base=2), min(myy), max(myy))
    drawcoi <- function() polygon(myx.coi, myy.coi, density=18)
  }
  else
    drawcoi <- function() {}
  ## Title
  if (is.null(dotargs$main))
    dotargs$main <- ifelse(is.null(cwt2), "Wavelet Powerspectrum", "Wavelet Cross-Spectrum")
  ## Labels
  withunit <- function(text) {
    if (is.null(tunit))
      text
    else
      paste(text, " (", tunit, ")", sep = "")
  }
  if (is.null(dotargs$xlab))
    dotargs$xlab <- withunit("Time")
  if (is.null(dotargs$ylab))
    dotargs$ylab <- withunit("Scale")
  ## Color
  dotargs$col <- mycol
  ## Drawing
  addcall <- function(f, d) {
    f0 <- substitute(f)
    eval(as.call(c(as.list(f0), d)))
  }
  if (contour) {
    ticks <- pretty(0:nlevels)
    if (global) {
      extra.space <- 0.25
      extra.plot <- function() plotgw(cwt, sub=T, ar=ar)
    }
    else {
      extra.space <- NULL
      extra.plot <- function() {}
    }
    
  #--------------------------------------------------------------------  
  #start change by Stefan Metzger (stefan.met@gmail.com), 2015-03-17

#     addcall(filled.contour(myx, myy, myz, levels=0:nlevels,
#                            plot.axes={axis(1);logyrs <- ceiling(min(myy)):floor(max(myy));axis(2, at=logyrs, labels=signif(2^logyrs, 2));drawcoi()},
#                            key.axes={axis(4,at=ticks,labels=signif(qs[ticks+1], digits=2))},
#                            extra.space=extra.space, extra.plot=extra.plot),
#             dotargs)
    addcall(filled.contour(myx, myy, myz, levels=0:nlevels,
                           plot.axes={axis(1);logyrs <- ceiling(min(myy)):floor(max(myy));axis(2, at=logyrs, labels=signif(2^logyrs, 2));drawcoi()},
                           key.axes={axis(4,at=ticks,labels=signif(qs[ticks+1], digits=2))}),
            dotargs)        

  #end change by Stefan Metzger (stefan.met@gmail.com), 2015-03-17
  #--------------------------------------------------------------------  
  
  }
  else {
    addcall(image(x=myx, y=myy, z=myz, breaks=0:nlevels),
            dotargs)
    drawcoi()
  }
}


global.wavelet <- function(cwt) {
  n <- nrow(cwt@spectrum)
  m <- ncol(cwt@spectrum)
  period <- cwt@period
  gp <- vector(length=m)
  ok <- incoi(cwt)
  for (i in 1:m) {
    row <- cwt@spectrum[ok(period[i]),i]
    if (length(row) > 0)
      gp[i] <- mean(sapply(row, function(x) Mod(x)^2))
    else
      gp[i] <- NA
  }
  gp
}

plotgw <- function(cwt, sub=F, ar=NULL, ...) {
  gp <- global.wavelet(cwt)
  myy <- log(cwt@period, base=2)
  if (!sub)
    plot(gp, myy, type="l", ylim=range(myy), xlim=c(0, max(gp, na.rm=T)), yaxs='i', lwd=2,
         main="Global Wavelet", xlab="Variance", ylab="log2(scale [years])", ...)
  else {
    mar <- par("mar")
    mar[2] <- 0
    mar[3] <- 1
    plot.new()
    title("Global Wavelet", xlab="Variance")
    plot.window(ylim=range(myy), xlim=c(0, max(gp, na.rm=T)), yaxs='i')
    lines(gp, myy, type="l", lwd=2)
    axis(1)
    box()
  }
  x <- cwt@series
  if (is.null(ar))
    myalpha <- guess.ar(x)
  else
    myalpha <- ar
  theox <- noise.power(length(x), myalpha)[-1] * var(x)
  theoy <- log(cwt@dt * length(x) / 1:length(theox), base=2)
  lines(theox, theoy , lty=3)
  lines(qchisq(0.95, cwt@wavelet@df) * theox, theoy , lty=2)
}

guess.ar <- function(x) {
  myac <- acf(x, plot=F)
  mean(c(myac$acf[2], sqrt(max(0, myac$acf[3]))))
}

noise.power <- function(n, alpha=0) {
  (1 - alpha^2) / (1 + alpha^2 - 2 * alpha * cos(2 * pi * (0:floor(n/2)) / n))
}

incoi <- function(cwt) {
  mycoi <- cwt@coi
  m <- nrow(cwt@spectrum)
  function (period)
    mycoi[1:m] >= period
}

spectrum.cwt <- function(cwt, mask.coi = FALSE) {
  spec <- cwt@spectrum
  if (mask.coi) {
    ok <- incoi(cwt)
    for (i in 1:length(cwt@period))
      spec[!ok(cwt@period[i]),i] <- NA
  }
  spec
}
