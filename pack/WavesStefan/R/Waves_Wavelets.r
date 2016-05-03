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

setClass("Wavelet",
         representation(tnorm = "complex",
                        fnorm = "complex",
                        efold = "numeric",
                        fourier = "numeric",
                        cdelta = "numeric",
                        df = "numeric"))

setClass("Wavelet.Morlet",
         representation(mod = "numeric"),
         prototype(fnorm = as.complex(piq),
                   tnorm = as.complex(piq),
                   efold = sqrt(2),
                   df = 2),
         contains = "Wavelet")

setClass("Wavelet.DOG",
         representation(order = "integer",
                        polynomial = "function"),
         prototype(efold = sqrt(2),
                   df = 1),
         contains = "Wavelet")

setClass("Wavelet.Haar",
         representation(),
         prototype(efold = 1,
                   df = 1,
                   fourier = 2*pi,
                   fnorm = 1+0i,
                   tnorm = 1+0i),
         contains = "Wavelet")

morlet <- function(mod = 6)
  add.cdelta(new("Wavelet.Morlet", mod = mod,
                 fourier = 4 * pi / (mod + sqrt(2 + mod^2))))

polynomial.dog <- function(m) {
  p <- 1
  if (m > 0)
  for (k in 1:m) {
    p <- c(0, -p) + c(p[-1] * (1:(k-1)), 0, 0)
  }
  function(x) horner(p, x) * exp(-x^2/2)
}

dog <- function(order = 2L) {
  order <- as.integer(order)
  stopifnot(order >= 0)
  u <- sqrt(gamma(order + 0.5))
  add.cdelta(new("Wavelet.DOG", order = order,
                 fnorm = - as.complex(i^order / u),
                 tnorm = as.complex((-1)^(order + 1) / u),
                 fourier = 2 * pi / sqrt(order + 0.5),
                 polynomial = polynomial.dog(order)))
}

mother <- function(wavelet, time) stop("method \"mother\" undefined for wavelet class ", class(wavelet))
daughter <- function(wavelet, scale, freq, time) stop("method \"daughter\" undefined for wavelet class ", class(wavelet))

setGeneric("mother")
setGeneric("daughter")

setMethod("daughter", signature(wavelet="Wavelet", scale="numeric", freq="missing", time="numeric"),
          function(wavelet, scale, time) mother(wavelet, time = time / scale[1]) / sqrt(scale[1]))


setMethod("daughter", signature(wavelet="Wavelet.Morlet", scale="numeric", freq="numeric", time="missing"),
          function(wavelet, scale, freq) normalize.energy(wavelet, scale[1]) * heaviside(freq) * exp(-(scale[1]*freq-wavelet@mod)^2/2))

setMethod("mother", signature(wavelet="Wavelet.Morlet", time="numeric"),
          function(wavelet, time) wavelet@tnorm * exp(-time^2 / 2 + i * wavelet@mod * time))

setMethod("daughter", signature(wavelet="Wavelet.DOG", scale="numeric", freq="numeric", time="missing"),
          function(wavelet, scale, freq) { m <- wavelet@order; sf <- scale[1] * freq; normalize.energy(wavelet, scale[1]) * sf^m * exp(-sf^2 / 2) })

setMethod("mother", signature(wavelet="Wavelet.DOG", time="numeric"),
          function(wavelet, time) wavelet@tnorm * wavelet@polynomial(time))


setMethod("mother", signature(wavelet="Wavelet.Haar", time="numeric"),
          function(wavelet, time) { x <- rep(0, length(time)); x[time >= 0 & time < 1/2] <- 1; x[time >= 1/2 & time < 1] <- -1; wavelet@tnorm * x })

setMethod("daughter", signature(wavelet="Wavelet.Haar", scale="numeric", freq="numeric", time="missing"),
          function(wavelet, scale, freq) normalize.energy(wavelet, scale) * fouhaar(scale * freq))


normalize.energy <- function(wavelet, scale)
  sqrt(2 * pi * scale) * wavelet@fnorm

plot.wavelet <- function(x, ...) {
  ft <- 3 * x@efold
  t <- seq(-ft, ft, by=0.01)
  w <- daughter(x, scale=1, time=t)
  yl <- max(abs(range(c(Re(w), Im(w)))))
  plot(c(), xlim=range(t), ylim=c(-yl, yl), xlab="time", ylab="", ...)
  abline(h=0)
  lines(t, Re(w), type="l", lty=1, ...)
  if (any(Im(w) != 0))
    lines(t, Im(w), type="l", lty=2, ...)
}

setMethod("plot", signature(x="Wavelet", y="missing"), plot.wavelet)

recon.delta <- function(wavelet, n=4096, dj=1/4, dt=1, scale0 = 2 / wavelet@fourier,
                        jmax = floor(log(n * dt / scale0, base = 2)) / dj) {
  scale0 <- 2 / wavelet@fourier
  freq0 <- 2 * pi / n / dt
  freq <- freq0 * c(0:(n / 2), -((n / 2 - 1):1))
  scale <- sapply(0:jmax, function(j) scale0 * 2^(j * dj)) * dt
  spec <- vector(length = length(scale))
  for (j in 1:length(scale))
    spec[j] <- mean(Conj(daughter(wavelet, scale = scale[j], freq = freq)))
  dj / Re(mother(wavelet, time = 0)) * sum(Re(spec) / sqrt(scale))
}

add.cdelta <- function(wavelet) {
  wavelet@cdelta <- recon.delta2(wavelet)
  wavelet
}

recon.delta2 <- function(wavelet) {
  dxi <- 0.001
  k <- 10
  xi <- seq(from = -k*wavelet@efold, to = k*wavelet@efold, by = dxi)
  nope <- which(xi == 0)
  if (length(nope) > 0)
    xi <- xi[-nope]
  stuff <- Mod(daughter(wavelet, scale=1, freq=xi))^2 / abs(xi)
  dxi * sum(stuff) / 1.3696
}

mexicanHat <- dog(order = 2)


fouhaar <- function(f) {
  g <- pi * f / 2
  ifelse(f == 0, 0, 0+1i * exp(0-2i * g) * sin(g)^2 / g)
}

haar <- add.cdelta(new("Wavelet.Haar"))

