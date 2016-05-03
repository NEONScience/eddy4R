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

piq <- pi^-0.25

i <- 0+1i

heaviside <- function(x) ifelse(x > 0, 1, 0)

horner <- function(p, x) {
  y <- rep(0, length(x))
  for (k in length(p):1)
    y <- y * x + p[k]
  y
}

ztransform <- function(x) (x-mean(x))/sd(x)

shorten <- function(x, by=2, op=mean) {
  if (is.matrix(x)) {
    n <- nrow(x)
    m <- ncol(x)
    y <- matrix(nrow=ceiling(n/by), ncol=m)
    for (j in 1:m)
      y[,j] <- as.vector(tapply(x[,j], floor(0:(n-1)/by)+1, op))
    if (n %% by > 0)
      y[-ceiling(n/by),]
    else
      y
  }
  else {
    n <- length(x)
    y <- tapply(x, floor(0:(n-1)/by)+1, op)
    if (n %% by > 0)
      y[-ceiling(n/by)]
    else
      y
  }
}

interpol <- function(xs, ys) {
  n <- length(xs)
  f <- function(x) {
    if (x < xs[1]) {
      NA
    }
    else if (x > xs[n]) {
      NA
    }
    else {
      for (i in 1:n) {
        if (x == xs[i]) {
          return(ys[i])
        }
        else if (x < xs[i]) {
          d <- (x - xs[i-1]) / (xs[i] - xs[i - 1])
          return((1-d) * ys[i-1] + d * ys[i])
        }
      }
    }
  }
  function(x) sapply(x, f)
}

equalize <- function(xs, k = 1, centered = F) {
  v <- as.vector(xs)
  if (centered && min(v) < 0) {
    pos <- v[v >= 0]
    neg <- v[v < 0]
    if (k %% 2 == 0)
      qpos <- seq(0, 1, length = k + 1)[seq(1, k + 1, by = 2)]
    else
      qpos <- seq(0, 1, length = k + 1)[seq(2, k + 1, by = 2)]
    qneg <- seq(0, 1, length = k + 1)[seq(1, k + 1, by = 2)]
    qs <- c(quantile(neg, qneg)[-length(qneg)], 0, quantile(pos, qpos)[-1])
  }
  else {
    qs <- quantile(v, seq(0, 1, length = k + 1))
  }
  as.vector(qs)
}

smart.range <- function(x) {
  r <- range(x)
  if (r[1] > 0)
    c(0, r[2])
  else if (r[2] < 0)
    c(r[1], 0)
  else {
    s <- max(abs(r))
    c(-s, s)
  }
}
