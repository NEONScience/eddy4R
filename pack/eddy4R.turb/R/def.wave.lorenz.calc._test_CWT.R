def.wave.lorenz.calc._test_CWT <- function(veloZaxsCoef) {
  
  waveScalOrd <- do.call(cbind, lapply(seq(1, ncol(veloZaxsCoef)), function(x) sort(veloZaxsCoef[,x], decreasing = FALSE))) # Sort data in descending order
  idxWaveScalOrd <- do.call(cbind, lapply(seq(1, ncol(veloZaxsCoef)), function(x) sort.int(veloZaxsCoef[,x], decreasing = FALSE, index.return = TRUE)$ix)) # Sort data in descending order

  waveScalSum <- colSums(waveScalOrd)

  frac <- seq(1, nrow(veloZaxsCoef)) / nrow(veloZaxsCoef)
  lorenzOut <- do.call(cbind, lapply(seq(1, ncol(veloZaxsCoef)), function(x) (cumsum(waveScalOrd[,x])) / waveScalSum[x]))
  
  return(list(lorenz = lorenzOut, frac = frac, index = idxWaveScalOrd))

}