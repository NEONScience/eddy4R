def.wave.lorenz.calc <- function(x) {
  
  xsort <- lapply(x, function(x) sort(x))
  index <- lapply(x, function(x) sort.int(x, index.return = TRUE)$ix)
  
  frac <- lapply(index, function(x) seq(1, length(x)) / length(x))
  lorenz <- lapply(xsort, function(x) cumsum(x) / sum(x))
  
  return(list(lorenz = lorenz, index = index, frac = frac))
  
}