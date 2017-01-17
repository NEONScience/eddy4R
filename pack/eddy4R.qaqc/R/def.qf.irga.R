diag01 <- rep(8191, 72000)

#qfIrga <- intToBits(diag01)

qfIrga <- t(sapply(diag01,function(x){ as.integer(intToBits(x))}))

bitsToInt<-function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}

qfIrgaAgc <- sapply(seq_len(nrow(qfIrga)), function(x) (bitsToInt(qfIrga[x,1:4])*6.67)/100)


qfIrga <- data.frame(qfIrgaAgc, qfIrga[,5:13])

names(qfIrga) <- c("qfIrgaAgc", "qfIrgaSync", "qfIrgaPll", "qfIrgaChop","qfIrgaDetc", "qfIrgaPres", "qfIrgaAux", "qfIrgaTempIn", "qfIrgaTempOut", "qfIrgaHead")


#deci2base <- function(deci, base = 2) {
#      a <- base ^ (31:0)
#      b <- 2 * a
#      sapply(deci, function(x) paste(as.integer((x %% b)>=a), collapse=""))
# }
