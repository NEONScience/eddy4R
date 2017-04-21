


qfqm <- rhdf5::h5read(file = base::paste0(Para$Flow$DirInp, "/ECTE_dp0p_", Para$Flow$Loc, "_", Para$Flow$FileDp0p, ".h5"),
                      name = base::paste0("/", Para$Flow$Loc, "/dp0p/qfqm/", VarLoca, "_001/",Para$Flow$LevlTowr), read.attributes = TRUE)
                      
for(idx in base::names(qfqm)) qfqm[[idx]] <- base::as.vector(qfqm[[idx]]); base::rm(idx)

lapply(seq_len(length(qfqm)), function(x){ 
  print(x)
  attributes(qfqm[[x]])$Unit <<- attributes(qfqm)$Unit[[x]]
  })

# convert list to data.frame
qfqm <- base::as.data.frame(qfqm, stringsAsFactors = FALSE)

