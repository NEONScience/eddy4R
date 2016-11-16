# molar density of dry air alone
data$irga$rho_mole_dry_7200 <- ff::as.ff(data$irga$rho_mole_air_7200 - data$irga$densMoleH2o)
base::attr(x = data$irga$rho_mole_dry_7200, which = "unit") <- "mol m-3"