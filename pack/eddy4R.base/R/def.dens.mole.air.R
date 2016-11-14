# molar density of dry air and water vapor
data$irga$rho_mole_air_7200 <- ff::as.ff(data$irga$p_cell_7200 / eddy4R.base::Natu$Rg / data$irga$T_cell_7200)
base::attr(x = data$irga$rho_mole_air_7200, which = "unit") <- "mol m-3"