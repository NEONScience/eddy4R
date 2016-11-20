# wet mass fraction (specific humidity)
data$irga$FW_mass_H2O_7200 <- ff::as.ff(data$irga$rhoMoleH2O * eddy4R.base::Natu$MolmH2o /
                                          (data$irga$rho_mole_dry_7200 * eddy4R.base::Natu$MolmDry + data$irga$rhoMoleH2O * eddy4R.base::Natu$MolmH2o))
base::attr(x = data$irga$FW_mass_H2O_7200, which = "unit") <- "kg kg-1"