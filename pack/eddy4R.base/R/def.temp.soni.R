# sonic temperature [K] from speed of sound [m s-1] (Campbell Scientific, Eq. (9))
data$soni$T_SONIC <- ff::as.ff(data$soni$veloSoni^2 / eddy4R.base::Natu$GmmaDry / 
                                 (eddy4R.base::Natu$Rg / eddy4R.base::Natu$MolmDry))
base::attr(x = data$soni$T_SONIC, which = "unit") <- "K"