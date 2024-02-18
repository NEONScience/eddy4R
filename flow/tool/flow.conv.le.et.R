#############################################################################################
#' @title Workflow for converting 30 minute latent heat flux data to evapotranspiration 

#' @author
#' Chris Florian \email{eddy4R.info@gmail.com}
#' Stefan Metzger \email{eddy4R.info@gmail.com}

#' @description
#' Workflow. Workflow for downloading data using neonUtilities and converting 30 minute latent heat flux data to FAO ET units of mm d-1. 

#' @param Currently none

#' @return Currently none

#' @references

#' @keywords eddy-covariance, NEON

#' @examples Currently none

#' @seealso Currently none

# changelog and author contributions / copyrights
#   Chris Florian (2021-10-15)
#     original creation
##############################################################################################

#install package dependencies 
#install neonUtilities from CRAN

install.packages("neonUtilities")

#install eddy4R.base from GitHub

devtools::install_github('NEONScience/eddy4R/pack/eddy4R.base')

#define site and time period

site <- "OSBS"

startdate <- "2021-07-01"

enddate <- "2021-09-01"

#set input and output directories 

DirIn <- #"C:/Users/cflorian/Desktop/LE-to-ET"

DirOut <- #"C:/Users/cflorian/Desktop/LE-to-ET"

#download data from the NEON data portal

neonUtilities::zipsByProduct(dpID = "DP4.00200.001", site = site, startdate = startdate, enddate = enddate, package = "basic", check.size = F, savepath = DirOut)

#extract LE data from bundled HDF5 files

dp04Data <- neonUtilities::stackEddy(filepath = paste0(DirIn, "/filesToStack00200"), level = "dp04", var = "fluxH2o", avg = 30)

LE <- dp4Data[[site]]$data.fluxH2o.turb.flux

#extract air temp data from bundled HDF5 files

dp01Data <- neonUtilities::stackEddy(filepath = paste0(DirIn, "/filesToStack00200"), level = "dp01", var = c("soni"), avg = 30)

tempAir <- dp1Data[[site]]$data.soni.tempAir.mean

#extract time and convert from character to time format

timeBgn <- as.POSIXct(dp4Data[[site]]$timeBgn, format="%Y-%m-%dT%H:%M:%S", tz="GMT")

#compile working dataframe

data <- data.frame(cbind(timeBgn, LE, tempAir))

#remove flagged data

data <- data[which(dp1Data[[site]]$qfqm.soni.tempAir.qfFinl == 0 & dp4Data[[site]]$qfqm.fluxH2o.turb.qfFinl == 0),]

#calculate latent heat of vaporization 

data$Lv <- 2500827 - 2360 * eddy4R.base::def.unit.conv(data=as.numeric(data$tempAir),unitFrom="K",unitTo="C")

#latent heat flux in units of energy  [W m-2] == [kg s-3] to kinematic units [mol m-2 s-1]
#warning: ideally this calculation would be performed with the high frequency raw data, using half hourly data might be good enough for some use cases 

data$LE_kin <- data$LE/(data$Lv * eddy4R.base::IntlNatu$MolmH2o)

# conversion of evapotranspiration from kinematic units [mol m-2 s-1] to FAO units [mm d-1] (http://www.fao.org/docrep/X0490E/x0490e04.htm)
# based on molar volume of 1.802e-5 m3 mol-1 (http://www.science.uwaterloo.ca/~cchieh/cact/applychem/waterphys.html)
# times 1e3 conversion from m to mm
# times 86400 conversion from s to d
  
data$ET <- data$LE_kin * 1.802e-5 * 1e3 * 86400
  