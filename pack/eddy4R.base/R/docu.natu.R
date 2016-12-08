#' Natural Constants
#'
#' Natural constants, such as the ideal gas constant, for use across eddy4R package suite. 
#' 
#'
#' @format A list of named constants:
#' \describe{
#'   \item{Grav}{acceleration of gravity = 9.81 [m s-2]}
#'   \item{PrdErth}{rotation period of the Earth (one sidereal day) = 86164.1 [s]}
#'   \item{AvelErth}{angular velocity of Earth = 7.2921159e-05 [rad s-1]}
#'   \item{Pres00}{NIST standard pressure = 101325 [Pa == kg m-1 s-2]}
#'   \item{Temp00}{NIST standard temperature = 293.15 [K]}
#'   \item{Rg}{ideal gas constant = 8.314462175 [J mol-1 K-1 == kg m2 s-2 mol-1 K-1]}
#'   \item{VonkFokn}{von-Karman constant accordig to Foken (2008) = 0.4 [-]}
#'   \item{MolmDry}{molecular mass of dry air = 28.97e-3 [kg mol-1]}
#'   \item{MolmH2o}{molecular mass of water vapor = 18.02e-3 [kg mol-1]}
#'   \item{MolmCo2}{molecular mass of carbon dioxide = 44.01e-3 [kg mol-1]}
#'   \item{MolmCh4}{molecular mass of methane = 16.04e-3 [kg mol-1]}
#'   \item{MolmC}{molecular mass of carbon = 12e-3 [kg mol-1]}
#'   \item{RtioMolmH2oDry}{molar mass ratio water vapour / dry air = 0.6220228 [-]}
#'   \item{RtioMolmDryH2o}{molar mass ratio dry air / water vapour = 1.607658 [-]}
#'   \item{CpDry}{dry air specific heat at constant pressure = 1004.64 [J kg-1 K-1]}
#'   \item{CvDry}{dry air specific heat at constant volume = 717.6 [J kg-1 K-1]}
#'   \item{RsDry}{specific gas constant for dry air	= 287.04 [J kg-1 K-1]}
#'   \item{GmmaDry}{CpDry / CvDry = 1.4 [-]}
#'   \item{KppaDry}{Dry air Kappa exponent for ideal gas law (Poisson), RsDry / CpDry = 0.2857 [-]}
#'   \item{CpH2o}{water vapour specific heat at constant pressure = 1846 [J kg-1 K-1]}
#'   \item{CvH2o}{water vapour specific heat at constant volume = 1384.04 [J kg-1 K-1]}
#'   \item{RsH2o}{specific gas constant for water vapour	= 461.96 [J kg-1 K-1]}
#'   \item{GmmaH2o}{CpH2o / CvH2o = 1.333776 [-]}
#'   \item{KppaH2o}{water vapour Kappa exponent for ideal gas law (Poisson), RsH2o / CpH2o = 0.2502 [-]}
#' }
#' @source Natural constants are defined within flow.save.intl.cnst.R,
#'  available in the data-raw/ folder of the source version of the package
"Natu"
