% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.flux.sclr.R
\name{def.flux.sclr}
\alias{def.flux.sclr}
\title{Definition function: Eddy-covariance turbulent flux calculation for scalar quantities}
\usage{
def.flux.sclr(
  inp,
  conv = NULL,
  Unit,
  AlgBase = c("mean", "trnd", "ord03")[1],
  idep = NULL
)
}
\arguments{
\item{inp}{A data frame with the variables vect and sclr that each contain the instantaneous differences reported by ?def.stat.sta.diff. In a typical eddy-covariance application, vect would be the vertical wind speed in streamwise ENU convention (positive from below), e.g.  veloZaxs derived from ?def.rot.ang.zaxs.erth and further processed in ?def.stat.sta.diff, of class "numeric" and with unit attribute [m s-1]. scal would be any scalar quantity in SI base units that does not require WPL density correction (Webb et al., 1980), i.e. dry air temperature in unit [K] and gas concentration in dry mole fraction [mol m-3], of class "numeric" and with unit attribute. These inputs can be viewed as a specific example that can be generalized through replacement by other variables that share the same coordinate conventions and consistent units among inp and Unit.}

\item{conv}{An optional vector of class "numeric" with unit attribute to permit conversion of the results, e.g. to output units that are different from the product of the inp$vect unit and the inp$sclr unit. conv must be either of length = 1 or have the same length as number of observations in inp. If conv is of length = 1, then the same conversion factor is applied to all observations supplied in inp (e.g., unit conversion). On the other hand, if conv is of the same length as number of observations in inp, then a point-by-point conversion is performed individually for each observation supplied in inp (e.g., different weights for each observation).}

\item{Unit}{A data frame with the entries InpVect, InpSclr, Conv, Out, of class "character". To ensure consistent units of the returned object, Unit needs to be specified with the constraint that Unit$Out = Unit$InpVect * Unit$InpSclr * Unit$Conv. If the function call argument conv is not specified, then Unit$Conv should be supplied as = "-".}

\item{AlgBase}{A vector of length 1 that defines the base state with respect to which the element-dataframe base in the returned object is calculated, of class "character" and no unit attribute. Is set to one of AlgBase <- c("mean", "trnd", "ord03")[1] and defaults to "mean", with the additional options detrending "trnd" and 3rd-order polynomial "ord03". See ?eddy4R.base::def.base.ec() for additional details.}

\item{idep}{An optional vector of class "numeric" with unit attribute. idep is only required to be specified if argument AlgBase is set to "trnd" or "ord03", in which case idep provides the independent variable for interpolation.}
}
\value{
The returned object is a list containing the element vectors base, conv, corr, diff, max, mean, min, sd, each of class "numeric" and with unit attribute.
All elements with the exception of conv and corr are calculated from the instantaneous products inp$vect * inp$scal * conv and are assigned the Unit$Out unit attribute. diff provides the instantaneous products themselves, with the same number of observations as inp. base provides the base state of the instantaneous products as specified per argument AlgBase, with a single observation for AlgBase = "mean", and the same number of observations as inp for AlgBase %in% c("trnd", "ord03"). max, mean, min and sd provide the maximum, mean, minimum and standard deviation of the instantaneous products, respectively, each with a single observation.
The element conv reports the conversion vector utilized during the fuction call, with the same number of observations as inp and unit attribute.
The element corr is calculated directly from inp$vect and inp$scal without invoking conv, and provides the correlation between vector and scalar quantity with unit attribute "-".
}
\description{
Function definition. This function calculates eddy-covariance turbulent flux for scalar quantities, such as temperature, moisture, CO2, CH4, NOx, VOCs etc.
}
\examples{
Sensible heat flux in units of energy [kg s-3] = [W m-2]
make sure to assign all variables and units, the function should run ok.
input data: vertical wind speed and temperature instantaneous differences from base state, see ?def.stat.sta.diff for details
inp <- base::data.frame(
  vect = c(0.2259224, 0.2342562, 0.3936403, 0.2395388, 0.2885017),
  sclr = c(0.1067013, 0.1015043, 0.1324425, 0.1732023, 0.1262345)
)
attr(inp$vect,"unit") <- "m s-1"; attr(inp$sclr,"unit") <- "K"
volumetric heat capacity for conversion from kinematic units [K m s-1] to units of energy [W m-2], see ?def.heat.air.wet for details
conv <- 1220.079
attr(conv,"unit") <- "kg m-1 s2 K-1"
function call
out <- def.flux.sclr(
  inp = inp,
  conv = conv,
  Unit = base::data.frame(InpVect = "m s-1", InpSclr = "K", Conv = "kg m-1 s2 K-1", Out = "W m-2")
)
utils::str(out)
base::rm(inp, conv, out)
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
Webb, E. K., Pearman, G. I., and Leuning, R.: Correction of flux measurements for density effects due to heat and water vapour transfer, Q. J. R. Meteorolog. Soc., 106, 85-100, doi:10.1002/qj.49710644707, 1980.
}
\seealso{
Currently none.
}
\author{
Stefan Metzger \email{eddy4R.info@gmail.com}
}
\keyword{CH4,}
\keyword{CO2,}
\keyword{H2O,}
\keyword{NOx,}
\keyword{VOC,}
\keyword{correlation,}
\keyword{deviation,}
\keyword{flux,}
\keyword{humidity,}
\keyword{moisture,}
\keyword{scalar}
\keyword{standard}
\keyword{temperature,}
\keyword{water,}
