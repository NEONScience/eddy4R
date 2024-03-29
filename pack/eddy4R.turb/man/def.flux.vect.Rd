% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.flux.vect.R
\name{def.flux.vect}
\alias{def.flux.vect}
\title{Definition function: Eddy-covariance turbulent flux calculation for vector quantities}
\usage{
def.flux.vect(
  inp,
  rot,
  Unit = base::data.frame(Inp = "m s-1", Out = "m s-1", OutSq = "m2 s-2")
)
}
\arguments{
\item{inp}{A data frame containing the instantaneous differences produced by ?def.stat.sta.diff() of 1) the wind vector in meteorological ENU convention with the variables veloXaxs (latitudinal wind speed, positive from west), veloYaxs (longitudinal wind speed, positive from south), and veloZaxs (vertical wind speed, positive from below), and 2) the wind vector in streamwise ENU convention with the variables veloXaxsHor (streamwise wind speed, positive from front), veloYaxsHor (cross-wind speed, positive from left), and veloZaxsHor (vertical wind speed, positive from below) derived from ?def.rot.ang.zaxs.erth, of class "numeric", each with unit attribute [m s-1]. The wind vector inputs can be viewed as a specific example that can be generalized through replacement by other vector quantities that share the same coordinate conventions and consistent units among inp and Unit.}

\item{rot}{A list of rotation matrices with the list elements mtrxRot01 and mtrxRot02 derived from ?def.rot.ang.zaxs.erth, class "numeric", each with unit attribute. [-]}

\item{Unit}{A data frame with the entries Inp (input units), Out (output units), and OutSq (squared output units), of class "character".}
}
\value{
The returned object is a list containing the element dataframes corr, diff, mean, and sd, each of class "numeric" and with unit attribute.
The elements corr, mean and sd are all calculated from the same stress tensor based on the inp (veloXaxs, veloYaxs, veloZaxs) and rot (mtrxRot01 and mtrxRot02) arguments. The element corr contains the horizontal-vertical correlations, the element mean contains the horizontal-vertical covariances, and the element sd contains the standard deviation for each wind vector component in streamwise ENU convention, with a single observation each.
The element diff contains the instantaneous horizontal-vertical products of inp (veloXaxsHor, veloYaxsHor, veloZaxsHor) with the same number of observations as inp.
}
\description{
Function definition. This function calculates eddy-covariance turbulent flux for vector quantities, such as the wind vector -> momentum flux and friction velocity.
}
\examples{
Make sure to assign all variables and units, the function should run ok.
inp <- base::data.frame(
  veloXaxs = c(-1.889635, -1.661724, -1.615837, -1.711132, -1.223001),
  veloYaxs = c(1.365195, 1.277106, 1.394891, 1.180698, 1.283836),
  veloZaxs = c(0.176613897, 0.184947662, 0.344331819, 0.190230311, 0.239193186)
)
attr(inp$veloXaxs,"unit") <- "m s-1"; attr(inp$veloYaxs,"unit") <- "m s-1"; attr(inp$veloZaxs,"unit") <- "m s-1"
out <- def.flux.vect(
  inp = base::cbind(def.stat.sta.diff(inp = inp)$diff,
                    def.stat.sta.diff(inp = def.rot.ang.zaxs.erth(inp = inp)$data)$diff),
  rot = def.rot.ang.zaxs.erth(inp = inp)$rot,
  Unit = base::data.frame(Inp = "m s-1", Out = "m s-1", OutSq = "m2 s-2")
)
utils::str(out)
base::rm(inp, out)
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
}
\seealso{
Currently none.
}
\author{
Stefan Metzger \email{eddy4R.info@gmail.com}
}
\keyword{correlation,}
\keyword{deviation,}
\keyword{flux,}
\keyword{friction}
\keyword{shear}
\keyword{standard}
\keyword{stress,}
\keyword{vector}
\keyword{velocity,}
