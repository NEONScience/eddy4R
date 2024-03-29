% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.ucrt.samp.R
\name{def.ucrt.samp}
\alias{def.ucrt.samp}
\title{Definition function: Statistical errors for scalars, variances, and fluxes}
\usage{
def.ucrt.samp(
  data = NULL,
  distIsca,
  valuMean,
  coefCorr,
  distMean,
  timeFold = 0,
  spcs = NULL
)
}
\arguments{
\item{\code{data}}{A vector of instantaneous data. If user provides vector, Moving block bootstrap technique will be implemented to provide an additional uncertainty estimate. If NULL is provided, only Lenschow technique will be performed.}

\item{\code{distIsca}}{A list of integral length scales for scalars, variances, and fluxes.}

\item{\code{valuMean}}{Mean Values.}

\item{\code{coefCorr}}{Flux correlation coefficient}

\item{\code{distMean}}{Average flight length}

\item{\code{timeFold}}{e-folding time}
}
\value{
Statistical Errors for scalars, variances, and fluxes
}
\description{
Function defintion. Statistical errors for scalars, variances, and fluxes.
}
\examples{
Currently none
}
\references{
Torrence and Compo, 1998 \cr
Lenschow, 1994 \cr
D.H. Lenschow, B.B.Stankov. (1986) Length Scales in the Convective Boundary Layer. Journal of the Atmospheric Sciences. 43:12, 1198-1209 \cr
J.Mann, D. Lenschow (1994) Errors in airborne flux measurements. JGR:Atmospheres, 99, 14519-14526 \cr
J. Bange (2002). Airborne Measurements of Turbulent Energy Exchange Between the Earth Surface and the Atmosphere. \cr
S. Salesky, M. Chamecki, N. Dias (2012) Estimating random error in eddy covariance fluxes and other turbulence statistics: the filtering method .  Boundary-Layer Meteorology., 144, 113-135. \cr 
J.Bange, F. Beyrich, D. Engelbart. (2002) Airborne measurements of turbulent fluxes during LITFASS-98: Comparison with ground measurements and remote sensing in a case study. Theor. Appl. Climatol. 73, 35-51 \cr
}
\seealso{
Currently none
}
\author{
Stefan Metzger \email{eddy4R.info@gmail.com}
Kenny Pratt
}
\keyword{eddy-covariance,}
\keyword{sampling}
\keyword{uncertainty}
