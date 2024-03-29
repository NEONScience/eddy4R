% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.itc.R
\name{def.itc}
\alias{def.itc}
\title{Definition function: Integral turbulence characteristics}
\usage{
def.itc(
  stblObkv,
  lat,
  VarInp = c("veloXaxs", "veloZaxs", "temp", "all")[4],
  sd,
  varScal,
  Thsh = 100,
  Perc = TRUE,
  CorTemp = TRUE
)
}
\arguments{
\item{\code{stblObkv}}{Stability parameter and of class "numeric". [dimensionless]}

\item{\code{lat}}{Latitude and of class "numeric". [degrees North]}

\item{\code{VarInp}}{A vector of class "character" containing the name of variables to be performed integral turbulence characteristics test. \code{VarInp} = c("veloXaxs","veloZaxs","temp","all"), where "veloXaxs" is along-axis horizontal wind speed, "veloZaxs" is vertical-axis wind speed, "temp" is air temperature, and "all" is all three variables. Defaults to "all".[-]}

\item{\code{sd}}{A vector or data frame containing standard deviation of \code{VarInp} and of class "numeric". If \code{VarInp} = "all",  \code{sd} shall contain in the follwing orders, standard deviation of along-axis horizontal wind speed, standard deviation of vertical-axis wind speed, and standard deviation of air temperature. [user-defined]}

\item{\code{varScal}}{A vector or data frame containing the scaling variables of \code{VarInp} and of class "numeric". If \code{VarInp} = "all", \code{varScal} shall contain in the follwing orders, scaling variable of wind speed (friction velocity will be used for both "veloXaxs" and "veloZaxs") and scaling variable of air temperature.  [user-defined]}

\item{\code{Thsh}}{Threshold value to indicate if quality flag will raise high (qfItc = 1) when the calculated quality indicator values are greater the threshold. Defaults as 100. [percent]}

\item{\code{CorTemp}}{Logical state if the temperture is used in the determination of ITCs of sensible heat flux (fluxSens). Defaults as TRUE.}
}
\value{
The returned object consists of:\cr
\code{qiItc} integral turbulence characteristics test results (quality indicator) of \code{VarInp}. If \code{VarInp} = "all", it includes test results for individual variable, i.e., along-axis horizontal wind speed, vertical-axis wind speed, and air temperature and also combined variables, i.e., friction velocity and sensible heat flux. [percent]\cr
\code{qfItc} integral turbulence characteristics quality flag of of \code{VarInp}. If \code{VarInp} = "all", it includes quality flag results for individual variable, i.e., along-axis horizontal wind speed, vertical-axis wind speed, and air temperature and also combined variables, i.e., friction velocity and sensible heat flux. [NA]
}
\description{
Function defintion. Integral turbulence characteristics.
}
\examples{
#input
sd <- data.frame(veloXaxs=0.5254, veloZaxs=0.2570, temp=0.1405)
varScal <- data.frame(veloFric=0.1918, tempScal=0.0268)
#call function
def.itc(stblObkv=0.0255, lat=70.4214, VarInp=c("all"), sd=sd, varScal=varScal)
#single entry
def.itc(stblObkv=0.0255, lat=70.4214, VarInp=c("veloXaxs"), sd=0.5254, varScal=0.1918)
}
\references{
Thomas, C. and Foken, T: Re-evaluation of integral turbulence characteristics and their parameterisations, 15th Conference on Turbulence and Boundary Layers, Wageningen, NL, American Meteorological Society, pp. 129-132, 2002.
}
\seealso{
Currently none
}
\author{
Stefan Metzger \email{eddy4R.info@gmail.com} \cr
Natchaya Pingintha-Durden \cr
}
\keyword{characteristics,}
\keyword{eddy-covariance,}
\keyword{flux}
\keyword{integral}
\keyword{turbulence}
\keyword{turbulent}
