% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.stna.R
\name{def.stna}
\alias{def.stna}
\title{Definition function: Stationarity tests}
\usage{
def.stna(
  data,
  MethStna = c(1, 2, 3)[2],
  whrVar,
  NumSubSamp = 6,
  corTempPot = TRUE,
  presTempPot = NULL,
  vrbs = TRUE,
  Thsh = 100,
  Perc = TRUE,
  ...
)
}
\arguments{
\item{\code{data}}{A vector containing the input data. Of class "numeric" or "integer". [user-defined]}

\item{\code{MethStna}}{A Vector containing the stationarity test methods. \code{MethStna} = c(1,2,3), where 1 is calculating using trend method (Vickers and Mahrt, 1997) , 2 is calculating using internal stationarity method (Foken and Wichura, 1996) , and 3 is calculating using both methods. Defaults to 2. [-]}

\item{\code{whrVar}}{Specific column in \code{data} containing the variables to be performed stationarity test. Of class "numeric" (column number) or "character" (column name). Defaults to NULL. [-]}

\item{\code{NumSubSamp}}{An object of class "numeric" or "integer" containing the number of sub sample over averaing period. For example, \code{NumSubSamp} = 6 if a 30 min averaging period is subsetted into 5 minute intervals. Defaults to 6. [-]}

\item{\code{corTempPot}}{A logical indicating whether or not to use potential temperature in flux calculation. Defaults to TRUE. [-]}

\item{\code{presTempPot}}{A vector containing the air pressure data that will be used in the calculation when \code{corTempPot}=TRUE. Of class "numeric" or "integer" and of the same length as \code{data} or single entry. [Pa]}

\item{\code{PltfEc}}{A specifier indicating which eddy covariance platform data are processed. Should be either "airc" or "towr". Defaults to "airc". [-]}

\item{\code{flagCh4}}{A logical indicating whether or not methane flux is processed. Defaults to TRUE. [-]}

\item{\code{vrbs}}{Logical. Default true. When FALSE supresses warnings when calculating rptStna01 and rptStna02.}

\item{\code{Thsh}}{Threshold value to indicate if quality flag will raise high (qfItc = 1) when the calculated quality indicator values are greater the threshold. Defaults as 100. [percent]}

\item{\code{Perc}}{A logical indicatiing whether or not to output the quality indicator in the unit of percentage or fraction. Defaults to TRUE. [-]}

\item{\code{...}}{Passes additonal arguments to REYNflux. For example pass spcs and rmm when calculating chemistry fluxes. [-]}
}
\value{
The returned object consists of:\cr
\code{qiStnaTrnd} Stationarity test results (quality indicator) when calculating using trend method (Vickers and Mahrt, 1997).
\code{qiStnaSubSamp} Stationarity test results (quality indicator) when calculating using internal stationarity method (Foken and Wichura, 1996).
\code{qfStna} Stationarity test quality flag.
}
\description{
Function defintion. Stationarity tests based on Vickers and Mahrt (1997) and Foken and Wichura (1996).
}
\examples{
Will provide in the future.
}
\references{
Foken, T. and Wichura, B.: Tools for quality assessment of surface-based flux measurements, Agricultural and Forest Meteorology, 78, 83-105, (1996) \cr
Vickers, D. and Mahrt, L.: Quality control and flux sampling problems for tower and aircraft data, Journal of Atmospheric and Oceanic Technology, 14, 512-526, 1997. \cr
}
\seealso{
Currently none
}
\author{
Stefan Metzger \email{eddy4R.info@gmail.com} \cr
Natchaya Pingintha-Durden
}
\keyword{eddy-covariance,}
\keyword{flux}
\keyword{stationarity,}
\keyword{turbulent}
