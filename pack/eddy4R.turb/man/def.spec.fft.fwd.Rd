% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.spec.fft.fwd.R
\name{def.spec.fft.fwd}
\alias{def.spec.fft.fwd}
\title{Definition function: Fast Fourier transform}
\usage{
def.spec.fft.fwd(
  time,
  data,
  veloRltv,
  distZaxsMeas,
  FreqSamp,
  MethMeanRmv = FALSE,
  MethTrndRmv = TRUE,
  WghtTape = 0.05
)
}
\arguments{
\item{time}{continuous timestamp in any float format; only for gapfilling}

\item{data}{continuous matrix of variables to be transformed with same length as tstamp; NAs are interpolated}

\item{veloRltv}{relative motion between observation platform and atmosphere with same length as tstamp; NAs are interpolated. E.g. |wind vector| for tower observation [m s-1]}

\item{distZaxsMeas}{measurement height, either single number or vector of same length as tstamp}

\item{FreqSamp}{observation frequency: single integer [Hz]}

\item{MethMeanRmv}{logical to determine if the data should be demeaned}

\item{MethTrndRmv}{logical to determine if the data should be detrended}

\item{WghtTape}{taper the data for a fraction (<=0.5) at both ends, or FALSE}
}
\value{
A list (\code{rpt}) of transformed data variables in frequency space and calculation of various independent variables sorted from lowest to highest frequency
}
\description{
Fast Fourier transform.
}
\examples{
Currently none
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007
}
\seealso{
Currently none
}
\author{
Stefan Metzger \email{eddy4R.info@gmail.com}

David Durden
}
\keyword{FFT,}
\keyword{Fast}
\keyword{Fourier}
\keyword{Transform,}
\keyword{spectral}
