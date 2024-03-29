% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.hdf5.read.qfqm.R
\name{def.hdf5.read.qfqm}
\alias{def.hdf5.read.qfqm}
\title{Wrapper function: Reading quality flags from NEON HDF5 files}
\usage{
def.hdf5.read.qfqm(
  DirInpLoca,
  SiteLoca,
  DateLoca,
  VarLoca,
  LvlTowr = c("000_040", "000_050", "000_060")[3],
  FreqLoca,
  DataType = c("data", "qfqm")[1],
  MethMeas = c("ecte", "ecse")[1]
)
}
\arguments{
\item{DirInpLoca}{Character: Input directory.}

\item{SiteLoca}{Character: Site location.}

\item{DateLoca}{Character: Date in ISO format "(2016-05-26").}

\item{VarLoca}{Character: Which instrument to read data from.}

\item{LvlTowr}{The tower level that the sensor data is being collected in NEON data product convention (HOR_VER)}

\item{FreqLoca}{Integer: Measurement frequency.}

\item{DataType}{Character: Specify between data and qfqm for read in.}

\item{MethMeas}{A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecte".}
}
\value{
Named list \code{rpt} containing time-series of quality flags.
}
\description{
definition function. Reads an HDF5 input file in NEON standard format from \code{DirInpLoca}.
}
\examples{
Currently none.
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
}
\seealso{
Currently none.
}
\author{
David Durden \email{ddurden@battelleecology.org}
}
\keyword{QAQC,}
\keyword{diagnostic}
\keyword{file,}
\keyword{flags}
\keyword{flags,}
\keyword{pre-processing,}
\keyword{quality}
\keyword{read,}
