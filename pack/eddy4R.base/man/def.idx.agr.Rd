% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.idx.agr.R
\name{def.idx.agr}
\alias{def.idx.agr}
\title{Definition function: indices for aggregation periods}
\usage{
def.idx.agr(
  time,
  PrdAgr,
  FreqLoca,
  MethIdx = c("rglr", "specBgn", "specEnd")[1],
  crdH2oVali = FALSE,
  data = NULL,
  CritTime = 0
)
}
\arguments{
\item{time}{a vector of timestamps}

\item{PrdAgr}{the time period to aggregate to averaging in seconds (30 min = 1800 s) [s]}

\item{FreqLoca}{the frequency of the measurements that are being aggregated in hertz [Hz]}

\item{MethIdx}{a vector of class "character" containing the name of method used to determine the beginning and ending indicies. MethIdx = c("rglr", "specBgn", "specEnd"), where \cr
"rglr" is the regular method, e.g. for FreqLoca = 1 and PrdAgr = 1800, the first result of idxBgn and idxEnd are 1 and 1800, respectively. \cr
"specBgn" is the specific method to determine the beginning and ending indicies using the first indency when data is available. \cr
"specEnd" is the specific method to determine the beginning and ending indicies using the last indency when data is available. \cr
Defaults to "rglr". [-]}

\item{crdH2oVali}{a logical parameter indicating indices are being produced for a crdH2o during validation period.  (defaults to FALSE).}

\item{data}{a vector of input data which will be used to determine when data is available when "specBgn" or "specEnd" is selected. Defaults to NULL. [User-defined]}

\item{CritTime}{the critcal time to include before determine the beginning and ending indicies, e.g. CritTime = 60 for aggregation only the last 2 min from 3 min measurement time. Defaults to 0. [s]}
}
\value{
A dataframe of indices and corresponding times for aggregation periods.
}
\description{
Definition function to produce a dataframe of indices and corresponding times for aggregation periods.
}
\examples{
FreqLoca <- 20
timeMeas <- base::as.POSIXlt(seq.POSIXt(
  from = base::as.POSIXlt("2016-01-01 00:00:00.001", format="\%Y-\%m-\%d \%H:\%M:\%OS", tz="UTC"),
  to = base::as.POSIXlt("2016-01-01 04:59:59.952", format="\%Y-\%m-\%d \%H:\%M:\%OS", tz="UTC"),
  by = 1/FreqLoca), tz = "UTC")
PrdAgr <- 1800
def.idx.agr(time = timeMeas, PrdAgr = PrdAgr, FreqLoca = FreqLoca)
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007.
}
\seealso{
Currently none
}
\author{
Dave Durden \email{ddurden@battelleecology.org} \cr
Natchaya Pingintha-Durden \email{ndurden@battelleecology.org}
}
\keyword{NEON,}
\keyword{aggregation,}
\keyword{averaging}
\keyword{intervals}
