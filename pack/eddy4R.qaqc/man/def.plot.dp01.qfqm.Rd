% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/def.plot.dp01.qfqm.R
\name{def.plot.dp01.qfqm}
\alias{def.plot.dp01.qfqm}
\title{Definition function: Plot quality flags and quality metrics (basic L1 data products)}
\usage{
def.plot.dp01.qfqm(
  dataDp01,
  WndwTime = c(min(dataDp01$timeAgrBgn), max(dataDp01$timeAgrBgn)),
  NameQmPlot = sub("Pass", "", names(dataDp01$dataAgr[[1]][grep("Pass",
    names(dataDp01$dataAgr[[1]]))]))
)
}
\arguments{
\item{dataDp01}{Required input. A list output from wrap.dp01.qfqm.R of: \cr
timeAgrBgn - the starting time stamp of aggregated L1 data and quality metrics \cr
timeAgrEnd - the ending time stamp (non-inclusive) of aggregated L1 data and quality metrics \cr
dataAgr - a list of named variables, each containing a data frame of the time-aggregated mean, minimum, maximum, variance, number of points going into the average, standard error of the mean, and quality metrics (pass, fail, NA) pertaining to that variable for each flag in flgs, as well as the alpha & beta quality metrics and final quality flag. It is important that the column names of this data frame are indistinguishable from those that would be created from wrap.dp01.qfqm.R}

\item{WndwTime}{Optional. A 2-element POSIXlt vector of the minimum and maximum time range to plot. Default is the entire data range.}

\item{NameQmPlot}{Optional. A character vector listing the individual quality metrics to plot. The strings in this vector can be partial names, i.e. a partial match will result in the quality metric being plotted (ex. NameQmIndiv <- c("Step") will result in the quality metrics "qmStepPass","qmStepFail" and "qmStepNa" to be plotted). Default is all QMs.}
}
\value{
Running this function will output 3 plots per data variable: 1) basic L1 statistics (mean, min, max, etc). 2) Pass, Fail, and NA quality metrics for ever flag, 3) the final Alpha and Beta quality metrics and the final quality flag.
}
\description{
Function definition. Plots the aggregated quality flags, quality metrics and final quality flag for basic L1 (time window averaged) data products as output from wrap.dp01.qfqm.R.
}
\examples{
Currently none
}
\references{
License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007. \cr
NEON Algorithm Theoretical Basis Document: Quality Flags and Quality Metrics for TIS Data Products (NEON.DOC.001113)
}
\seealso{
Currently none
}
\author{
Cove Sturtevant \email{eddy4R.info@gmail.com }
}
\keyword{L1}
\keyword{NEON}
\keyword{QAQC,}
\keyword{and}
\keyword{average,}
\keyword{final}
\keyword{flag}
\keyword{flags}
\keyword{metrics,}
\keyword{quality}
