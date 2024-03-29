% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrap.dp01.qfqm.ecte.R
\name{wrap.dp01.qfqm.ecte}
\alias{wrap.dp01.qfqm.ecte}
\title{Wrapper function: Create NEON Level 1 data product quality flags and quality metrics across list elements}
\usage{
wrap.dp01.qfqm.ecte(
  qfqm,
  idx = NULL,
  TypeMeas = "samp",
  MethMeas = c("ecte", "ecse")[1],
  RptExpd = FALSE
)
}
\arguments{
\item{qfqm}{A data.frame or list containing the L0p input data quality flags (sensor and plausibility flags) at native resolution. Of type numeric or integer. [-]}

\item{idx}{If data is a list, which list entries should be processed into Level 1 data product quality metrics? Defaults to NULL which expects qfqm to be a data.frame. Of type character. [-]}

\item{TypeMeas}{A vector of class "character" containing the name of measurement type (sampling or validation), TypeMeas = c("samp", "ecse"). Defaults to "samp". [-]}

\item{MethMeas}{A vector of class "character" containing the name of measurement method (eddy-covariance turbulent exchange or storage exchange), MethMeas = c("ecte", "ecse"). Defaults to "ecse". [-]}

\item{RptExpd}{A logical parameter that determines if the full quality metric \code{qm} is output in the returned list (defaults to FALSE).}
}
\value{
A list of: \cr
\code{qm}  A list of data frame's containing quality metrics (fractions) of failed, pass, and NA for each of the individual flag which related to L1 sub-data products if RptExpd = TRUE. [fraction] \cr
\code{qmAlph} A dataframe containing metrics in a  columns of class "numeric" containing the alpha quality metric for L1 sub-data products. [fraction] \cr
\code{qmBeta} A dataframe containing metrics in a columns of class "numeric" containing the beta quality metric for L1 sub-data products. [fraction] \cr
\code{qfFinl} A dataframe containing flags in a columns of class "numeric", [0,1], containing the final quality flag for L1 sub-data products. [-] \cr
\code{qfSciRevw} A dataframe containing flags in a columns of class "numeric", [0,1], containing the scientific review quality flag for L1 sub-data products. [-] \cr
}
\description{
Wrapper function. Compute NEON Level 1 data product quality flags and quality metrics (qfFinl, qfSciRevw, qmAlph, qmBeta) across list elements.
}
\examples{
#generate the fake quality flags for each sensor
TimeBgn <- "2016-04-24 02:00:00.000"
TimeEnd <- "2016-04-24 02:29:59.950"
qf <- list()
qf$irgaTurb <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "irgaTurb", PcntQf = 0.05)
qf$mfcSampTurb <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "mfcSampTurb", PcntQf = 0.05)
qf$soni <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 20, Sens = "soni", PcntQf = 0.05)
qf$amrs <- eddy4R.qaqc::def.qf.ecte(TimeBgn = TimeBgn, TimeEnd = TimeEnd, Freq = 40, Sens = "amrs", PcntQf = 0.05)
#calculate quality metric, qmAlpha, qmBeta, qfFinl
qfqm <- list()
qfqm <- wrap.dp01.qfqm.ecte(qfqm = wrk$qfqm, idx = c("soni", "amrs", "co2Turb", "h2oTurb") )
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
\keyword{NEON}
\keyword{QAQC,}
\keyword{flags,}
\keyword{metrics}
\keyword{quality}
