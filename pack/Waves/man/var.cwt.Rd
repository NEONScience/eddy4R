\name{var.cwt}
\alias{var.cwt}
\title{Variance of Continuous Wavelet Transform}
\description{\code{var.cwt} aggregates the spectrum of a continuous wavelet
  transform to (an estimate of) the variance.}
\usage{var.cwt(cwt, mask.coi = FALSE, aggregate = sum)}
\arguments{
  \item{cwt}{an object of class \code{CWT}.}
  \item{mask.coi}{logical.  Should values outside the cone of influence
    be treated as \code{NA}?}
  \item{aggregate}{a function to do the aggregation.  Must accept a
    single unnamed matrix argument, and understand \code{na.rm}.  Typical
    instances are \code{sum} for total aggregation, \code{rowSums} for
    aggregation in time domain, and \code{colSums} for aggregation in
    frequency domain.}
}
\value{
  The value depends on the \code{aggregate} function, which is applied
  to a matrix.
}
\references{
  C. Torrence and G.P. Compo (1998): \cite{A practical guide to wavelet
    analysis}.  Bull. Amer. Meteor. Soc. 79, P. 61-78
}
\author{
  Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}
}
\seealso{
  \code{\link{cwt}}, \code{\link{cov.cwt}}
}
\examples{
data(fanfare)
fanfarecwt <- cwt(fanfare, wavelet = morlet())

# Compare variance of data set and its continuous wavelet transform
var(fanfare)
var.cwt(fanfarecwt, mask.coi = FALSE)
var.cwt(fanfarecwt, mask.coi = TRUE)

# Plot variance distribution in time domain
plot(fanfarecwt@time, var.cwt(fanfarecwt, aggregate = rowSums),
     xlab = "Time [s]", ylab = "Variance", type = "l")

# Plot variance distribution in frequency domain
plot(1 / fanfarecwt@period, var.cwt(fanfarecwt, aggregate = colSums),
     xlab = "Frequency [Hz]", ylab = "Variance", type = "l")
}
\keyword{array}
\keyword{univar}
