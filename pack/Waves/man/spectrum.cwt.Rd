\name{spectrum.cwt}
\alias{spectrum.cwt}
\title{Extract Spectrum from CWT Object}
\description{\code{spectrum.cwt} extracts the spectrum matrix
  (scalogram data) from a \code{\linkS4class{CWT}} object, optionally
  clipping at the cone of influence.}
\usage{spectrum.cwt(cwt, mask.coi = FALSE)}
\arguments{
  \item{cwt}{a \code{\linkS4class{CWT}} object.}
  \item{mask.coi}{logical.  Should entries outside the cone of influence
  be masked out (set to \code{NA})?}
}
\value{
  A matrix specifying the spectral power of the CWT.  Rows are time
  domain, columns are frequency domain.
}
\author{
  Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}
}
\examples{
data(fanfare)
fanfarecwt <- cwt(fanfare, haar)
summary(spectrum.cwt(fanfarecwt, mask.coi = TRUE))
}
\keyword{array}
