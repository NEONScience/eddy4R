\name{compatible.cwt}
\alias{compatible.cwt}
\title{
  Compatibility Check for Pair of Continuous Wavelet Transforms
}
\description{
  \code{compatible.cwt} checks two objects of class
  \code{\linkS4class{CWT}} for compatibility.  Direct comparisons of
  spectra and co-spectral calculations, such as covariance, only make
  sense for compatible objects.
}
\usage{
compatible.cwt(cwt1, cwt2, raise.error = FALSE)
}
\arguments{
  \item{cwt1}{An object of class \code{\linkS4class{CWT}}.}
  \item{cwt2}{An object of class \code{\linkS4class{CWT}}.}
  \item{raise.error}{logical.  Should an error be raised if the
    arguments are incompatible?}
}
\details{
  The following items are checked for compatibility:
  \enumerate{
    \item the dimensions of the \code{spectrum} slot,
    \item the value of the \code{dt} and \code{dj} slots,
    \item the value of the \code{cdelta} slot of the \code{wavelet} object.
  }
}
\value{
  A logical value.  For \code{raise.error = TRUE}, only an invisible
  \code{TRUE} can be returned successfully.
}
\author{
  Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}
}
\seealso{
  \code{\link{cov.cwt}}
}
\examples{
transf <- function(wavelet) cwt(rnorm(1000), wavelet)
compatible.cwt(transf(dog(2)), transf(mexicanHat)) # synonymous
compatible.cwt(transf(haar), transf(morlet()))
try(compatible.cwt(transf(haar), transf(morlet()),
    raise.error = TRUE))                           # raises error
}
