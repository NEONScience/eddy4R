\name{dog}
\alias{dog}
\title{
  Derivative-of-Gaussian Wavelet
}
\description{
  The derivative-of-gaussian wavelet provides a balance between
  time-domain and frequency-domain resolution.
}
\usage{
dog(order = 2)
}
\arguments{
  \item{order}{integer.  The order of the derivative.  Odd orders yield
    asymmetric wavelets suitable for edge detection; even orders yield
    symmetric wavelets suitable for peak detection.  The argument value
    is coerced by \code{as.integer}.  Only nonnegative values are
    legal.}
}
\value{
  A new object of a subclass of \code{\linkS4class{Wavelet}}.
}
\author{
  Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}
}
\seealso{
  \code{\link{haar}},
  \code{\link{morlet}},
  \code{\link{mexicanHat}}
}
\examples{
plot(dog(1))
plot(dog(2))
}
