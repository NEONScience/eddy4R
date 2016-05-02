\name{cov.cwt}
\alias{cov.cwt}
\title{Covariance of Continuous Wavelet Transforms}
\description{\code{cov.cwt} aggregates the cross-spectrum of two
  continuous wavelet transforms to (an estimate of) the covariance.}
\usage{cov.cwt(cwt1, cwt2, mask.coi = FALSE, aggregate = sum)}
\arguments{
  \item{cwt1}{an object of class \code{CWT}.}
  \item{cwt2}{an object of class \code{CWT}.}
  \item{mask.coi}{logical.  Should values outside the cone of influence
    be treated as \code{NA}?}
  \item{aggregate}{a function to do the aggregation.  Must accept a
    single unnamed matrix argument, and understand \code{na.rm}.  Typical
    instances are \code{sum} for total aggregation, \code{rowSums} for
    aggregation in time domain, and \code{colSums} for aggregation in
    frequency domain.}
}
\value{
  The value depends on the \code{aggregate} function, which is
  applied to a matrix.
}
\references{
  C. Torrence and G.P. Compo (1998): \cite{A practical guide to
    wavelet analysis}.  Bull. Amer. Meteor. Soc. 79, P. 61-78
}
\author{
  Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}
}
\seealso{
  \code{\link{cwt}},
  \code{\link{var.cwt}}
}
\keyword{array}
\keyword{multivariate}
