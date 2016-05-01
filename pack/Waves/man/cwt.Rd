\name{cwt}
\alias{cwt}
\title{Continuous Wavelet Transform}
\description{\code{cwt} performs a continuous wavelet transform.}
\usage{cwt(x, wavelet,
    scale0 = minscale.cwt(x, wavelet),
    dj = 0.25,
    jmax = nscale.cwt(x, wavelet, scale0, dj))}
\arguments{
  \item{x}{an vector or object of class \code{ts} to transform.
    Implicitly coerced to \code{ts}.}
  \item{wavelet}{an object of class \code{\linkS4class{Wavelet}} to specify the
    transformation.}
  \item{scale0}{the minimum scale.}
  \item{dj}{scale increment, logarithmically with base 2.}
  \item{jmax}{number of scale increments.}
}
\details{Scales are chosen as \code{scale0 * 2^(j * dj)}, for \code{j in
    0:jmax}.}
\value{An object of class \code{\linkS4class{CWT}} containing the results of the transform.}
\references{C. Torrence and G.P. Compo (1998): \cite{A practical guide to
  wavelet analysis}.  Bull. Amer. Meteor. Soc. 79, P. 61-78}
\author{Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}}
\seealso{\code{\link{var.cwt}}, \code{\link{cov.cwt}}, \code{\link{recon.cwt}}}
\examples{
  cwt(ts(rnorm(1000), freq=365), morlet(6), dj = 1/8)
}
\keyword{array}
