\name{morlet}
\alias{morlet}
\title{
  Morlet Wavelet
}
\description{
  The Morlet family of wavelets provides excellent frequency-domain
  resolution but poor time-domain resolution.
}
\usage{
morlet(mod = 6)
}
\arguments{
  \item{mod}{numeric.  Modulation parameter, typical values are >5.}
}
\value{
  A new object of a subclass of \code{\linkS4class{Wavelet}}.
}
\references{
  C. Torrence and G.P. Compo (1998): \cite{A practical guide to
    wavelet analysis}.  Bull. Amer. Meteor. Soc. 79, P. 61-78
}
\author{
  Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}
}
\seealso{
  \code{\link{haar}},
  \code{\link{dog}},
}
\examples{
plot(morlet(6))
}
