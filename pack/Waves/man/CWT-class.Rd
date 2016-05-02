\name{CWT-class}
\Rdversion{1.1}
\docType{class}
\alias{CWT-class}
\alias{plot,CWT,missing-method}

\title{Class \code{"CWT"}}
\description{
  A class to encapsulate the results of a continuous wavelet transform.
}
\section{Objects from the Class}{
  Objects are created as results of the continuous wavelet transform
  function (\code{\link{cwt}}).
}
\section{Slots}{
  \describe{
    \item{\code{series}:}{Object of class \code{"ts"} containing the
      original data.}
    \item{\code{spectrum}:}{complex matrix.  Results of convolution of
      data and wavelet.  Rows are time domain, columns are frequency
      domain.}
    \item{\code{scale}:}{numeric.  Vector of actually used scale
      parameter values.}
    \item{\code{period}:}{numeric.  Same as \code{scale} but expressed
      as (Fourier-equivalent) period length.}
    \item{\code{time}:}{numeric.  Vector of time points.}
    \item{\code{dt}:}{numeric.  Resolution in time domain.}
    \item{\code{dj}:}{numeric.  Resolution (logarithmic base 2) in
      frequency domain.}
    \item{\code{mean}:}{numeric.  Mean value of data, subtracted before
      convolution.}
    \item{\code{wavelet}:}{Object of class \code{"Wavelet"} used for the
      transform.}
    \item{\code{coi}:}{numeric.  Vector of critical period lengths for
      cone of influence, for all time points.}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "CWT", y = "missing")}: plot the
      object with \code{\link{plot.cwt}}.}
  }
}
\author{Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}}
\seealso{
  \code{\linkS4class{Wavelet}}, \code{\link{cwt}}
}
\examples{
showClass("CWT")
}
\keyword{classes}
