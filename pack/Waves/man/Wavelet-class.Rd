\name{Wavelet-class}
\Rdversion{1.1}
\docType{class}
\alias{Wavelet-class}
\alias{daughter,Wavelet,numeric,missing,numeric-method}
\alias{plot,Wavelet,missing-method}

\title{Class \code{"Wavelet"}}
\description{
  Abstract base class for wavelets.
}
\section{Objects from the Class}{
  Concrete wavelets should be objects of subclasses of \code{Wavelet}.
  Any subclass should set all the slots to meaningful values, and
  implement the methods \code{mother} and \code{daughter} indicated
  below.  Only \code{mother} is required for plotting, and only
  \code{daughter} is required for a continuous wavelet transform.
}
\section{Slots}{
  \describe{
    \item{\code{tnorm}:}{complex.  Normalization factor in time domain.}
    \item{\code{fnorm}:}{complex.  Normalization factor in frequency domain.}
    \item{\code{efold}:}{numeric.  e-folding time; used for calculation
      of cone of influence.}
    \item{\code{fourier}:}{numeric.  Fourier-equivalent frequency.  Peak
      at scale \code{s} corresponds to effective frequency \code{fourier
      * s}.}
    \item{\code{cdelta}:}{numeric.  Reconstruction factor.}
    \item{\code{df}:}{numeric.  Degrees of freedom; used for
      significance testing against AR(1) process.  Should be \code{1}
      for real-valued wavelets, \code{2} for complex-valued wavelets.}
  }
}
\section{Methods}{
  \describe{
    \item{mother}{\code{signature(wavelet = "Wavelet.Morlet",
	time = "numeric")}: Mother wavelet function in time domain.
      Must be implemented for each concrete wavelet class.}
    \item{daughter}{\code{signature(wavelet = "Wavelet", scale =
	"numeric", freq = "missing", time = "numeric")}: Daughter wavelet
      function in time domain.  Is implemented generically in terms of
      \code{mother} for all concrete wavelet classes.}
    \item{daughter}{\code{signature(wavelet="Wavelet.Morlet",
	scale = "numeric", freq = "numeric", time = "missing")}:
      Daughter wavelet function in frequency domain.  Must be
      implemented for each concrete wavelet class.}
    \item{plot}{\code{signature(x = "Wavelet", y = "missing")}: Plot the
      wavelet; real part with solid line, imaginary part (if any) with
      dashed line.}
  }
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
  \code{\link{morlet}},
  \code{\link{dog}},
  \code{\link{mexicanHat}},
  \code{\linkS4class{CWT}}
}
\examples{
showClass("Wavelet")
}
\keyword{classes}
