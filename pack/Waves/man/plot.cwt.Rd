\name{plot.cwt}
\alias{plot.cwt}
\alias{plot}
\title{Plot Continuous Wavelet Transform}
\description{
  \code{plot.cwt} plots continuous wavelet scalograms with many
  options.
}
\usage{
plot.cwt(cwt, cwt2 = NULL,
         nlevels = 20L, contour = TRUE,
         shortage = max(1, ceiling(nrow(cwt)/1000)), shortop = mean,
         tfactor = 1, tunit = NULL,
         ar = NULL, coi = TRUE, global = is.null(cwt2),
         palette = function(nlevels) rev(rainbow(nlevels, start = 0, end
= 4/6)),
         period.weight = NULL, rescale.method = "linear", ...)
}
\arguments{
  \item{cwt}{An object of class \code{\linkS4class{CWT}}.}
  \item{cwt2}{An object of class \code{\linkS4class{CWT}}.
    Cross-scalogram of \code{cwt} and \code{cwt2} is plotted if not
    \code{NULL}, single scalogram of \code{cwt} otherwise.}
  \item{nlevels}{integer.  Number of levels of spectral intensity in plot.}
  \item{contour}{logical.  Use contour plot instead of raster image?}
  \item{shortage}{integer.  Aggregation factor for spectrum in time domain for more
    efficient plotting.}
  \item{shortop}{A function to aggregate spectrum in time domain.
    Should map a numeric vector to a single value.  Typical candidates
    are \code{mean} and \code{median}.}
  \item{tfactor}{numeric.  Conversion factor for time domain.}
  \item{tunit}{character.  Time unit, or \code{NULL} for unspecified.}
  \item{ar}{numeric.  Coefficient for null hypothesis AR(1)-process.
    Estimate from the data is used instead if \code{NULL}.}
  \item{coi}{logical.  Display cone of influence?}
  \item{global}{logical.  Display global wavelet?}
  \item{palette}{A function of the form \code{function (nlevels)} that
    yields a vector of length \code{nlevels} of colours.}
  \item{period.weight}{A weight function for period-dependent coloring.
    Should map a numeric vector to a numeric vector of the same length.
    Use \code{function(s) 1/s} for amplitude equalization.  No weighting
    if \code{NULL}.}
  \item{rescale.method}{character.  One of the following:
    \describe{
      \item{\code{"linear"}}{Linear color scaling.}
      \item{\code{"log"}}{Logarithmic color scaling.}
      \item{\code{"quantile"}}{Equal-area color scaling.}
    }
  }
  \item{\dots}{Additional graphical parameters passed to the underlying
    call of \code{plot}.  Attempts to specify \code{col} are ignored.
    For \code{xlab}, \code{ylab} and \code{main}, default values apply
    if not explicitly specified.}
}
\details{
  Under construction.  Features may change significantly in future
  versions.
}
\author{
  Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}
}
\seealso{
  \code{\link{cwt}}
}
\examples{
data(fanfare)
fanfarecwt <- cwt(fanfare, morlet(6))
plot(fanfarecwt, shortage = 15, nlevels = 32, rescale.method = "log",
     period.weight = function (s) 1/s, tunit = "s")
}
