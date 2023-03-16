\name{recon.cwt}
\alias{recon.cwt}
\title{
  Reconstruction from Continuous Wavelet Transform
}
\description{
  \code{recon.cwt} reconstructs data from the continuous wavelet transform.
}
\usage{
recon.cwt(cwt, mask.coi = FALSE)
}
\arguments{
  \item{cwt}{An object of class \code{\linkS4class{CWT}}.}
  \item{mask.coi}{logical.  Should values outside the cone of influence
    be treated as \code{NA}?}
}
\details{
  The original data are included in objects of class
  \code{\linkS4class{CWT}}.  The reconstruction currently serves merely
  as a check; only the spectral contribution outside the cone of
  influence can be filtered out.  This function would be more useful
  with additional filtering capabilities.
}
\value{
  An object of class \code{ts} containing the reconstructed data.
}
\references{
  C. Torrence and G.P. Compo (1998): \cite{A practical guide to wavelet
    analysis}.  Bull. Amer. Meteor. Soc. 79, P. 61-78
}
\author{
  Baltasar Trancon y Widemann \email{Baltasar.Trancon@uni-bayreuth.de}
}
\seealso{
  \code{\link{var.cwt}}
}
\examples{
data(fanfare)
fanfarecwt <- cwt(fanfare, morlet(6))
plot(fanfare)
lines(recon.cwt(fanfarecwt) - fanfare, ylim=c(-1,1), col="red")
}
