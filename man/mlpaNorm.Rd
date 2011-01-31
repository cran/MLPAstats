\name{mlpaNorm}
\alias{mlpaNorm}
\alias{plot.mlpaNorm}

\title{ Normalization of MLPA data  }
\description{
Experimental conditions and differences across probes (due to size and
nature) can introduce systematic intensity variations that must be accounted
for. In this function, we have implemented four different methods to normalize
the intensities of the probes.
}
\usage{
mlpaNorm(x, method = "sum.peaks.controls", probes.control = TRUE, replicate = FALSE, transform = "none", sliding = 9, quadratic = TRUE, robust = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ object of class ''setupMLPA´´. }
  \item{method}{ type of method fot normalization, c("nonlinear", "slope.correction", "sum.peaks.controls", "sum.peaks.all") }
  \item{probes.control}{ whether control probes should be used. }
  \item{replicate}{ whether replicates are given. }
  \item{transform}{ a choice from: c("none", "logarithm","sqrt") }
  \item{sliding}{ default=9. }
  \item{quadratic}{ whether quadratic structure shold be fitted. }
  \item{robust}{ whether robust regression should be used. }
}
\details{
  Data for nomralization must be first set up via setupMLPA() 
}
\value{
Object of class mlpaNorm, that can be passes to mlpa() for performing inferences.
}


\seealso{\code{\link{setupMLPA}},  }
\examples{
data(MLPAvalidation)
mlpa.dat <- setupMLPA(controls, tests, size, probes.control)
norm.dat <- mlpaNorm(mlpa.dat, method = "sum.peaks.controls")
}
\keyword{models}
