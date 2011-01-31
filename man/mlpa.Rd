\name{mlpa}

\alias{mlpa}
\alias{plot.mlpa}
\alias{print.mlpa}

\title{mlpa - inference}
\description{
 Performs inference of the dosage ratios between
test and control samples with a variety of methods
}
\usage{
mlpa(x, method, threshold = c(0.7, 1.33), gamma = 0.95, alpha = 0.01, id = 0.05, bands = "parametric", maxit = 100, B = 200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{normalized data}
  \item{method}{method to be used for the inference: c("threshold", "mixed-model", "outlier", "REX-MLPA")}
  \item{threshold}{ threshold for significant differences }
  \item{gamma}{XXX}
  \item{alpha}{XXX}
  \item{id}{XXX}
  \item{bands}{XXX}
  \item{maxit}{XXX}
  \item{B}{XXX}
}
\details{
Before performing inferences normalization of the data is required. 
}
\value{
Object of class ''mlpa´´.
}

\seealso{\code{\link{mlpaNorm}},  }
\examples{
#see 
#vignette(MLPAstats)
}
\keyword{models}
