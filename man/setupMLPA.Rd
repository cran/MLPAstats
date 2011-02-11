\name{setupMLPA}
\alias{setupMLPA}
\alias{print.setupMLPA}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Sets up data for MLAstats analysis }
\description{
 Sets up the data ready for a normalization, from 
}
\usage{
setupMLPA(data.controls, data.cases, size.probes, reference.probes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.controls}{ contains data for control subjects }
  \item{data.cases}{ data for test subjects }
  \item{size.probes}{ provides size of the probes }
  \item{reference.probes}{ numberig of the probes to be taken as internal control for normalization }
}
\details{
   Input data should be available in the R session for an analysis from the command line. 
   Details on the format are given the the user manual, type vignette("MLPAstats") 
   for more information. 
}
\value{
  object of class ''setupMLPA´´.
}

\examples{
  data(MLPAvalidation)
  mlpa.dat <- setupMLPA(controls, cases, size, reference.probes)
}
\keyword{utilities}
