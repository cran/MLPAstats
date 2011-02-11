\name{BRCAcases}
\alias{BRCAcases}
\alias{BRCAcontrols}
\alias{referenceProbes}
\alias{BRCAsize}
\docType{data}
\title{
Sample text files required for the analysis of MLPA data. 
}
\description{
Data set in text file used to illustrate the analysis of the BRCA sample. 
BRCAcases and BRCAcontrols contain the intensities for the case and control samples, see format below.
The ``referenceProbes'' and the ``size'' files store the numbering of the reference probes and 
the sizes off all probes, respectivelly.
See the vignette for more details.  
}

\format{
Sample Replica RNAseP.1 HIRA UBE3A.9EX ...
NA.HMP05.a2 A 13220 9108 10644 ...
NA.HMP05.a2 B 13028 9891 10673 ...
...
}
\examples{
#the files are found in the directory
#path <- system.file("data", "BRCAcases.txt", package = "MLPAstats")
#path
}
\keyword{datasets}
