\name{MLPAstats-internal}
\alias{plot.mlpa0}
\alias{plot.mlpa0.i}
\alias{plot.mlpa1}
\alias{plot.mlpa1.i}
\alias{plot.mlpa2}
\alias{plot.mlpa3}
\alias{plot.mlpaNorm.i}
\alias{slope.correction}
\alias{iterReg}
\alias{iterReg.i}
\alias{medianFilter}
\alias{exponential.correction}
\alias{bootBands}
\alias{toleranceInterval}
\alias{get.file.menu}
\alias{get.normalize.menu}
\alias{get.mlpa.menu}
\alias{get.results.menu}
\alias{gui.sel.var.nm}
\alias{getThresholds}

\title{Internal MLPAstats functions}

\description{Internal MLPAstats functions}

\usage{
\method{plot}{mlpa0}(x, ratio=TRUE, threshold, ylim, ...)
\method{plot}{mlpa0.i}(x, ind, ratio, thresholds, ylim, ...)
\method{plot}{mlpa1}(x, ylim = c(0.7, 1.3), ...)
\method{plot}{mlpa1.i}(x, ind, ylim, ...)
\method{plot}{mlpa2}(x, ind, tit = TRUE, ...)
\method{plot}{mlpa3}(x, id, adj = 0.1, ...)


slope.correction(y, x, sel.reg, robust, quadratic = TRUE)
iterReg(x, y, sel.control, type.method, type.bands, level, B, maxit)
iterReg.i(x, y, sel.control, type, level)
medianFilter(x, sliding)
exponential.correction(x, control, probes.control, sliding, transf, transf.inv)
bootBands(x, y, sel.control, type.method, B, level)
toleranceInterval(x, gamma, alpha, k)

get.file.menu()
get.normalize.menu()
get.results.menu()
gui.sel.var.nm(probe.list) 
getThresholds(x)

}

\details{These are not to be called by the user}

\keyword{internal}
