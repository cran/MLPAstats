pkgname <- "MLPAstats"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('MLPAstats')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("BRCA")
### * BRCA

flush(stderr()); flush(stdout())

### Name: BRCA
### Title: BRCA dataset for MLPA analysis
### Aliases: BRCA BRCAcontrols BRCAtests
### Keywords: datasets

### ** Examples

data(BRCA)



cleanEx()
nameEx("MLPAstats-package")
### * MLPAstats-package

flush(stderr()); flush(stdout())

### Name: MLPAstats-package
### Title: MLPAstats
### Aliases: MLPAstats-package MLPAstats
### Keywords: package

### ** Examples

gui.mlpa()



cleanEx()
nameEx("MLPAvalidation")
### * MLPAvalidation

flush(stderr()); flush(stdout())

### Name: MLPAvalidation
### Title: MLPA dataset for validation study
### Aliases: MLPAvalidation controls tests size probes.control
### Keywords: datasets

### ** Examples

data(MLPAvalidation)



cleanEx()
nameEx("gui.mlpa")
### * gui.mlpa

flush(stderr()); flush(stdout())

### Name: gui.mlpa
### Title: GUI for MLPAstats
### Aliases: gui.mlpa
### Keywords: gui

### ** Examples

## Not run: 
##D ## Start the GUI
##D gui.mlpa()
## End(Not run)




cleanEx()
nameEx("mlpa")
### * mlpa

flush(stderr()); flush(stdout())

### Name: mlpa
### Title: mlpa - inference
### Aliases: mlpa plot.mlpa print.mlpa
### Keywords: models

### ** Examples

#see 
#vignette(MLPAstats)



cleanEx()
nameEx("mlpaNorm")
### * mlpaNorm

flush(stderr()); flush(stdout())

### Name: mlpaNorm
### Title: Normalization of MLPA data
### Aliases: mlpaNorm plot.mlpaNorm
### Keywords: models

### ** Examples

data(MLPAvalidation)
mlpa.dat <- setupMLPA(controls, tests, size, probes.control)
norm.dat <- mlpaNorm(mlpa.dat, method = "sum.peaks.controls")



cleanEx()
nameEx("setupMLPA")
### * setupMLPA

flush(stderr()); flush(stdout())

### Name: setupMLPA
### Title: Sets up data for MLAstats analysis
### Aliases: setupMLPA print.setupMLPA
### Keywords: utilities

### ** Examples

  data(MLPAvalidation)
  mlpa.dat <- setupMLPA(controls, tests, size, probes.control)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
