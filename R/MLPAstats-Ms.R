getInfo<-function()
 {
  ll<-length(names(.GlobalEnv$Ms))
  cat("\n")
  cat("Status of Ms object \n")
  cat("------------------- \n")
  if (ll>=1)
   cat("  Data loaded ...\n")
  if (ll>=2)
   cat("  Data normalized ...\n")
  if (ll>=3)
   cat("  Data analized ... \n")
 }


getProbes<-function()
 {
  names(.GlobalEnv$Ms$mlpa$arg.mlpa$test.dat)[-c(1:2)]
 }


getPeaks<-function(name.probe, individuals="controls")
 {
  if (missing(name.probe))
   stop("Indicate the name of the probe")

  if (!name.probe%in%getProbes())
   stop(paste(name.probe,"is not a valid probe"))

  ind<-charmatch(individuals,c("controls","tests"))
  if(ind==0)
   stop("'individuals' should be 'controls' or 'tests'")

  if (ind==1)
   ans<-.GlobalEnv$Ms$mlpa$arg.mlpa$cont.dat[name.probe]
  if (ind==2)
   ans<-.GlobalEnv$Ms$mlpa$arg.mlpa$test.dat[name.probe]

  ans
 
 }

getSize <- function()
 {
  .GlobalEnv$Ms$mlpa$arg.mlpa$size   
 }

getProbesControl <- function()
 {
  getProbes()[.GlobalEnv$Ms$mlpa$arg.mlpa$probe]   
 }


getConfig<-function()
 {
  .GlobalEnv$Ms$analysis$analysis.config
 }

getNormalize<-function()
 {
  .GlobalEnv$Ms$norm$norm.dat
 }


getResults<-function()
 {
  .GlobalEnv$Ms$analysis$analysis.res
 }

