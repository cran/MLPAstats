`plot.mlpa3` <-
function(x,id,adj=0.1,...)
 {
  nprobes<-length(attr(attr(x$info,"peaks"),"probes"))
  ntest<-length(attr(attr(x$info,"peaks"),"individuals"))
  probes.control<-attr(x$info,"peaks")$probes.control
  mycol<-ifelse(1:nprobes%in%probes.control,"gray50","gray75")
  mycol2<-rep(mycol,ntest)
  plot(x$mod,I(1+resid(.))~as.numeric(gen) | control, 
           id=id,cex=0.7,ylab="Ratio",xlab="probes",
           abline=c(1,0),lty=2,
           subset=x$mod$data$control!="control",adj=adj,
           col.symbol=mycol2,pch=19,col.line="black",...)
 }

