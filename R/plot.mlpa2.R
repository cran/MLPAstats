`plot.mlpa2` <-
function(x,ind,tit=TRUE,...)
  {

  probes.control<-attr(attr(x,"info"),"peaks")$probes.control
  lab<-attr(attr(attr(x,"info"),"peaks"),"probes")
  name.indiv<-attr(attr(attr(x,"info"),"peaks"),"individuals")[ind]
 
  x<-x[[ind]]

  my.color<- ifelse(c(1:length(x$x))%in%probes.control,"gray50","gray75")

  plot(x$x,x$y,type="n",
     xlab="relative peak heights in controls",
     ylab="relative peak heights in test")
  if(tit)
   title(name.indiv)

  points(x$x,x$y,col=my.color,pch=19,cex=1.2)
  segments(min(x$x),min(x$x)*x$coef,max(x$x),max(x$x)*x$coef,lty=2)
  lines(lowess(x$x,x$bands[,1]))
  lines(lowess(x$x,x$bands[,2]))
  conf.limit<-round((1-x$level)*2*100,1)

#  ll<-par("usr")
#  text(ll[2],ll[3]+0.03,paste("Confidence limit: ",conf.limit,"%","    s.e. of the regression: ",round(100*x$se.reg,2),sep=""),cex=0.7,adj=1)

  tt<-paste("Confidence limit: ",conf.limit,"%","    s.e. of the regression: ",round(100*x$se.reg,2),sep="")
  legend("bottomright",tt,cex=0.7,bty="n")


  if (any(x$marker.sig))
   {
    text(x$x[x$marker.sig],x$y[x$marker.sig],
         lab[x$marker.sig],cex=0.7,font=2,xpd=TRUE) 
   }

  legend("topleft",c("Control probes","Analytical Probes","Boundary"),lty=c(NA,NA,1),pch=c(19,19,NA),
        col=c("gray50","gray75","black"),cex=0.8,
        y.intersp=0.9,bty="n")
}

