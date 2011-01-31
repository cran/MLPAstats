`plot.mlpa0` <-
function(x, ratio=TRUE, threshold, ylim, ...)
 {

 # inherits

 x$test.norm<-t(x$test.norm)

 if (ratio)
  {
   diff<-x$test.norm/x$control.norm 
   if (missing(threshold)) {
    threshold<-c(0.7,1.33)
   }
   thresholds<-threshold
  }
 else
  {
   diff<-x$test.norm-x$control.norm 
   if (missing(threshold)) {
    threshold<-0.4  
   }
   thresholds<-c(-threshold,threshold)
  }
  
 lab.x<-attr(attr(x,"peaks"),"individuals")
  
 old.mar <- par("mar")
 old.mfrow <- par("mfrow")
 on.exit(par(mar = old.mar, mfrow = old.mfrow))
 n<-length(lab.x)+1
 m<-matrix(c(1:(2*n)),nrow=n,ncol=2,byrow=TRUE)
 layout(m, widths = c(0.2, 1))
 mar.new<-old.mar
 mar.new[1]<-0.5
 mar.new[2]<-1
 mar.new[3]<-0.5  
 par(mar = mar.new)
 if(missing(ylim))
    ylim<-c(min(diff,thresholds[1]),max(diff,thresholds[2]))

 for (i in 1:(n-1))
   {
      plot(c(0:10),ylim=ylim,type="n",axes=FALSE)
      if (ratio) 
        text(10,1,lab.x[i],adj=1,font=2)
      else
        text(10,0,lab.x[i],adj=1,font=2)
      plot.mlpa0.i(x,i, ratio, thresholds, ylim, ...)
   }  
 lab.y<-attr(attr(x,"peaks"),"probes")
 n.probes<-length(lab.y)
 plot(c(0:10),ylim=ylim,type="n",axes=FALSE)
 mar.new[1]<-mar.new[3]<-0
 par(mar = mar.new)
 plot(1:n.probes,ylim=c(-1,0),type="n",axes=FALSE) 
 text(1:n.probes,rep(00,n.probes),lab.y,srt=90,font=2,adj=1)
 }

