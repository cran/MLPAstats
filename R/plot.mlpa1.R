`plot.mlpa1` <-
function(x, ylim=c(0.7,1.3), ...)
 {

 # inherits

 
 lab.x<-attr(attr(attr(x,"info"),"peaks"),"individuals")
  
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
 
 for (i in 1:(n-1))
   {

      plot(c(0:10),ylim=ylim,type="n",axes=FALSE)
      text(10,1,lab.x[i],adj=1,font=2)
      plot.mlpa1.i(x,i, ylim, ...)
   }  

 lab.y<-attr(attr(attr(x,"info"),"peaks"),"probes")
 n.probes<-length(lab.y)
 plot(c(0:10),ylim=ylim,type="n",axes=FALSE)
 mar.new[1]<-mar.new[3]<-0
 par(mar = mar.new)
 plot(1:n.probes,ylim=c(-1,0),type="n",axes=FALSE) 
 text(1:n.probes,rep(0,n.probes),lab.y,srt=90,font=2,adj=1)
 }

