`plot.mlpa0.i` <-
function(x, ind, ratio, thresholds, ylim, ...)
 {
  xx<-x$control.norm 
  yy<-x$test.norm[,ind]
  n<-length(xx)
  if (ratio)
    diff<-yy/xx
  else
    diff<-yy-xx

  sig.deletion<-diff<thresholds[1]
  sig.duplication<-diff>thresholds[2]

  my.color<- ifelse(sig.deletion,"red",ifelse(sig.duplication,"green","gray50"))


  plot(diff,type="n",axes=FALSE, ylim=ylim, xlab="", ylab="", ...) 
  control<-par("usr")

  if (ratio)
    text(control[1],1,"ratio of \n normalized peaks",srt=90,adj=c(0.5,-1),xpd=TRUE)
  else
    text(control[1],1,"difference of \n normalized peaks",srt=90,adj=c(0.5,-1),xpd=TRUE)

  segments(0,control[3],0,control[4])

  if (ratio) {
     segments(1:n,rep(1,n),1:n,diff,col=my.color,lwd=2)
     segments(0,1,n,1)
  }
  else {
     segments(1:n,rep(0,n),1:n,diff,col=my.color,lwd=2)
     segments(0,0,n,0)
  }


  par(xpd=TRUE)
  for (i in 1:length(thresholds))
   {
     segments(0,thresholds[i],n,thresholds[i],col="gray70")
     text(0,thresholds[i],as.character(round(thresholds[i],2)),adj=1,xpd=TRUE,cex=0.6)
   }
  text(0,1,as.character(1),adj=1,xpd=TRUE)
 }

