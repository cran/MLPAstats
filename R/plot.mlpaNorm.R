`plot.mlpaNorm` <-
function(x, individuals="control", subject=1, add.correction=TRUE, type="height", ylim, ...)
 {

  ind <- subject
  if(!inherits(x,"mlpaNorm"))
     stop("data must be an object of class 'mlpaNorm'")

  type.ind<- charmatch(individuals, c("control","test"))
  if (is.na(type.ind)) 
    stop("\n individuals must be 'control' or 'test' ")


  type.type<- charmatch(type, c("height","relative height"))
  if (is.na(type.type)) 
    stop("\n type must be 'height' or 'relative height' ")


  peaks<-attr(x,"peaks")
  probes<-attr(peaks,"probes")

  if (type.type == 1)
   {
    if (type.ind==1)
     peaksOK<-apply(peaks$control,2,mean)
    else
    peaksOK<-unlist(peaks$test[ind,,drop=TRUE])
   }
  else
   {
    if (type.ind==1)
     peaksOK<-x$control.norm
    else
    peaksOK<-x$test.norm[ind,]
   }

   x.temp<-peaks$size[peaks$probes.control]
   n<-length(peaksOK)
   cc<-c("gray75","gray50")
   my.color<- ifelse(c(1:n)%in%peaks$probes.control,cc[2],cc[1])

   if(missing(ylim))
     ylim<-c(0,max(peaksOK))  

   plot(peaks$size,peaksOK,type="n",ylim=ylim,
         ylab=ifelse(type.type==1,"Peak Height","Relative Peak Height"),
         xlab="Probe size")

   segments(peaks$size,0,peaks$size,peaksOK,col=my.color,lwd=3)

   tt<-(max(peaks$size)-min(peaks$size))
   legend("topright",c("analytical probes","control probes"),
      pch=c(16,16),bty="n",cex=0.8,col=cc)
   
   if (add.correction)
    {
     if (type.ind==1)
      lines(peaks$size,x$pred.c,col="red", lwd=2, ...)
     else
      lines(peaks$size,x$pred.t[,ind],col="red", lwd=2, ...)
    }

   if (type.ind==1)
    title("Mean controls")
   else
    title(attr(peaks,"individuals")[ind])
 }

