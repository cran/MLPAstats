`medianFilter` <-
function(x,sliding)
 {
   control<-sliding/2
   if (!(control>trunc(control)))
     stop("try an even number for sliding")
   n<-length(x)
   out<-rep(NA,n)
   nn<-trunc(control)
   x<-c(rep(x[1],nn),x,rep(x[n],nn))

   for (i in 1:n)
    {
      out[i]<-median(x[i:(i+sliding-1)])
    }
   out
 }

