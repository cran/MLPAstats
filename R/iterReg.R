`iterReg` <-
function(x,y,sel.control,type.method,
      type.bands,level,B,maxit)
 {
  k<-0
  itera<-TRUE
  while(itera & k<maxit)  
   {
 
  if (type.bands==1)
    res.i<-iterReg.i(x,y,sel.control,type.method,level)
  else 
    res.i<-bootBands(x,y,sel.control,type.method,B,level)

  lim.inf<-res.i$bands[,1]
  lim.sup<-res.i$bands[,2] 
#  if (type.method==1 & type.bands==1)
  if (type.method==1)
    outlier<- y/x>=lim.inf & y/x<=lim.sup  
  else
    outlier<- y>=lim.inf & y<=lim.sup  
  n<-length(x)
   significant<-c(1:n)[!outlier]
  if (any(!outlier)) 
    itera<-TRUE
  else
    itera<-FALSE
  sel.control.old<-sel.control
  sel.control<-c(1:n)[outlier]
  if (any(!sel.control%in%sel.control.old))
   itera<-TRUE
  else
   itera<-FALSE
  k<-k+1

  }

  significant.all<-rep(0,n)
  significant.all[significant]<-ifelse(y[significant]>x[significant],1,-1)
  
  if(type.bands==1)
   {
    res<-iterReg.i(x,y,sel.control,type.method,level)
    ans<-list(coef=res$mod$coef,se.reg=res$s,
            df=res$df,n.iter=k-1,bands=res$bands,
            marker.sig=significant,eval=significant.all,x=x,y=y,
            level=level)
   } 
  else
   {
    res<-bootBands(x,y,sel.control,type.method,B,level)
    ans<-list(bands=res$bands,marker.sig=significant,eval=significant.all,x=x,y=y,
            level=level,se.reg=res$s,coef=res$coef)
   }
  ans
 }

