`mlpa` <-
function(x, method, threshold=c(0.7,1.33), gamma=0.95,  alpha=0.01, id=0.05, bands="parametric", maxit=100,B=200)
 {
 selec<-c("threshold","mixed-model","outlier","REX-MLPA")
 type.method<-charmatch(method,selec)
 if(type.method==0)
   stop("method should be 'threshold', 'mixed-model', 'outlier', or 'REX-MLPA'")
  
 level=1-alpha

 if (type.method==4)
  {
   type.bands<-charmatch(bands,c("parametric","bootstrap"))
   if(type.bands==0)
    stop(" bands should be 'parametric' or 'bootstrap'")
  }


 k <- attr(x,"n.replicates")

 if (type.method==1)
  {
    indiv<-attr(attr(x,"peaks"),"individuals")
    res<-vector("list", length(indiv))
    for (i in 1:length(indiv))
      {
       ratio<-x$test.norm[i,]/x$control.norm
       res[[i]]$marker.sig<-c(1:ncol(x$test.norm))[ratio>threshold[2] | ratio<threshold[1]]
       significant<-res[[i]]$marker.sig
       significant<-significant[!is.na(significant)]
       eval<-rep(0,ncol(x$test.norm))
       if (length(significant)>0)  
        eval[significant]<-ifelse(ratio[significant]>threshold[2],1,-1)
       res[[i]]$eval<-eval
      }
    attr(res,"info")<-x
    attr(res,"threshold")<-threshold
  }

 else if (type.method==2)
  {
    if (k==1)
     stop("No replicates are available. Try another method")

    res<-toleranceInterval(x,gamma,alpha,k)
    attr(res,"info")<-x
    attr(res,"params")<-list(alpha=alpha, gamma=gamma)
  }


 else if (type.method==3)
  {
    if (k==1)
     stop("No replicates are available. Try another method")

    mod<-toleranceInterval(x,gamma,alpha,k)
    sig<-qnorm(1-(id/2))
    sig.ok<-abs(resid(mod$mod,type="p"))>sig
    indiv<-attr(attr(mod$info,"peaks"),"individuals")
    dat<-mod$mod$data
    dat$sig<-sig.ok 

    res<-vector("list", length(indiv))
    for (i in 1:length(indiv))
      {
       dat.i<-dat[dat$control==indiv[i],]
       res[[i]]$marker.sig<-c(1:nrow(dat.i))[dat.i$sig]
      }
    attr(res,"mod")<-mod
    attr(res,"id")<-id
    attr(res,"info")<-x
  }

 else
  {
#
# Ojo esto hacia regresion constante. Ahora solo REX
#
#   type.method.ok<-type.method-3

   type.method.ok<-2
   res<-list()
   probes.control<-attr(x,"peaks")$probes.control 
   for (i in 1:nrow(x$test.norm))
    res[[i]]<-iterReg(x$control.norm,as.numeric(x$test.norm[i,]),
             probes.control,type.method.ok,
             type.bands,level,B,maxit)
    attr(res,"info")<-x 
  }

  class(res)<-"mlpa"
  attr(res,"type")<-selec[type.method]
  attr(res,"type.num")<-type.method
  res

 }

