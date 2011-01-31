`exponential.correction` <-
function(x, control, probes.control, sliding, transf, transf.inv)
 {
# x is an object of class setupMLPA and it is controlled in mlpaNorm

  if (control) 
      xx<-x$control
  else 
      xx<-x$test

  if(probes.control)
   {
     Fil<-xx[,x$probes.control]

     if (control)
       id<-attr(x,"extra.info.control")[,1]
     else
       id<-attr(x,"extra.info.test")[,1]

    dat<-data.frame(id=id,
           y=c(Fil,recursive=TRUE),
           x=rep(x$size[x$probes.control],each=nrow(Fil)))
    dat$x.offset<-dat$x-min(x$size)
    dat$y.transf<-transf(dat$y)

    datG<-groupedData(y.transf~x.offset|id,data=dat)
   }

  else    # OJO!!!! ESTO !!!!
   {
    Fil<-apply(xx,2,medianFilter,sliding)
    dat<-data.frame(id=rep(dimnames(Fil)[[2]],each=nrow(Fil)),
                 y=c(Fil,recursive=TRUE),x=rep(x$size,ncol(Fil)))
    dat$x.offset<-dat$x-min(x$size)
    dat$y.transf<-transf(dat$y)
    datG<-groupedData(y.transf~x.offset|id,data=dat)
   } 
 
  assign("datG",datG, env = .GlobalEnv)

  kk<-(max(datG$x.offset)-min(datG$x.offset))/6

  mod.lis<-nlsList(y.transf~(top-bottom)*exp(-(1/k)*x.offset)+bottom, data=datG,
               start=list(top=transf(max(datG$y)),bottom=transf(min(datG$y)),k=kk))


  if(control)
   {
    mod<-nlme(mod.lis,random=top+k+bottom~1)
    newdat<-data.frame(x.offset=x$size-min(x$size),id=rep(levels(datG$id),each=length(x$size)))
    pred<-predict(mod,newdat,level=1)
    pred.ok<-matrix(pred,nrow=nrow(xx),ncol=ncol(xx))
    pred.end<-apply(pred.ok,1,mean)  
    controlMean<-apply(xx,1,mean)
    rel.height<-xx/transf.inv(pred.end)
   }  

  else
   {
    mod<-nlme(mod.lis,random=k~1)
    newdat<-data.frame(x.offset=x$size-min(x$size),id=rep(levels(datG$id),each=length(x$size)))
    pred<-predict(mod,newdat,level=1)
    pred.end<-matrix(pred,nrow=nrow(xx),ncol=ncol(xx))
    rel.height<-xx/transf.inv(pred.end)
   }

  ans<-list(rel.height=rel.height, pred=transf.inv(pred.end))
  ans

 }

