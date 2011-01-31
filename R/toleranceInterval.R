`toleranceInterval` <-
function(x,gamma,alpha,k)
 {
  test<-c(t(as.matrix(x$test)))
  n.test<-nrow(x$test)/k
  control<-c(t(as.matrix(x$control)))
  n.control<-nrow(x$control)/k
  group<-c(rep(0,length(control)),rep(1,length(test)))
  group2<-c(rep(0,length(control)),rep(c(1:n.test),each=ncol(x$test)*k))
  probe<-attr(attr(x,"peaks"),"probes")
  probes<-rep(probe,(n.test+n.control)*k)
  dat<-data.frame(peak=c(control,test),group=group,probe=probes,group2=group2)

  ans<-list()

  for (i in 1:n.test)
   {
    dat.i<-dat[dat$group2==0 | dat$group2==i,]
    mostra.gd<-groupedData(peak~group|probe,dat.i)
    model.lme<-lme(log(peak)~group,mostra.gd,random=pdDiag(form=~group),method="REML")

    pred.dif<-data.frame(row.names(ranef(model.lme)),fixed.effects(model.lme)[2]+ranef(model.lme)[,2])
    names(pred.dif)<-c("probe","dif")
    pred.dif<-pred.dif[order(pred.dif$probe),]

# Tolerance intervals. Exact method. Aqui podemos tener un problema con los grados de libertad. Siguiendo el lme serían 
# el total de datos menos el total de probes menos 1. Pero otra aproximación sería simplemente el total de probes menos 1
# que es lo que hemos puesto

    var.dif<-(model.lme$sigma^2)*2
#    df<-model.lme$fixDF$terms[2]
    df<-model.lme$dims$ngrps[1] - 1
    var.dif.up<-df*(var.dif)/qchisq(alpha,df)

    cut.gamma<-(1-gamma)/2
    
    qtol.up<-qnorm(1-cut.gamma)*sqrt(var.dif.up)
    qtol.low<-qnorm(cut.gamma)*sqrt(var.dif.up)

    eval<-(pred.dif[,2]>qtol.up)*(1)+(pred.dif[,2]<qtol.low)*(-1)
    o<-match(probe,pred.dif$probe)  


    ans[[i]]<-list(model=model.lme,threshold=c(qtol.up,qtol.low),
                 pred.dif=pred.dif[o,],eval=eval[o])
   }

  ans
 }

