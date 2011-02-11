`setupMLPA` <-
function(data.controls, data.cases, size.probes, reference.probes)
 {

 data.tests<-data.cases
 probes.control<-reference.probes 

# check data
 if(!identical(names(data.controls),names(data.tests)))
  stop("data for control and test samples should have the same variables")


 n<-length(size.probes)

 oo.c<-order(data.controls[,1],data.controls[,2]) 
 oo.t<-order(data.tests[,1],data.tests[,2]) 
 data.controls<-data.controls[oo.c,]
 data.tests<-data.tests[oo.t,]


 o<-order(size.probes)
 size<-size.probes[o]
 
 control<-data.controls[,-c(1:2)]
 control<-control[,o]
 test<-data.tests[,-c(1:2)]
 test<-test[,o] 

 mean.control<-aggregate(control,by=list(probe=data.controls[,1]),FUN=mean)[,-1] 
 mean.test<-aggregate(test,by=list(probe=data.tests[,1]),FUN=mean)[,-1] 


 probes.control.o<-c(1:n)[o%in%probes.control]

 names.probes<-names(test)
 names.indv<-unique(data.tests[,1])

 ans<-list(control=control, test=test, mean.control=mean.control, mean.test=mean.test, 
            size=size, probes.control=probes.control.o)

 attr(ans,"individuals")<-names.indv 
 attr(ans,"probes")<-names.probes
 attr(ans,"extra.info.control")<-data.controls[,1:2] 
 attr(ans,"extra.info.test")<-data.tests[,1:2]

 attr(ans,"n.replicates")<-length(unique(data.controls[,2]))

 class(ans)<-"setupMLPA"
 
 ans 
 }

