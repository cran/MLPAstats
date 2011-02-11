`mlpaNorm` <-
function(x, method="sum.peaks.controls", reference.probes=TRUE, replicate=FALSE, transform="none", sliding=9, quadratic=TRUE, robust=FALSE)
 {

  probes.control<-reference.probes
  if(!inherits(x,"setupMLPA"))
     stop("data must be an object of class 'setupMLPA'")

  type.method <- charmatch(method, c("nonlinear","slope.correction", "sum.peaks.controls", "sum.peaks.all"))
  if (is.na(type.method)) 
    stop("\n model must be 'nonlinear', 'slope.correction', 'sum.peaks.controls' or 'sum.peaks.all' ")



  if (type.method==1 & !replicate)
      {
        if (attr(x, "n.replicates")==1)
         {
          stop("No replicates are available. Try another method")
         }
        else
         {
          warning("This method needs replicates... \n Argument 'replicate' has been set to TRUE")
          replicate<-TRUE
         }
      }


 if (type.method==2 & replicate)
  stop("Slope correction method does not support replicates")


  if(!replicate)
    {
      x$control<-x$mean.control
      x$test<-x$mean.test
    }


  type.transf <- charmatch(transform, c("none","logarithm", "sqrt"))
  if (is.na(type.method)) 
    stop("\n transformation must be 'none', 'logarithm', or 'sqrt' ")

  if(type.transf==1){
     transf<-function(x) {x}
     transf.inv<-function(x) {x}
  }
  if(type.transf==2){
     transf<-log 
     transf.inv<-exp
  }
  if(type.transf==3){
     transf<-sqrt
     transf.inv<-function(x) {x^2}
  }


  if (type.method==1) 
   {

     on.exit(rm("datG",pos=1))    
 
     if(!probes.control)
      {
        ans.control<-exponential.correction(x,control=TRUE, probes.control=FALSE,
                               sliding=sliding, transf=transf, transf.inv=transf.inv)
       
        ans.test<-exponential.correction(x,control=FALSE, probes.control=FALSE,
                               sliding=sliding, transf=transf, transf.inv=transf.inv)
      }  

     if(probes.control)
      {
        ans.control<-exponential.correction(x,control=TRUE, probes.control=TRUE,
                                sliding=sliding, transf=transf, transf.inv=transf.inv)
       
        ans.test<-exponential.correction(x,control=FALSE, probes.control=TRUE,
                                sliding=sliding, transf=transf, transf.inv=transf.inv)
      }

  
     ans<-list(control.norm=ans.control$rel.height,
              test.norm=ans.test$rel.height, pred.c=ans.control$pred,
              pred.t=ans.test$pred) 
   }

  if (type.method==2) 
   {
    controlMean<-apply(x$control,2,mean)


    controls.corrected<-slope.correction(controlMean,x$size,x$probes.control,robust,quadratic)


    test.corrected<-apply(x$test,1,slope.correction,x=x$size,
                          sel.reg=x$probes.control,robust=robust,quadratic=quadratic)

    rel.height.control<-controlMean/controls.corrected
    rel.height.test<-t(x$test)/test.corrected

    ans<-list(control.norm=rel.height.control,
              test.norm=t(rel.height.test), pred.c=controls.corrected, pred.t=data.frame(test.corrected))
 
   }

  if (type.method==3)
   {

# controls
     sum.peaks<-apply(x$control,1,sum,na.rm=TRUE)
     temp<-x$control/sum.peaks
     rel.height.control<-apply(temp,2,mean,na.rm=TRUE)
# tests
     sum.peaks<-apply(x$test,1,sum,na.rm=TRUE)
     rel.height.test<-x$test/sum.peaks
 
     if(replicate)
       ans<-list(control.norm=temp,
               test.norm=rel.height.test) 
     else
       ans<-list(control.norm=rel.height.control,
               test.norm=rel.height.test) 
   }


  if (type.method==4)
   {

# controls
     all<-rbind(x$control,x$test)
     sum.peaks<-apply(all,1,sum)
     temp<-x$control/sum.peaks
     rel.height.control<-apply(temp,2,mean)
# tests
     rel.height.test<-x$test/sum.peaks
 
     if(replicate)
       ans<-list(control.norm=temp,
               test.norm=rel.height.test) 
     else
       ans<-list(control.norm=rel.height.control,
              test.norm=rel.height.test) 
   
   }
  attr(ans,"peaks")<-unclass(x)
  attr(ans,"n.replicates")<-attr(x,"n.replicates")

  class(ans)<-"mlpaNorm"
  ans
 }

