getThresholds<-function(x)
 {
  if (!attr(x,"type") %in% c("mixed-model", "threshold"))
   stop ("x must be fitted using 'mixed-model' or 'threshold' method")
  
  if (attr(x,"type")=="mixed-model")
   {
    individuals<-attr(attr(attr(x,"info"),"peaks"),"individuals")
    n<-length(individuals)

    ans<-matrix(NA, nrow=n , ncol=2)
    for (i in 1:n) 
     {
      ans[i,]<-round(exp(rev(x[[i]]$threshold)),2)
     }
    dimnames(ans)[[1]]<-individuals
    dimnames(ans)[[2]]<-c("lower", "upper")
    attr(ans,"params")<-attr(x,"params")
   }
  else
   {
    ans<- attributes(ans)$threshold
   }
 attr(ans,"type")<-attr(x,"type") 
 class(ans)<-"thresholds"
 ans
 }

print.thresholds<-function(x,...)
 {
  if (attr(x,"type")=="mixed-model")
   {
    params<-attr(x,"params")
    cat("\n")
    cat("Thresholds obtained from mixed-model \n")
    cat("------------------------------------ \n")
    print.table(x)
    cat("\n")
    cat("alpha:", params$alpha, "gamma:", params$gamma)
    cat("\n") 
   }
  else
   {
    cat("\n")
    cat("Thresholds for all individuals:", x)
    cat("\n")
   }
 }
