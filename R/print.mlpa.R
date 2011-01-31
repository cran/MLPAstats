`print.mlpa` <-
function(x,...)
 {

  indv<-attr(attr(attr(x,"info"),"peaks"),"individuals")
  probes<-attr(attr(attr(x,"info"),"peaks"),"probes")
  
  method<-attr(x,"type")

  ans<-x[[1]]$eval  
  for (i in 2:length(indv))
   ans<-rbind(ans,x[[i]]$eval)

  dimnames(ans)[[1]]<-indv
  dimnames(ans)[[2]]<-probes
  
  cat("MLPA analysis using",method,"(-1: relative loss, 0:normal, 1:relative gain) \n")
  cat("\n")
  print(ans)

  invisible(ans)


 }

