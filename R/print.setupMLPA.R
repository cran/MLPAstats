`print.setupMLPA` <-
function(x,...)
 {
  probes<-attr(x,"probes")
  n.probes<-length(probes)
  n.probes.control<-length(x$probes.control)

  cat("Object of class setupMLPA \n")
  cat("------------------------- \n")
  cat("Number of control samples:",nrow(x$mean.control),"\n")
  cat("Number of test samples:",nrow(x$mean.test),"\n")
  
  cat("Number of probes:",n.probes,"\n")

  if (n.probes>8)
   cat("  ",probes[1:4],"...",probes[(n.probes-3):n.probes],"\n")
  else
   cat(probes)
  cat("Number of control probes:",n.probes.control,"\n")
  
  cat("Number of replicates:",attr(x,"n.replicates"),"\n")


 }

