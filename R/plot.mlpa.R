`plot.mlpa` <-
function(x,ind=1,...)
 {
   type<-attr(x,"type.num")
   if (!(type %in% c(1,2,3,4)))
      type<-5

   switch(type,
   plot.mlpa0(attr(x,"info"), threshold=attr(x,"threshold")),
   plot.mlpa1(x,...),     
   plot.mlpa3(attr(x,"mod"),attr(x,"id"),...),
   plot.mlpa2(x,ind,...),
   cat("method not implemented for this type \n"),)     

   
 }

