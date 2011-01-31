`plot.mlpa` <-
function(x,ind=1,...)
 {
   type<-attr(x,"type.num")

   if (type==3)
    plot.mlpa3(attr(x,"mod"),attr(x,"id"),...)  
   else if (type==4)
    plot.mlpa2(x,ind,...)   
   else if (type==1)
    {
     plot.mlpa0(attr(x,"info"), threshold=attr(x,"threshold"))
    }
   else if (type==2)
    {
     plot.mlpa1(x,...)
    }

   else 
    cat("method not implemented for this type \n")
   
 }

