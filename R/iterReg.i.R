`iterReg.i` <-
function(x,y,sel.control,type,level)
 {

#
# type=1: offset (JRG approach)
# type=2: REX 
#

  x1<-x[sel.control]
  y1<-y[sel.control]

  if (type==1)
   {
   mod<-lm(log(y1)~offset(log(x1)))
   newdat<-data.frame(x1=x)
   }
  else
   {
    mod<-lm(y1~x1-1)
    newdat<-data.frame(x1=x)
   }

  pred<-predict(mod,newdat,se.fit=TRUE,interval="prediction",level=level)
  s<-pred$res
   
#OJO este sum(x.sqrt^2)! comprobar con otros

  if (type!=1)
   {
    se.x<-sqrt(s^2*(1+((x^2)/sum(x1^2))))
    lim.inf<-pred$fit[,1]-(qt(level,pred$df)*se.x)
    lim.sup<-pred$fit[,1]+(qt(level,pred$df)*se.x)
#    lim.inf<-pred$fit[,2]
#    lim.sup<-pred$fit[,3]
   }
  else
   {
#    se.x<-s
    se.x<-sqrt(s^2*(1+(1/sum(x1^2))))
    lim.inf<-rep(exp(mod$coef-(qt(level,pred$df)*se.x)),nrow(newdat))
    lim.sup<-rep(exp(mod$coef+(qt(level,pred$df)*se.x)),nrow(newdat))
#    lim.inf<-pred$fit[,2]
#    lim.sup<-pred$fit[,3]

   }
  
  ans<-list(mod=mod,bands=cbind(lim.inf,lim.sup),
            s=pred$res,df=pred$df) 
  ans

 }

