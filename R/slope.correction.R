`slope.correction` <-
function(y, x, sel.reg, robust, quadratic=TRUE)
{

y.ok<-y[sel.reg]
x.ok<-x[sel.reg]

if (robust)
 {
# 

#
# Probar lmrob
#

 predict.lts<-function(x,newdata=NULL)
  {
   if (!is.null(newdata))
   {
    if (ncol(newdata)==1)
     pred<-coef(x)[1]+coef(x)[2]*newdata[,1]
    else
     pred<-coef(x)[1]+coef(x)[2]*newdata[,1]+coef(x)[3]*newdata[,1]^2
   } 
   else
   {
    pred<-x$fitted.values
   }
   pred 
  }
  
  if (quadratic)
    mod<-lm(y.ok~x.ok+I(x.ok^2))
  else
    mod<-lm(y.ok~x.ok)

  newdat<-data.frame(x.ok=x)
  ans<-predict(mod,newdata=newdat)
 }

else
 {
  if (quadratic)
    mod<-lm(y.ok~x.ok+I(x.ok^2))
  else
    mod<-lm(y.ok~x.ok)

  newdat<-data.frame(x.ok=x)
  ans<-predict(mod,newdata=newdat)
 }

ans

}

