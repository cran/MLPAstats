`bootBands` <-
function(x,y,sel.control,type.method,B,level)
{
 dat <- data.frame(y=y[sel.control],x=x[sel.control])
 if (type.method==1)
   res.lm <- glm(log(y)~offset(log(x)), data=dat)
 else
   res.lm <- glm(y~x-1, data=dat)

 s<-predict(res.lm,se=TRUE)$res
 coef<-coef(res.lm)
 
 res.diag <- glm.diag(res.lm)
 res.res <- res.diag$res*res.diag$sd
 res.res <- res.res-mean(res.res)
#  We set up a new data frame with the data, the standardized 
#  residuals and the fitted values for use in the bootstrap.

 res.data <- data.frame(dat,resid=res.res,fit=fitted(res.lm))

 new.data <- data.frame(x=x)
 new.fit <- predict(res.lm, new.data)
  
 res.fun <- function(dat, inds, i.pred, fit.pred, x.pred, type.method)
     {
          assign(".inds", inds, envir=.GlobalEnv)
          if (type.method==1)
           lm.b <- glm(fit+resid[.inds] ~ offset(log(x)), data=dat)
          else 
           lm.b <- glm(fit+resid[.inds] ~ x - 1, data=dat)
          pred.b <- predict(lm.b,x.pred)
          remove(".inds", envir=.GlobalEnv)
          c(coef(lm.b), pred.b-(fit.pred+dat$resid[i.pred]))
     }

 res.boot <- boot(res.data, res.fun, R=B, m=1, fit.pred=new.fit,
                  x.pred=new.data, type.method=type.method) 

#  Basic bootstrap prediction limits 
 fit<-rbind(inf=new.fit,sup=new.fit)
 pp<-c(ceiling(level*(B+1)),trunc((B+1)-level*(B+1)))
 percent<-apply(res.boot$t[,-c(1)],2,function(x) sort(x)[pp])
 limits<- fit-percent

 ans<-list(fit=new.fit,bands=t(limits),s=s,coef=coef)
 ans
}

