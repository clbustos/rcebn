
limpiar.loads<-function(l) {
  l2<-matrix(0,nrow(l),ncol(l))
  for(i in 1:nrow(l)) {
    l2[i,which.max(l[i,])]<-max(l[i,])
  }
  l2
}
#' @export

lavaan.from.omega<-function(x,cut) {
  ob<-x$schmid$oblique
  f1<-psych::factor2cluster(ob,cut)
  salida<-list() 
  salida$g<-paste0("g=~",paste0(rownames(ob),collapse="+"))
  
  if(ncol(f1)<ncol(ob)) {
    warning(ncol(ob)-ncol(f1)," factors were discarded.")
  }
  f1<-f1[,colSums(f1)>0]
  #print(ncol(f1))
  for(i in 1:ncol(f1)) {
    if(sum(f1[,i])==0) {
      message("Non items on factor ",i)
      next
    }
    salida[[paste0("f",i)]]<-paste0("f",i,"=~",paste0(rownames(ob)[f1[,i]==1],collapse="+"))
    salida[[paste0("g:f",i)]]<-paste0("f",i," ~~0*g")
    if(i<ncol(f1)) {
      for(j in (i+1):ncol(f1)) {
        salida[[paste0(i,":",j)]]<-paste0("f",i," ~~0*f",j)
      }
    }
  }
  
  out<-paste0(salida,"\n",collapse="\n")
  attr(out,"nfactors")<-ncol(f1)
  out
}
#' @export
lavaan.from.fa<-function(x,cut) {
  ob<-as.matrix(loadings(x))
  
  f1<-psych::factor2cluster(x,cut)
  salida<-list()   
  if(ncol(f1)<ncol(ob)) {
    warning(ncol(ob)-ncol(f1)," factors were discarded.")
  }
  f1<-f1[,colSums(f1)>0]
  #print(ncol(f1))
  for(i in 1:ncol(f1)) {
    if(sum(f1[,i])==0) {
      message("Non items on factor ",i)
      next
    }
    salida[[paste0("f",i)]]<-paste0("f",i,"=~",paste0(rownames(ob)[f1[,i]==1],collapse="+"))
  }
  
  out<-paste0(salida,"\n",collapse="\n")
  attr(out,"nfactors")<-ncol(f1)
  out
}


#' Cross validation of a factorial analysis.
#' @param x dataframe
#' @param ftt vector with number of factor to test
#' @param folds number of folds for cross-validation
#' @param cut threshold for factorial loadings 
#' @import foreach
#' @export
cv.fa<-function(x,ftt,folds=5,cut=0.3,...) {
  require(foreach)
  folds<-cvTools::cvFolds(nrow(x))
  results<-list()
  
  for(i in 1:folds$K) {
    training<-x[folds$subsets[folds$which!=i],]
    test<-x[folds$subsets[folds$which==i],]
    cat("Fold:",i,"using ",nrow(training)," cases on training and ",nrow(test),"on test\n")
    cor.training<-cor(training)
    cov.test<-cov(test)
    cor.test<-cor(test)
    measures=c("chisq","df","pvalue","rmsea","rmsea.pvalue","cfi","tli","srmr","aic","bic")
    
    results[[paste0("r",i)]]<- foreach(j=ftt) %dopar% {
      cat("number of factors:",j,"\n")
      
      fa1<-psych::fa(cor.training,nfactors = j,...)
      
      f<-calculate.ml.f(cor.test,ll=loadings(fa1))
      
      lfo<-lavaan.from.fa(fa1,cut)
      
      sem.1<-try(lavaan::cfa(lfo,sample.cov=cov.test,sample.nobs=nrow(test),control=list(optim.method='BFGS')))
      
      if(!inherits(sem.1,"try-error")) {
        list(fold=i,factors=j,factors.final=attr(lfo,"nfactors"), measures=c(f=f,lavaan::fitMeasures(sem.1,measures)))
      } else {
      cat(lfo)
        list(fold=i,factors=j,factors.final=attr(lfo,"nfactors"),measures=c(f=f,rep(NA,length(measures))))
      }
     } #)
    }
  class(results)<-c("cv.fa")
  results

}

#' @export
cv.fa.bif<-function(x,ftt,folds=5,cut=0.3,...) {
  folds<-cvTools::cvFolds(nrow(x),K=folds)
  results<-list()
  for(i in 1:folds$K) {
    training<-x[folds$subsets[folds$which!=i],]
    test<-x[folds$subsets[folds$which==i],]
    cat("Fold:",i,"\n")
    cor.training<-cor(training)
    cov.test<-cov(test)
    measures=c("chisq","df","pvalue","rmsea","rmsea.pvalue","cfi","tli","srmr","aic","bic")
    results[[paste0("r",i)]]<-foreach(j=ftt) %dopar% {
      #for(j in ftt) {
      cat("number of factors:",j," on ",nrow(training)," cases on training and ",nrow(test),"on test\n")
      fa1<-psych::omega(cor.training,nfactors = j,plot=F,...)
      
      cat("Calculating SEM\n")
      lfo<-lavaan.from.omega(fa1,cut)
      sem.1<-try(lavaan::cfa(lfo,sample.cov=cov.test,sample.nobs=nrow(test),control=list(optim.method='BFGS')))
      
      if(!inherits(sem.1,"try-error")) {
        list(fold=i,factors=j,factors.final=attr(lfo,"nfactors"), measures=lavaan::fitMeasures(sem.1,measures))
      } else {
        list(fold=i,factors=j,factors.final=attr(lfo,"nfactors"),rep(NA,length(measures)))
      }
      #cat(j,"->f:",f,"\n")
      #results[j,i]<-f
    }
  }
  class(results)<-c("cv.fa","cv.fa.bif")
  results
  #t(sapply(results,function(x) {c(fold=x$fold,factors=x$factors,factors.final=x$factors.final, measures=x$measures)}))
}

#' @export
as.data.frame.cv.fa<-function(x) {
  data.frame(t(do.call(cbind,lapply(x,function(xx) {sapply(xx,function(x) {c(fold=x$fold,factors=x$factors,factors.final=x$factors.final,x$measures)})}))))
}

#' @export
plot.cv.fa<-function(x,factors="factors",measure="chisq") {
  require(ggplot2)
  xx<-as.data.frame.cv.fa(x)
  xx1<-data.frame(fold=xx$fold,factors=xx[,factors],xx[,4:ncol(xx)])
  #print(xx1)
  aggre.mean<-aggregate(xx[,4:ncol(xx)],list(factors=xx[,factors]),mean,na.rm=T)
  aggre.sd<-aggregate(xx[,4:ncol(xx)],list(factors=xx[,factors]),sd,na.rm=T)
  aggre.sd.1<-aggre.mean[,2:ncol(aggre.sd)]+aggre.sd[,2:ncol(aggre.sd)]
  aggre.sd.m1<-aggre.mean[,2:ncol(aggre.sd)]-aggre.sd[,2:ncol(aggre.sd)]
  #print(aggre.sd.1)
  #print(aggre.sd.m1)
  
  aggre.mean$fold<-0
  aggre.sd.1$fold<- -1
  aggre.sd.1$factors<-aggre.mean$factors
  aggre.sd.m1$fold<- -2
  aggre.sd.m1$factors<-aggre.mean$factors
  
  #print(aggre)
  xx1<-rbind(aggre.mean,aggre.sd.1,aggre.sd.m1,xx1)
  
  xx1$summary<-factor(xx1$fold<=0,labels=c("Folds","Mean"))
  xx1$linetype<-1
  xx1$linetype[xx1$fold<0]<-2
  xx1$linetype<-factor(xx1$linetype)
  f<-paste0("ggplot(xx1,aes(x=factors,y=",measure,",group=fold,colour=summary,linetype=linetype))+scale_linetype_discrete(guide='none')+geom_line()")
  eval(parse(text=f))

}

#' @export

best.result.cv.fa<-function(x) {
  d.f<-as.data.frame(x)
  d.f.2<-d.f
  d.f.2$cfi<- - d.f.2$cfi
  d.f.2$tli<- - d.f.2$tli
  ag.factors<-aggregate(d.f.2[,-c(1:3)],list(factors=d.f.2$factors),mean)
  ag.factors.f<-aggregate(d.f.2[,-c(1:3)],list(factors=d.f.2$factors.final),mean)
  mejor.factor<-sapply(ag.factors,function(xx) {ag.factors$factors[which.min(xx)]})
  mejor.valor<-sapply(ag.factors,function(xx) {xx[which.min(xx)]})
  mejor.factor.f<-sapply(ag.factors.f,function(xx) {ag.factors.f$factors[which.min(xx)]})
  mejor.valor.f<-sapply(ag.factors.f,function(xx) {xx[which.min(xx)]})
  data.frame(mejor.factor,mejor.valor,mejor.factor.f,mejor.valor.f)
}
