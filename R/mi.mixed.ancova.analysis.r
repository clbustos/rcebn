# Calcular ancovas de manera general para modelos mixtos, en una base pre-establecida
# @param mi mice multiple imputation database
# @v      Vector of variables to perform ancova. First es pre-test, second is post-test and third is experimental
# @subset Subset of data to calculate within. Same as subset on lm
# @trans  Transformation of @v, to use inside regression methods
# @covariates Other covariates
# @quad Use quadratic regression on pre-value
# @extra test extra variables, one on one.

mi.mixed.ancova.analysis<-function(mi,v,mixed_model,subset=1:nrow(complete(mi)), trans="I",  covariates=NULL ,quad=F, extra=NULL) {
  
  pow2<-function(x) {x^2}
  require(lme4)
  require(car)
  require(miceadds)
  pre=v[1]
  post=v[2]
  exp.v=v[3]
    covar=""
  if(!is.null(covariates)) {
    covar=paste0(c("",covariates),collapse="+")
  }
  # Models
  # ======
  #
  # 1 Complete model, including intersection between experimental and pre
  # post ~ (mixed) + pre*exp + cov 
    f.1<-paste0(trans,"(",post,")~(",mixed_model,")+",trans,"(",pre,")",covar,"+",exp.v,"+",trans,"(",pre,"):",exp.v,collapse="")
  # 2 Model with experimental group
  # post ~ (mixed) + pre+exp+group
    f.2<-paste0(trans,"(",post,")~(",mixed_model,")+",trans,"(",pre,")",covar,"+",exp.v,collapse="")
  # 3 Model only with covariates
  # post ~ (mixed)  + pre + covar
    f.3<-paste0(trans,"(",post,")~(",mixed_model,")+",trans,"(",pre,")",covar,collapse="")
    
    cat("Cases:",length(subset),"\n")
    #cat(f.1,"\n")
    #cat(f.2,"\n")
    #cat(f.3,"\n")
    # First, levene
    lvt.with<-function(f) {
      sapply(with(mi, 
      {
      lm.1<-lmer(f,subset=subset,REML=F)
      leveneTest(y=resid(lm.1),group=get(exp.v)[subset])$"F value"[1]
      
      } )$analyses,I)
    }
    
    lvt.simple<-micombine.F(lvt.with(f.2),df1=1,display=F)
    lvt.slope<-micombine.F(lvt.with(f.1),df1=1,display=F)
    
    if(quad) {
      f.1cuad<-paste0(trans,"(",post,")~(",mixed_model,")+",trans,"(",pre,")",covar,"+",exp.v,"+",trans,"(",pre,"):",exp.v,"+",trans,"(",pre,"^2)",  collapse="")
      # First and a half, quadratic
      fit.quad<-with(data=mi,lmer(f.1cuad,subset=subset,REML=F))
      #return(met.2)
      test.quad<-pool.compare(fit.quad,fit.1,method="Wald")

    }
    
    # Second, equal slopes
    
    fit.1<-with(data=mi,lmer(f.1,subset=subset,REML=F))
    fit.2<-with(data=mi,lmer(f.2,subset=subset,REML=F))

    #print(fit.0)
    test.slope<-pool.compare(fit.1,fit.2,method="Wald")
    base_model<-f.2
    base_fit<-fit.2

    
    # if slope is positive, we perform a test of significance for the simple effect on the point 0 of transformation
    test.slope.simple<-NULL
    
    mode.test<-"ancova"
    
    if (test.slope$pvalue<0.05) {
      mode.test<-"moderation"
      base_fit<-fit.1
      base_model<-f.1
      f.1a<-paste0(trans,"(",post,")~(",mixed_model,")+",trans,"(",pre,")",covar,"+",trans,"(",pre,"):",exp.v, "+",exp.v,collapse="")
      
      # 2 Model with experimental group
      f.1b<-paste0(trans,"(",post,")~(",mixed_model,")+",trans,"(",pre,")",covar,"+",trans,"(",pre,"):",exp.v,collapse="")

      cat("Simple test:\n")
      cat(f.1a,"\n")
      cat(f.1b,"\n")
      fit.1a<-with(data=mi,lmer(f.1a,subset=subset,REML=F))
      fit.1b<-with(data=mi,lmer(f.1b,subset=subset,REML=F))
      #print(fit.0)
      test.slope.simple<-pool.compare(fit.1a,fit.1b,method="Wald")
    }
      
    #met.2$pvalue es lo que necesitamos
    
    # Third: Classical ANCOVA
    
    fit.2<-with(data=mi,lmer(f.2,subset=subset,REML=F))
    fit.3<-with(data=mi,lmer(f.3,subset=subset,REML=F))
    #print(fit.0)
    test.final<-pool.compare(fit.2,fit.3,method="Wald")

      
    extra_res=list()
    if(!is.null(extra)) {
      for(ex in extra) {
          f.extra<-paste0(base_model,"+",ex,collapse="")
          fit.extra<-with(data=mi,lmer(f.extra,subset=subset,REML=F))
          test.extra<-pool.compare(fit.extra,base_fit,method="Wald")
          extra_res[[ex]]<-list(test=test.extra,pool=fit.extra)
      }
    }
    out<-list(test.levene.simple=lvt.simple, test.levene.slope=lvt.slope,pool.slope=fit.1, test.slope=test.slope, test.slope.simple=test.slope.simple, pool.slope=fit.1,pool.final=fit.2, ancova=test.final, extra=extra_res,mode.test=mode.test)
    
    
    if(quad) {
      out$test.quad<-test.quad
      out$pool.quad<-fit.quad
    }
    
    
    class(out)<-"mi.mixed.ancova.analysis"
    invisible(out)
    
  }
summary.mi.mixed.ancova.analysis<-function(x) {
  
}
