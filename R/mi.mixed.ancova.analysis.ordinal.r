# Calcular ancovas de manera general para modelos mixtos, en una base pre-establecida
# @param mi mice multiple imputation database
# @v      Vector of variables to perform ancova. First es pre-test, second is post-test and third is experimental
# @subset Subset of data to calculate within. Same as subset on lm
# @trans  Transformation of @v, to use inside regression methods
# @
# @covariates Other covariates


mi.mixed.ancova.analysis.ordinal<-function(mi,v,mixed_model,subset=1:nrow(complete(mi)), trans="I",  covariates=NULL ,quad=F) {
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
  #Models
  # 1 Complete model, including intersection between experimental and pre

  # 1 Complete model, including intersection between experimental and pre
  # 2 Model with experimental group
    f.2<-paste0(trans,"(",post,")~(",mixed_model,")+",trans,"(",pre,")",covar,"+factor(",exp.v,")",collapse="")
  # 3 Model only with covariates
    f.3<-paste0(trans,"(",post,")~(",mixed_model,")+",trans,"(",pre,")",covar,collapse="")
    
    cat("Cases:",length(subset),"\n")
    cat(f.2,"\n")
    cat(f.3,"\n")
    
    # Second, equal slopes
    
#    fit.1<-with(data=mi,clmm(f.1,subset=subset))
    
  #  fit.2<-with(data=mi,clmm(f.2,subset=subset))

    #print(fit.0)
    #test.slope<-pool.compare(fit.1,fit.2,method="likelihood")
  
      
      #met.2$pvalue es lo que necesitamos
      
      # Third: Classical ANCOVA
      
      fit.2<-with(data=mi,clmm(f.2,subset=subset),link="logit")
      fit.3<-with(data=mi,clmm(f.3,subset=subset),link="logit")
      chis<-numeric(mi$m)
      dfs<-numeric(mi$m)
      
      for(i in 1:mi$m) {
        res<-anova(fit.2$analyses[[i]], fit.3$analyses[[i]])
        chis[i]<-res$LR.stat[2]
        dfs[i]<-res$df[2]
      }
      return(micombine.chisquare(dk=chis,df=dfs[1]))
      
      out<-list(pool.slope=fit.1, test.slope=test.slope, test.slope.simple=test.slope.simple, pool.slope=fit.1,pool.final=fit.2, ancova=test.final)
    if(quad) {
      out$test.quad<-test.quad
      out$pool.quad<-fit.quad
    }
    class(out)<-"mi.mixed.ancova.analysis.ordinal"
    invisible(out)
    
  }


vcov.clmmesp<-function(x) {
    res<-NextMethod()
    print("h")
    #print(res)
    #print(colnames(res))
    res[-nrow(res),-nrow(res)]
}
