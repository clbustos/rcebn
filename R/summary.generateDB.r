#' Generates a summary for generateDB
#' @param x generateDB object 
#' @export
summary.generateDB<-function(x) {
  require(psych)
  #require(mvnmle)
  
  alphas<-sapply(x$db,function(x) {
    if(dim(x)[2]==1) {
      NA
    } else {
    try(alpha(x,check.keys=F)$total$raw_alpha)
    }
    })
  
  m1<-colMeans(x$means,na.rm=T)
  n<-colSums(!is.na(x$means))
  s1<-apply(x$means,2,sd,na.rm=T)
  sk<-apply(x$means,2,skew,na.rm=T)
  ku<-apply(x$means,2,kurtosi,na.rm=T)
  if(nrow(x$means)<=5000) {
    sw<-sapply(x$means,function(x) {shapiro.test(x)$p.value})
  } else {
    sw<-rep(NA,ncol(x$means))
  } 
  
  data.frame(v=colnames(x$means),n=n,mean=m1,sd=s1,skew=sk,kurtosis=ku,alpha=alphas,shapiro.wilk=sw)
}
