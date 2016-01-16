# Calcular las medias, desviaciones estándar e intervalos de confianza, considerando la existencia de datos perdidos
# @export
mi.descriptives<-function(mi,subset=1:nrow(complete(mi)), FUN=I, variables=1:ncol(FUN(complete(mi))) ) {
  m<-mi$m
  if(is.logical(subset)) {
    subset<-which(subset)
  }
  names.v<-names(FUN(complete(mi)))[variables]
  n<-length(subset)
  k<-length(variables)
  means<-matrix(0,m,k)
  var.s<-matrix(0,m,k)

  cat("Casos:",n,"\n")


  for(i in 1:m) {
#    print(complete(mi,i)[,variables])
    c.data<-FUN(complete(mi,i))
    
    means[i,]<-colMeans(c.data[subset,variables])
    var.s[i,]<-apply(c.data[subset,variables],2,var)
  }
  skew.s<-colMeans(t(sapply(1:mi$m,function(x) {sapply(complete(mi,x)[subset,variables],psych::skew,type=2)})))
  kurtosis.s<-colMeans(t(sapply(1:mi$m,function(x) {sapply(complete(mi,x)[subset,variables],psych::kurtosi,type=2)})))
  median.s<-colMeans(t(sapply(1:mi$m,function(x) {sapply(complete(mi,x)[subset,variables],median)})))
  
  
  var.e<-var.s/n
  ee<-sqrt(var.e)
  ci.l<-means+qt(0.025,n-1)*ee
  ci.u<-means+qt(0.975,n-1)*ee
  
  mm<-lapply(1:k,function(x) {
    pool.scalar(Q=means[,x],U=var.e[,x],n=n)
  })
  
  colnames(means)<-names.v
  colnames(var.s)<-names.v
  
  umeans<-sapply(mm, function(x) {x$qbar})
  sds<-colMeans(sqrt(var.s))
  ee.nopool<-colMeans(ee)
  ee.pool<-sqrt(sapply(mm, function(x) {x$t}))
  
  matriz<-data.frame(media=umeans,sd=sds,ic.l=umeans+ee.pool*qt(0.025,n-1),ic.u=umeans+ee.pool*qt(0.975,n-1),  lambda=sapply(mm,function(x) {x$lambda}), skew=skew.s,kurtosis=kurtosis.s,median=median.s)
  
  list(mean=umeans,sd=sds,ee=ee.nopool,ee.p=ee.pool,mm=sapply(mm,function(x) {x$lambda}),matriz=matriz)
  
}
