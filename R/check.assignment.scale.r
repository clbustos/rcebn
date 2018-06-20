# Compruebo si un ítem tiene mayor carga en otro factor que en el propio
# para ello, voy por pares de escalas (divide et impera)
# y veo si al generar carga cruzada, su relación es mayor con el 
# otro ítem que con el que se supone que corresponde
# Partimos con el caso simple (pares)
# y de ahí creamos el general
# @export
check.asignment.pair<-function(x,key)  {
  if(ncol(key)>2) {stop("Must be a n*2 key")}
  x.2<-x[,rowSums(key)>0]
  key.2<-key[rowSums(key)>0,]
  cov.x<-cov(x)
  nobs<-nrow(x)
  
  res<-lapply(1:nrow(key.2),function(i) {
    f.correct<-colnames(key.2)[key.2[i,]==1]
    f.incorrect<-colnames(key.2)[key.2[i,]==0]
    n.item<-rownames(key.2)[i]
    key.3<-key.2
    key.3[i,]<-c(1,1)
    #print(key.3)
    cfa.1<-lavaan::cfa(keys2lavaan(key.3), sample.cov = cov.x,sample.nobs =nobs, control=list(optim.method='BFGS'))
    ss<-standardizedSolution(cfa.1)
    #print(ss)
    cor.correct<-ss[ss$lhs==f.correct & ss$rhs==n.item,"est.std"]
    cor.incorrect<-ss[ss$lhs==f.incorrect & ss$rhs==n.item,"est.std"]
    data.frame(item=n.item,f.correct=f.correct,f.incorrect=f.incorrect,cor.correct=cor.correct, cor.incorrect=cor.incorrect,result=abs(cor.correct)>abs(cor.incorrect))
  }
  )
  do.call(rbind,res)
}
# @export
check.assigment.scale<-function(x,key) {
  res<-list()
  for(i in 1:(ncol(key)-1)) {
    
    for(j in (i+1):(ncol(key))) {
      n<-paste0(colnames(key)[c(i,j)],collapse=":")
      message(n)
      res[[n]]<-check.asignment.pair(x,key[,c(i,j)])
    }
  }
  do.call(rbind,res)
}

