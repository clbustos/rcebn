#' Returns a data.frame with correlations like the psych uses
#' @param x a data.frame
#' @export
#' @import stringr
corAsPsych<-function(x) {
  c.m<-corr.test(x,use="pairwise",adjust="none")
  c.r<-c.m$r
  c.p<-c.m$p
  c.p1<-cut(as.numeric(c.p),breaks=c(-1,0.01,0.05,1.1),labels=c("**","*",""))
  c.p1<-matrix(c.p1,nrow(c.p),ncol(c.p))
  out<-list()
  for(i in 1:ncol(c.r)) {
    v<-colnames(c.r)[i]
    out[[paste0(v,"(r)")]]<-round(c.r[,i],2)
    out[[paste0(v,"(p)")]]<-c.p1[,i]
  }
  
  for(i in 1:(ncol(c.r)-1)) {
    v<-colnames(c.r)[i]
    out[[paste0(v,"(r)")]][(i+1):ncol(c.r)]<-""
    out[[paste0(v,"(p)")]][(i+1):ncol(c.r)]<-""
  }
  data.frame(out)
}
