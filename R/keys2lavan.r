# Create a lavaan model for CFA
# based on a data.frame keys matrix
keys2lavaan<-function(x) {
  x<-data.frame(x)
  cn<-colnames(x)
  rn<-rownames(x)
  out<-character(ncol(x))
  for(i in 1:ncol(x)) {
    f<-cn[i]
    items<-rn[x[,i]!=0]
    out[i]<-paste0(f," =~ ",paste0(items,collapse="+"))
  }
  paste0(out,collapse="\n")
}