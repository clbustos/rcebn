# Create a matrix useful for target rotation
# based on a data.frame keys matrix
keys2target<-function(x,row.names=NULL) {
  
  x<-data.frame(lapply(data.frame(x),function(xx) {xx[is.na(xx)]<-0;xx}))
  
  row.valid=rowSums(abs(x))>0
  x<-x[row.valid,colSums(abs(x))>0]
  is.na(x)<-x!=0
  out=as.matrix(data.frame(x))
  if(!is.null(row.names)) {
    rownames(out)<-row.names[row.valid]
  }
  out
  }
