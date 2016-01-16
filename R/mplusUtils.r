#' Clean names to acceptable for Mplus
#' @param x string
#' @param a clean string
cleanMplusName<-function(x) {
  str_replace_all(x,"[^a-zA-Z0-9]","")
}

#' Generate a 'at' ARE 'X1,X2'
#' Useful for nominal, categorical and everything related
#' @param at Name of category
#' @param l list of variables
#' @return a string
parlistMplus<-function(at,l) {
  paste0(at, " ARE ",paste0(l,collapse=" "),";")
}

#' variable list
#' you could set as free (*), with a fixed value (<X>) or related to a variable ( (label) )
#' they could be part of the same definition, or separated (using ;)
#' 

typeConvMplus<-function(t) {
#print(t)
  as.character(sapply(t,function(x) {
    if      (x=="*")    {"*"} 
    else if (x=="")     {""}
    else if (str_sub(x,1,1)=="@") {paste0(x)}
    else                    {paste0("(",x,")")}
  }))
}

varlistMplus<-function(v,type=rep("*",length(v)), means=FALSE, separated=TRUE) {
  if(means) {separated=TRUE}
  
  v.r<-typeConvMplus(type)
  out1<-paste0(v,v.r)
  if(means) {
    out1<-paste0("[",out1,"]")
  }
  if(separated) {
    paste0(out1,";")
  } else {
    paste0(out1,collapse=" ")
  }
}

thresholdMplus<-function(n.v, x, type=rep("*",length(th))) {  
  levels.var<-length(unique(na.omit(x)))-1
  v.r<-typeConvMplus(type)
  paste0("[",n.v,"$",1:levels.var,"]",v.r,";")
}
#' Write data file in free format for Mplus
#' @param x     a data.frame or matrix
#' @param filename    Filename 
#' @export
writeDataFileMplus<-function(x,filename) {
  write.table(x,file=filename,na="-99",row.names=FALSE,col.names=FALSE)
}
