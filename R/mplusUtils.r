#' Clean a name for Mplus
cleanMplusName<-function(x) {
  str_replace_all(x,"[^a-zA-Z0-9]","")
}

parlistMplus<-function(at,l) {
  paste0(at, " ARE ",paste0(l,collapse=" "),";")
}

#' variable list
#' you could set as free (*), with a fixed value (@<X>) or related to a variable ( (label) )
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
  
  levels.var<-length(unique(x))-1
  v.r<-typeConvMplus(type)
  paste0("[",n.v,"$",1:levels.var,"]",v.r,";")
}
