#' Returns a data.frame with correlations like the psych uses
#' @param x a data.frame
#' @param delete.lower if should delete the lower diagonal
#' @param ... other params, passed to corr.test
#' @export
corAsPsych<-function(x,delete.lower=T, round.digits=2,...) {
  c.m<-psych::corr.test(x,...)
  c.r<-c.m$r
  c.p<-c.m$p
  # Duplicate the lower triangule of c.p
  for(i in 1:(nrow(c.p)-1)) {
    for(j in (i+1):nrow(c.p)) {
      c.p[i,j]<-c.p[j,i]
    }
  }
  c.p1<-cut(as.numeric(c.p),breaks=c(-1,0.01,0.05,1.1),labels=c("**","*",""))
  c.p1<-matrix(c.p1,nrow(c.p),ncol(c.p))

  out<-list()
  for(i in 1:ncol(c.r)) {
    v<-colnames(c.r)[i]
    out[[paste0(v,"(r)")]]<-round(c.r[,i],round.digits)
    out[[paste0(v,"(p)")]]<-c.p1[,i]
  }

  if(delete.lower) {
    for(i in 1:(ncol(c.r)-1)) {
      v<-colnames(c.r)[i]
      out[[paste0(v,"(r)")]][(i+1):ncol(c.r)]<-""
      out[[paste0(v,"(p)")]][(i+1):ncol(c.r)]<-""
    }
  }
  data.frame(out)
}


# Esto fue culpa de Pablo Vergara (gracias, Pablo)

corAsPsych.mi<-function(x, variables=colnames(complete(x)), round.digits=2, only.r=FALSE) {
  cors.m<-miceadds::micombine.cor(x,variables = variables)
  cors.m$variable1<-as.character(cors.m$variable1)
  cors.m$variable2<-as.character(cors.m$variable2)
  
  val.r<-matrix(1, length(variables), length(variables), dimnames = list(variables, variables))
  val.p<-matrix(NA, length(variables), length(variables), dimnames = list(variables, variables))
  for(i in 1:nrow(cors.m)) {
    var1<-cors.m[i, "variable1"]  
    var2<-cors.m[i, "variable2"]
    
    vr<-cors.m[i, "r"]
    vp<-cors.m[i, "p"]
  #  cat("Var1:",var1,". var2:", var2, ".r:", vr,"\n")
    val.r[var1,var2]<-vr
    val.r[var2,var1]<-vr
    val.p[var1,var2]<-vp
    val.p[var2,var1]<-vp
  }
  
  if(only.r) {
    return(val.r)
  } else {
  val.p2<-gtools::stars.pval(val.p)
  do.call(cbind, lapply(1:ncol(val.r), function(i) {
    res.1<-data.frame(r=round(val.r[,i],2), p=val.p2[,i])
    colnames(res.1)<-paste0(variables[i], c(".r",".p"))
    res.1
    }))
  }
}

