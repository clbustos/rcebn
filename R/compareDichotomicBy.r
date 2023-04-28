#' Generate a canonical description table to describe multiple dichotomic variables
#' 
#' By default, the first level is the 'correct' answer.
#' Includes: 
#' - Proportion of correct 
#' - Chi-Square test
#' - Effect size: Cramer V
#' @param vars a data.frame with variables
#' @param g groups to divide the information into
#' @param varnames variables to analyze
#' @param success value to considered as success
#' @param descriptives value to present as descriptive. Could be 'n', 'p' or 'both' 
#' @export
compareDichotomicBy<-function(vars,g,varnames=colnames(vars),success=NULL,descriptives='both') {

# Copied from sjmisc
cramer<-function (tab) 
{
  
    n<-sum(tab)
    c1<-chisq.test(tab)
    sqrt((c1$statistic/n)/(min(nrow(tab)-1,ncol(tab)-1)))
}


  g<-factor(g)
  n.g<-length(levels(g))
  k<-ncol(vars)
  
  #var.names<-colnames(vars)
  out.desc<-matrix(0,k,n.g)
  colnames(out.desc)<-levels(g)
  test.var<-character(k)
  p.values<-numeric(k)
  es<-numeric(k)
  for(i in 1:k) {    
    v=vars[,i]
    
    if(is.null(success)) {
      success<-levels(v)[1]
    }
    x.num<-aggregate(v==success,list(g=g),sum,na.rm=T)$x

    x.prop<-aggregate(v==success,list(g=g),mean,na.rm=T)$x
    if(descriptives=='both') {
      out.desc[i,]<-paste0(x.num," ( ",round(x.prop*100,1),"%)")
    } else if (descriptives=='n') {
      out.desc[i,]<-x.num
    } else if (descriptives=='p') {
      out.desc[i,]<-round(x.prop*100,1)
    }
    if(!any(v==success)) {
      next;
    }
    tab<-table(v==success,g)
    if(min(tab)>5) {
        tt<-chisq.test(tab);
        test.var[i]<-sprintf("X^2(%0.1f) = %0.2f",tt$parameter,abs(tt$statistic))
        p.values[i]<-tt$p.value
    } else {
        tt<-fisher.test(tab);
        test.var[i]<-"Exacto de Fisher"
        p.values[i]<-tt$p.value

    }
    es[i]<-cramer(tab)

  }
#  print(out.desc)
  
  out<-data.frame(vars=varnames, out.desc,estadistico=test.var,valor.p=p.values,p.adjust=p.adjust(p.values,"holm"),es=es)
  out
}
