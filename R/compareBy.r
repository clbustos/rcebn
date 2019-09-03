#' Generate a canonical description table to describe multiple variables for a
#' classification criterium
#' Includes:
#' - Mean and sd for each group
#' - Report of normality of each grupo
#' - Test of differences, adapted to normality of each group
#' - Effect size of differences: d for two groups, f for three or more
#' @param vars a data.frame with variables
#' @param g vector with groups to divide the information into
#' @param use.2.bm: Use brunner.munzel.test on variables with two groups
#' @param use.3.eta2: Use eta squared, instead of <code>f</code>, on comparison of
#'                    three or more groups
#' @param test.type "variable" for contingent on normality test, "parametric" for
#'                  t and ANOVA and "nonparametric" for U and Kruskall-Wallis
#' @importFrom stats wilcox.test t.test anova aov kruskal.test lm aggregate sd shapiro.test
#' @export
compareBy<-function(vars,g,varnames=colnames(vars),use.2.bm=FALSE,use.3.eta2=FALSE, test.type="variable") {
  g<-factor(g)
  # Factor by factor...
  if(any(is.na(g))) {
    warning("Group variable contains NA")
    cases.with.g<-!is.na(g)
    vars<-vars[cases.with.g,]
    g<-factor(g[cases.with.g])
  }
  n.g<-length(levels(g))
  k<-ncol(vars)

  #var.names<-colnames(vars)
  out.desc<-matrix(0,k,n.g*3)
  colnames(out.desc)<-paste0(paste0(gl(n.g,3,labels=levels(g)),   rep(c(".n",".M",".DE"),n.g)))
  test.var<-character(k)
  p.values<-numeric(k)
  es<-numeric(k)
  for(i in 1:k) {
    unico<- any(sapply(split(vars[,i], g),function(xx) { length(unique(xx))==1}))
    x.n<-aggregate(vars[,i],list(g=g),function(x) {sum(!is.na(x))})$x
    x.p<-x.n/sum(x.n)
    x.m<-aggregate(vars[,i],list(g=g),mean,na.rm=T)$x
    x.sd<-aggregate(vars[,i],list(g=g),sd,na.rm=T)$x
    x.n<-aggregate(vars[,i],list(g=g),function(x) {sum(!is.na(x))})$x

    if(!unico)  {
      x.st<-aggregate(vars[,i],list(g=g),function(x) {
      if(length(x)>2 & length(x)<5000 ) {
        shapiro.test(x)$p.value>0.05
      } else {
        NA
      }
      })$x
    }
    out.desc[i,seq(1,n.g*3,3)]<-x.n
    out.desc[i,seq(2,n.g*3,3)]<-x.m
    out.desc[i,seq(3,n.g*3,3)]<-x.sd

    all.normals<-all(x.st)


    pool.sd<-sqrt(pooled.variance(vars[,i],g))

    m.pooled<-sum(x.m*x.p)
    sigma_m<-sqrt(sum(x.p*(x.m-m.pooled)^2))

    if(n.g==2) {
      es.name="ES(d)"
      if(use.2.bm) {

        if (!requireNamespace("lawstat", quietly = TRUE)) { #nocov start
          stop("lawstat package needed for this function to work. Please install it.",
               call. = FALSE)
        } #nocov end
        pr.t="W"
        xx<-split(vars[,i],g)
        x1<-xx[[1]]
        x2<-xx[[2]]
        tt<-lawstat::brunner.munzel.test(x1,x2)
        test.var[i]<-sprintf("W(%0.1f)=%0.2f",tt$parameter,abs(tt$statistic))
        p.values[i]<-tt$p.value
      } else {
        if( (test.type=="variable"  && !is.na(all.normals) && all.normals) || (test.type=='parametric')) {
          tt<-t.test(vars[,i]~g);
          #print(tt$parameter)
          test.var[i]<-sprintf("t(%0.1f)=%0.2f",tt$parameter,abs(tt$statistic))
          p.values[i]<-tt$p.value
        } else {
          tt<-wilcox.test(vars[,i]~g)
          test.var[i]<-sprintf("U=%0.2f",tt$statistic)
          p.values[i]<-tt$p.value
        }
      }
      es[i]<-abs(x.m[1]-x.m[2])/pool.sd
    } else {
      if(use.3.eta2) {
      es.name="ES(eta²)"
      } else {
      es.name="ES(f)"
      }
      if(is.na(all.normals)) {
        test.var[i]<-NA
        p.values[i]<-NA
        es[i]<-NA
      } else {
        if(test.type=='parametric' || (test.type=="variable" && all.normals)) {
          tt<-anova(aov(vars[,i]~g));
          #print(tt$"F value")
          test.var[i]<-sprintf("F(%d, %d)=%0.2f",tt$Df[1],tt$Df[2],abs(tt$"F value"[1]))

          p.values[i]<-tt$"Pr(>F)"[1]
        } else {
          tt<-kruskal.test(vars[,i]~g)
          test.var[i]<-sprintf("X²(%d)=%0.2f",tt$parameter,tt$statistic)
          p.values[i]<-tt$p.value
        }
        if(use.3.eta2) {
          es[i]<-summary(lm(vars[,i]~g))$r.squared
        } else {
          es[i]<-sigma_m/pool.sd
        }
      }

    }
  }
#  print(out.desc)

  out<-data.frame(vars=varnames, out.desc,estadistico=test.var,valor.p=p.values,p.adjust=p.adjust(p.values,"holm"),es=round(es,2))
  c.out<-colnames(out)
  c.out[length(c.out)]<-es.name
  colnames(out)<-c.out
  out
}

pooled.variance<-function(x,g) {
  x2<-na.omit(data.frame(x=x,g=g))

  x.n<-aggregate(x2$x,list(g=x2$g),length)$x
  x.var<-aggregate(x2$x,list(g=x2$g),var)$x

  num<-sum(x.var*(x.n-1))
  den<-length(x)-length(levels(g))
  #print(x.var)
  num/den
}
