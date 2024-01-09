#' Presenta resultados de KMO / Bartlett de forma estándar
#' @param x matriz de correlaciones
#' @param base base de datos en bruto
#' @param ordinal si se debe usar la matriz de correlaciones policórica
#'
#' @export
presentar.kmo.bartlett<-function(x, n=NULL, policorica=FALSE) {
  if(policorica) {
    x1<-lavaan::lavCor(x, ordered=colnames(x))
  } else {
    x1<-x
  }
  
  
  kmo<-KMO(x1)
  if(is.null(n)) {
    bar<-cortest.bartlett(x1)
  } else {
    bar<-cortest.bartlett(x1 ,  n=n)
  }
  pander::pandoc.p(c("The result of KMO=", round(kmo$MSA,2) , " and Bartlet test, ",sprintf("X^2(%d) = %0.3f, p=%s", bar$df,bar$chisq, format.pval(bar$p.value,eps = 0.001)), " shows that correlation matrix is adequate for exploratory factorial analysis.\n"))
  pander::pandoc.table(kmo$MSAi,"Individual MSA")
}


#' Presenta resultados de CFA de forma estándarde forma estándar
#' @param x modelo lavaan
#' @param scaled si los resultados están escalados
#'
#' @export
presentar.cfa<-function(x, scaled=TRUE) {
  library(dplyr)
  library(magrittr)
  library(lavaan)
  library(pander)
  ss<-standardizedsolution(x)
  ss<-ss[ss$op %in% c("=~","~~"), ]
  pander::pandoc.table(cebn::compareFitMatrix(list(x),scaled = scaled,nested=F), "Model fit indices")
  pander::pandoc.table(ss, "Standarized solution")
  try(pander::pandoc.table(modificationindices(x,sort.=T,maximum.number = 10),"Modification indices"))
  if(!lavInspect(x,"post.check")) {
    pander::pandoc.p(pandoc.strong("There are some errors in the model that need to be fixed."))
    var.neg<-parameterestimates(x) %>% filter(op=="~~" & lhs==rhs & est<0)
    if(nrow(var.neg)>0) {
      pander::pandoc.table(var.neg,"Negative variances")
    } else {
      pander::pandoc.p("There are not negative variances")
    }
    cor.lv<-lavInspect(x,"cor.lv")
    bad.cor<-cor.smoother(cor.lv)$bad
    if(!is.null(bad.cor)) {
      pander::pandoc.table(cor.lv,"Matrix of latent correlations")
      if(length(bad.cor)>0) {
        try(pander::pandoc.table(bad.cor, "Variables that generate problems in matrix"))
      }
    }
  }

}

#' Presenta resultados de un omega
#' @param x resultado de omega u omegaSem
#'
#' @export
presentar.omega<-function(x) {

  rr<-x$omega.group
  rn<-c("G", paste0("F",1:(nrow(rr)-1)))
  #print(rr)
  #print(rn)
  rownames(rr)<-rn
  pander::pandoc.table(rr)
}
