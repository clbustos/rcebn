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
  pander::pandoc.p(c("El resultado de KMO=", round(kmo$MSA,2) , " y de la prueba de Bartlett, ",sprintf("X²(%d) = %0.3f, p=%s", bar$df,bar$chisq, format.pval(bar$p.value,eps = 0.001)), " muestra que la matriz de correlaciones es apta para el análisis factorial exploratorio.\n"))
  pander::pandoc.table(kmo$MSAi,"MSA individuales")
}


#' Presenta resultados de CFA de forma estándarde forma estándar
#' @param x modelo lavaan
#' @param scaled si los resultados están escalados
#'
#' @export
presentar.cfa<-function(x, scaled=TRUE) {
  library(dplyr)
  library(magrittr)
  ss<-standardizedsolution(x)
  ss<-ss[ss$op %in% c("=~","~~"), ]
  pander::pandoc.table(cebn::compareFitMatrix(list(x),scaled = scaled,nested=F), "Indices de ajuste")
  pander::pandoc.table(ss, "Solución estandarizada")
  pander::pandoc.table(modificationindices(x,sort.=T,maximum.number = 10),"Indices de modificación")
  if(!lavInspect(x,"post.check")) {
    pander::pandoc.p(pandoc.strong("El modelo presentó problemas."))
    var.neg<-parameterestimates(x) %>% filter(op=="~~" & lhs==rhs & est<0)
    if(nrow(var.neg)>0) {
      pander::pandoc.table(var.neg,"Varianzas negativas")
    } else {
      pander::pandoc.p("No hay varianzas negativas")
    }
    cor.lv<-lavInspect(x,"cor.lv")
    bad.cor<-cor.smoother(cor.lv)$bad
    if(!is.null(bad.cor)) {
      pander::pandoc.table(cor.lv,"Matriz de correlaciones latentes")
      pander::pandoc.table(bad.cor, "Variables que generan problemas en matriz ")
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
