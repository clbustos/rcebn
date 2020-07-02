#' Presenta resultados de KMO / Bartlett de forma estándar
#' @param x matriz de correlaciones
#' @param base base de datos en bruto
#'
#' @export
presentar.kmo.bartlett<-function(x, base) {
  kmo<-KMO(x)
  bar<-cortest.bartlett(x,n = nrow(base))
  pandoc.p(c("El resultado de KMO=", round(kmo$MSA,2) , " y de la prueba de Bartlett, ",sprintf("X²(%d) = %0.3f, p=%s", bar$df,bar$chisq, format.pval(bar$p.value,eps = 0.001)), " muestra que la matriz de correlaciones es apta para el análisis factorial exploratorio.\n"))
  pandoc.table(kmo$MSAi,"MSA individuales")
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
  pandoc.table(cebn::compareFitMatrix(list(x),scaled = scaled,nested=F), "Indices de ajuste")
  pandoc.table(ss, "Solución estandarizada")
  pandoc.table(modificationindices(x,sort.=T,maximum.number = 10),"Indices de modificación")
  if(!lavInspect(x,"post.check")) {
    pandoc.p(pandoc.strong("El modelo presentó problemas."))
    var.neg<-parameterestimates(x) %>% filter(op=="~~" & lhs==rhs & est<0)
    if(nrow(var.neg)>0) {
      pandoc.table(var.neg,"Varianzas negativas")
    } else {
      pandoc.p("No hay varianzas negativas")
    }
    cor.lv<-lavInspect(cfa.mod.2,"cor.lv")
    bad.cor<-cor.smoother(cor.lv)$bad
    if(!is.null(bad.cor)) {
      pandoc.table(cor.lv,"Matriz de correlaciones latentes")
      pandoc.table(bad.cor, "Variables que generan problemas en matriz ")
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
  pandoc.table(rr)
}
