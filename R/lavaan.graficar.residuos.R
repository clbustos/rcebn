#' Grafica los residuos por ítem de un modelo en lavaan
#' @param x lista de modelos lavaan
#'
#' @export
lavaan.graficar.residuos<-function(x) {
  library(ggplot2)
  ress<-lapply(names(x),function(i) {
    xx<-x[[i]]
    res.st<-sqrt(resid(xx,type="cor")$cov^2)
    res.m<-rowMeans(res.st)
    res.sd<-apply(res.st, 1,sd)
    data.frame(item=names(res.m), m=as.numeric(res.m),sd=as.numeric(res.sd),Modelo=i)
  })
  ress.f<-do.call(rbind,ress)

  ggplot(ress.f, aes(x=item,y=m, color=Modelo,group=Modelo))+geom_point()+geom_line()+coord_flip()
}
