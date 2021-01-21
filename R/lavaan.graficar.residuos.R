#' Grafica los residuos por ítem de un modelo en lavaan
#' @param x lista de modelos lavaan
#'
#' @export
lavaan.graficar.residuos<-function(x) {
  library(ggplot2)
  if(inherits(x,"lavaan")) {
    x<-list(x)
  }
  if(is.null(names(x))) {
    names(x)<-paste0("M:",1:length(x))
  }
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

#' Grafica los residuos por ítems para un análisis factorial
#' La idea es de Juan Luis Castillo
#' @param modelo fa
#'
#' @export

fa.graficar.residuos<-function(x, delete.diagonal=TRUE) {
  library(ggplot2)
  if(inherits(x,"fa")) {
    x<-list(x)
  }
  if(is.null(names(x))) {
    names(x)<-paste0("M:",1:length(x))
  }
  ress<-lapply(names(x),function(i) {
    xx<-x[[i]]
    residuos<-resid(xx)
    if(delete.diagonal) {
      diag(residuos)<-0
    }

    res.st<-sqrt(residuos^2)
        res.m<-rowMeans(res.st)
    res.sd<-apply(res.st, 1,sd)
    data.frame(item=names(res.m), m=as.numeric(res.m),sd=as.numeric(res.sd),Modelo=i)
  })
  ress.f<-do.call(rbind,ress)

  ggplot(ress.f, aes(x=item,y=m, color=Modelo,group=Modelo))+geom_point()+geom_line()+coord_flip()

}

