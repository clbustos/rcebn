#' Automatic Factor Analysis, to delete unnecesary factors and reageants
#' 
#' La idea es seleccionar automáticamente el número correcto de factores
#' e ítems, a partir de una solución inicial. 
#' Para ello, el criterio es que cada ítem pertenezca a un factor, al menor, 
#' definido como una carga superior a un umbral.
#' No me opo  ndré a las cargas cruzadas, por hoy al menos
#' Las reglas son:
#' - Si un factor queda sin ítem, se selecciona un número inferior de factor
#' - si hay un ítem que no carga en un factor, se elimina. Se elimina primero el ítem
#'   con menor comunalidad
#' 
#' @param x data.frame
#' @param nfactor Initial number of factors
#' @param cut factor loading threshold
#' @param check.crossloads if the model try to eliminate crossloads. You should
#'        disable this on biquartimin or other type of rotations 
#' @export
automatic_fa<-function(x,nfactor,cut.t=0.3,check.crossloads=T,...) {
  nfactor.a<-nfactor
  item.list<-data.frame(names=colnames(x),status=rep(TRUE,ncol(x)))
  cor.x<-cor(x,use="p")
  n<-nrow(x)
  fa.1<-psych::fa(cor.x, nfactors=nfactor,n.obs=n,...)
  log.i<-list()
  iteration=0
  while((res<-check.model.fa(fa.1,cut.t,check.crossloads))$status!="correct") {
    iteration=iteration+1
    message(res)
    if(res$status=="bad_item") {
      item.list$status[item.list$names %in% res$item]<-FALSE
    } else if(res$status=="less_factors") {
      nfactor.a<-nfactor.a-1
    }
    log.i[[iteration]]<-res
    cat("Status: n items=",sum(item.list$status),",factors=",nfactor.a,"\n")
    fa.1<-psych::fa(cor.x[item.list$status,item.list$status],n.obs=n,nfactor=nfactor.a,...)
  }
  out<-(list(log=log.i,items=item.list,nfactor=nfactor.a,fa=fa.1,cut.t=cut.t,iteration=iteration))
  class(out)<-"automatic_fa"
  out
}

#' @export
automatic_fa_bif<-function(x,nfactor,cut.t=0.3,check.crossloads=T,...) {
  nfactor.a<-nfactor
  item.list<-data.frame(names=colnames(x),status=rep(TRUE,ncol(x)))
  
  cor.x<-cor(x,use="p")
  n<-nrow(x)
  omega.1<-omega(cor.x,flip=F,n.obs=n,...)
  log.i<-list()
  iteration=0
  while((res<-check.model.fa(omega.1,cut.t,check.crossloads))$status!="correct") {
    iteration=iteration+1
    message(res)
    if(res$status=="bad_item") {
      item.list$status[item.list$names %in% res$item]<-FALSE
      
    } else if(res$status=="less_factors") {
      nfactor.a<-nfactor.a-1
    }
    log.i[[iteration]]<-res
    cat("Status: n items=",sum(item.list$status),",factors=",nfactor.a,"\n")
    omega.1<-psych::omega(cor.x[item.list$status,item.list$status],n.obs=n,nfactor=nfactor.a,flip=FALSE,...)
  }
  out<-(list(log=log.i,items=item.list,nfactor=nfactor.a,fa=omega.1,cut.t=cut.t,iteration=iteration))
  class(out)<-"automatic_fa"
  out
}


#' @export
check.model.fa<-function(fa.m,cut.t,check.crossloads) {
  if(inherits(fa.m,"omega")) {
    ll<-fa.m$schmid$oblique
    good.item<-fa.m$schmid$sl[,1]
  } else {
    ll<-loadings(fa.m)
    good.item<-fa.m$communality
  }
  
  nfactors<-ncol(ll)
  soluc<-cluster2factor(ll,cut.t)
  soluc.sin.0<-soluc[soluc!=0]
  
  cargas.cruzadas<-rowSums(ll>cut.t)>1
  
  #print(sum(cargas.cruzadas))
  print(table(soluc))
  
  if(sum(soluc==0)==0 && (!check.crossloads || sum(cargas.cruzadas)==0)) {
    return(list(status="correct"))
  } else if(length(unique(soluc.sin.0))<nfactors) {
    return(list(status="less_factors"))
  } else if(sum(soluc==0)>0) {
    factor.0<-soluc==0
    #print(fa.m$communality[soluc==0])
    item=names(which.min(good.item[soluc==0]))
    return(list(status="bad_item",type="no_factor",item=item))
  } else if(!check.crossloads || sum(cargas.cruzadas)>0) {
    item=names(which.min(good.item[cargas.cruzadas]))
    return(list(status="bad_item",type="cross_load",item=item))
  } else {
    stop("I don't know what the error is")
  }
}

#' Print results of automatic_fa
#'
#' @param automatic_fa class
#' @export
print.automatic_fa<-function(x) {
  cat("Threshold:",x$cut.t,"\n")
  cat("Items:",sum(x$items[,2]),"\n")
  cat("Factors:",x$nfactor,"\n")
  cat("Iterations:",x$iteration,"\n")
  soluc<-cluster2factor(x$fa,x$cut.t)
  print(table(soluc))
}
