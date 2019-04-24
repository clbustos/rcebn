cor.corrected<-function(x,keys,partialize=NULL,self.correct=TRUE, length.correct=TRUE,standarize=T) {
  x<-x[,rownames(keys)]

  pp<-nrow(keys)
  if(standarize) {
    x<-data.frame(scale(x))
  }
  totals<-as.matrix(x)%*%as.matrix(keys)
  if(is.null(partialize)) {
    cor.mar<-cor(x,totals)

  }
  if(!is.null(partialize)) {
    big.data<-cbind(x,totals)
    big.cor.mar<-cor(big.data)
    partialized.big.cols<-pp+partialize
    np.keys<-which(!(1:ncol(keys) %in% partialize))
    non.partialized<-c(1:pp,pp+np.keys)
    cor.mar.p<-partial.r(big.cor.mar, non.partialized,partialize)
    cor.mar<-cor.mar.p[1:pp,pp+np.keys]
    keys<-keys[,np.keys]
  }

  if(self.correct) {
    for(escala in 1:ncol(keys)) {
      t.escala=totals[,escala]
      items=which(keys[,escala]==1)
      for(item in items) {
        t.escala.mi<-t.escala-x[,item]
        cor.mar[item,escala]<-cor(t.escala.mi,x[,item])
      }
    }
  }

  if(length.correct) {
    cor.items<-cov(x)

    for(escala in 1:ncol(keys)) {
      items=which(keys[,escala]==1)
      k<-rep(length(items),pp)-as.numeric(keys[,escala]==1)
      cov.items.mi<-cor.items[items,items]
      alpha=k/(k-1)*(1-(sum(diag(cov.items.mi))) / sum(cov.items.mi))
      cor.mar[,escala]<-cor.mar[,escala] / (sqrt( (1-alpha)/k +alpha ))
    }
  }

  cor.mar
}

adjust.assignment.omg<-function(x,keys,partialize=NULL,...) {
  x.act<-x
  keys.act<-keys
  correct=FALSE
  while(!correct) {
    ac<-omg(x.act,keys.act,...)
    #print(ac$correct)
    if(all(ac$correct)) {
      correct=TRUE
    } else {
    }
  }
  list(x=x.act,keys=keys.act)

}

jacknife.omg<-function(x,keys,...) {
  x<-x[,rownames(keys)]
  x.act<-x
  keys.act<-keys
  correct=FALSE
  while(!correct) {
    ac<-omg(x.act,keys.act,...)
    #print(ac$correct)
    if(all(ac$correct)) {
      correct=TRUE
    } else {
      nc<-omg.num.correct.is.deleted(x=x.act,keys=keys.act,subset=!ac$correct,...)
      max.mejora<-max(nc)
      items.candidatos<-which(!ac$correct & nc ==max.mejora)
      if(length(items.candidatos)==0) {
        items.candidatos<-which(!ac$correct)
      }
      ac.cand<-ac[items.candidatos,]
      print(items.candidatos)
      print(ac.cand)
      #return(ac.cand)
      if(length(items.candidatos)==1) {
        item.to.delete=items.candidatos
      } else {
        #item.to.delete=sample(items.candidatos,1)

        item.to.delete<-items.candidatos[which.max(ac.cand$cor.max)]
      }
      cat("Items:",ncol(x.act),"->Deleted:(",max(nc),")",item.to.delete,"\n")
      x.act<-x.act[,-item.to.delete]
      keys.act<-keys.act[-item.to.delete,]
      keys.act<-keys.act[,colSums(keys.act)>0]
    }
  }
  list(x=x.act,keys=keys.act)
}

omg.num.correct.is.deleted<-function(x,keys,subset=NULL,...) {
  correctas<-numeric(ncol(x))
  if(is.null(subset)) {
    subset=1:ncol(x)
  } else if(is.logical(subset)) {
    subset=which(subset)
  }
  for(i in subset) {
    x1<-x[,-i]
    keys1<-keys[-i,]

    correctas[i]<-sum(omg(x1,keys1,...)$correct)
  }
  names(correctas)<-colnames(x)
  correctas
}


#' Run an OMG Analysis,
#'
#' As Stuive, Kiers & Timmerman(2009) said,"The OMG method considers for each item the observed correlations between the item and each subtest, where each subtest is an unweighted sumscore of items. These correlations are corrected for two sources of spuriously high correlations, namely, self-correlation and test length[...] An assignment of an item to a subtest is considered to be correct when the corrected subtest-item correlation is highest for the subtest the item is supposed to belong to and incorrect otherwise" (p.950)
#'
#' @param x matrix or data.frame
#' @param keys keys of assigment of
#' @param ... arguments to cor.corrected
#' @return a data.frame
#'
#'   \describe{
#'   \item{item}{Item name}
#'   \item{assigned.name}{Name of scale (came from keys)}
#'   \item{cor.assigned}{Corrected correlation of item to assigned scale}
#'   \item{max.name}{Name of scale for maximum correlation}
#'   \item{cor.max}{Maximum correlation to any scale}
#'   \item{diff}{Difference between assigned and maximum correlation.}
#'   }
#'
#' @seealso \code{\link{cor.corrected}} for corrected correlation for self-correlation and scales lengths.
#' @examples
#' library(qgraph)
#' data(big5)
#' data(big5groups)
#' big.keys<-sapply(big5groups,function(x) {xx<-numeric(ncol(big5));xx[x]<-1;xx})
#' rownames(big.keys)<-colnames(big5)
#' omg(big5,big.keys)
#'
#' @export
omg<-function(x,keys,partialize=NULL,...) {

  # Check if keys rownames are correct
  
  rn.keys<-rownames(keys)
  
  if(!all(rn.keys %in% colnames(x))) {
    stop("Some rownames in keys are not available as colnames in x")
  }
  
  x<-x[, rn.keys]
  if(!all(complete.cases(x))) {
    stop("Missing cases in x")
  }

  cc<-abs(cor.corrected(x=x,keys=keys,partialize=partialize,...))

  
  if(!is.null(partialize)) {
    keys<-keys[,which(!(1:ncol(keys) %in% partialize))]
  }
  scale.n<-colnames(cc)
  max.cor.v<-apply(cc,1,max)

  maxs.cor<-apply(cc,1,which.max)
  maxs.keys<-apply(keys,1,which.max)
  cor.assigned<-apply(cc*as.matrix(keys),1,max)

  data.frame( item=colnames(x),
  assigned.name=scale.n[maxs.keys],
  assigned.n=maxs.keys,
  cor.assigned=cor.assigned,
  max.name=scale.n[maxs.cor],
  max.n=maxs.cor,
  cor.max=max.cor.v,
  diff    = max.cor.v-cor.assigned,
  correct = maxs.cor==maxs.keys )
}

#' Create a xlsx for an omg analysis
#'
#' @param x matrix or data.frame
#' @param keys keys of assigment of
#' @param ... arguments to cor.corrected
#'
#' @return A Workbook object
#' @export
#'
omg.xlsx<-function(x,keys,order.keys=T,...) {
  x<-data.frame(x[,rownames(keys)])
  if(order.keys) {
    keys2cluster<-rowSums(as.matrix(keys)%*%diag(1:ncol(keys)))
    print(keys2cluster)
    order.keys<-order(keys2cluster,rownames(keys))
    x<-x[,order.keys]
    keys<-keys[order.keys,]
  }
  cc<-cor.corrected(x,keys,...)
  omg<-omg(x,keys,...)

  wb<-openxlsx::createWorkbook()
  ws<-openxlsx::addWorksheet(wb,"cor.corrected")
  openxlsx::writeDataTable(wb,1,data.frame(cc),rowNames=T)
  correct<-createStyle(border="TopBottomLeftRight", borderStyle="medium",textDecoration="bold",fgFill="#CCFFCC")
  incorrect<-createStyle(border="TopBottomLeftRight", borderStyle="medium",textDecoration="bold",fgFill="#FFCCCC")
  possible<-createStyle(fgFill="#FFDDFF")
  addStyle(wb,1,rows = which(omg$correct)+1,cols=omg$assigned.n[omg$correct]+1, style = correct,gridExpand = F)
  addStyle(wb,1,rows = which(!omg$correct)+1,cols=omg$assigned.n[!omg$correct]+1, style = incorrect,gridExpand = F)
  # Where are the correct ones
  possible.ones <- abs(cc)>abs(omg$cor.assigned)
  possible.ones.rr<-rowSums(possible.ones)
  for(i in 1:nrow(cc)) {
    if(possible.ones.rr[i]>0) {

      addStyle(wb,1,rows=i+1,cols=which(possible.ones[i,])+1,style=possible)
    }
  }
  wb
}
