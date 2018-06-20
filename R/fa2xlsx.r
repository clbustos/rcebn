#' fa2xlsx
#' View Creates a xlsx with different factorial analysis side by side
#' Marking the loadings to adscribe to a factor.
#' @param ... one or more factorial analysis
#' @param cut Minimal loading to assign a item to a factor
#' @param cut.diff Minimal difference between loading to assign it to a specific one
#' @param order.items A string, according to specification. 
#'                    "factor" orders items according to new factors and loads 
#'                    "o.factor" orders according original factors and loads
#'                    "o.factor-items" orders according to factors and items names
#' @param original.factors Vector of names for original factors. Useful for target rotations
#' @param filename Filename of the excel file. If NULL, create a new temporary file on each iteration
#' @export
fa2xlsx<-function(...,cut=0.3,cut.diff=0.1,order.items="factor", original.factors=NULL,filename=NULL,open.file=TRUE) {
  wb=fa2xlsx.wb(list(...),cut=cut,cut.diff=cut.diff,order.items=order.items, original.factors=original.factors,filename=filename)
  if(open.file) {
    if(!is.null(filename)) {
      openXL(filename)
    } else {
      openXL(wb)
    }
  }
}

#' fa2xlsx.wb
#' Creates a xlsx with different factorial analysis side by side
#' Marking the loadings to adscribe to a factor
#' @param x a list with factorial analysis
#' @param cut Minimal loading to assign a item to a factor
#' @param cut.diff Minimal difference between loading to assign it to a specific one
#' @param order.items Order items according to factor and loadings
#' @param filename Filename of the excel file. If NULL, create a new temporary file on each iteration
#' @return an Workbook object
#' @export
fa2xlsx.wb<-function(x,cut=0.3,cut.diff=0.1,order.items="factor", original.factors=NULL,filename=NULL) {
  require(openxlsx)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "fa")
  sc=1
  s <- createStyle(numFmt = "0.00")
  negStyle <- createStyle(fontColour = "#330000", bgFill = "#FF6666")
  posStyle <- createStyle(fontColour = "#003300", bgFill = "#66FF66")
  errStyle <- createStyle(fontColour = "#330033", bgFill = "#FF66FF")

  if(is.null(names(x))) {
    names(x)<-paste0("FA",1:length(x))
  }
  for(f in names(x)) {
    if(inherits(x[[f]],"omega")) {
      loads<-x[[f]]$schmid$oblique
    } else {
      loads<-loadings(x[[f]])
    }

    class(loads)<-"matrix"
    n.vars<-nrow(loads)
    n.factors<-ncol(loads)

    num.factor<-cluster2factor(loads,cut)

    max.per.factor<-apply(loads,1,function(x) {max(abs(x))})

    difs<-apply(loads,1,function(x) {x<-abs(x);y<-tail(sort(x),2); y[2]-y[1] })


    loads.2<-data.frame(id=1:n.vars,loads,factor=num.factor,difs=difs)
    if(!is.null(original.factors)) {
      loads.2$o.factor<-original.factors
    }
    
    if(order.items=='factor') {
      order.loads<-order(loads.2$factor,-max.per.factor)
      loads.2<-loads.2[order.loads,]
    } else if (order.items=='o.factor' && !is.null(original.factors)) {

      order.loads<-order(loads.2$o.factor,loads.2$factor,-max.per.factor)
      loads.2<-loads.2[order.loads,]
    }else if (order.items=='o.factor-items' && !is.null(original.factors)) {

      order.loads<-order(loads.2$o.factor,rownames(loads.2))
      loads.2<-loads.2[order.loads,]
    }
    writeData(wb,1,f,startCol=sc,startRow=1)
    writeData(wb,1,loads.2,startCol=sc,startRow=2,rowNames=TRUE,borders="surrounding")


    conditionalFormatting(wb, 1, rows = 2:(2+n.vars), cols = (sc+2):(sc+n.factors+1), rule=paste0("> 1"), style = negStyle)
    conditionalFormatting(wb, 1, rows = 2:(2+n.vars), cols = (sc+2):(sc+n.factors+1), rule=paste0("< -1"), style = errStyle)

    conditionalFormatting(wb, 1, rows = 2:(2+n.vars), cols = (sc+2):(sc+n.factors+1), rule=paste0("< -",cut), style = negStyle)
    conditionalFormatting(wb, 1, rows = 2:(2+n.vars), cols = (sc+2):(sc+n.factors+1), rule=paste0(">",cut), style = posStyle)
    conditionalFormatting(wb, 1, rows = 2:(2+n.vars), cols = sc+n.factors+3, rule=paste0("<",cut.diff), style = negStyle)

    addStyle(wb, 1, style = s, rows = 2:(2+n.vars), cols = (sc+2):(sc+n.factors+1), gridExpand = TRUE)

    sc=sc+n.factors+5+(!is.null(original.factors))
  }
  if(!is.null(filename)) {
    saveWorkbook(wb,filename,TRUE)
  }
  wb
}
#' Return the assigned factor to a given solution
#' @param x fa
#' @param cut threshold
#' @export

  cluster2factor<-function(x,cut) {
    if(inherits(x,"fa")) {
      loads<-as.matrix(loadings(x))
    } else {
      loads<-x
    }
    n.factors<-ncol(loads)
    #print(loads)
    f2c<-factor2cluster(loads,cut=cut)
    if(ncol(f2c)!=ncol(loads)) {
        f2c=cbind(f2c,matrix(0,nrow(loads),n.factors-ncol(f2c)))
    }
#    print(str(f2c))
    f2c %*% matrix(1:n.factors,n.factors,1)

}
