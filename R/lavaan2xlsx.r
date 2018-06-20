style.cor<-function(wb,ws,rows,cols) {
  s <- createStyle(numFmt = "0.00")

  negStylem3 <- createStyle(fontColour = "#660000", bgFill = "#FFCCCC")
  negStylep3 <- createStyle(fontColour = "#330000", bgFill = "#FF6666")

  posStylem3 <- createStyle(fontColour = "#008800", bgFill = "#CCFFCC")
  posStylep3 <- createStyle(fontColour = "#006600", bgFill = "#99FF99")
  posStylep7 <- createStyle(fontColour = "#003300", bgFill = "#33FF33")

  zeroStyle <- createStyle(fontColour = "#CCCCCC", bgFill = "#FFFFFF")


  conditionalFormatting(wb, ws, rows = rows, cols = cols, rule=paste0("< -0.3"), style = negStylep3)
  conditionalFormatting(wb, ws, rows = rows, cols = cols, rule=paste0("< 0"), style = negStylem3)
  conditionalFormatting(wb, ws, rows = rows, cols = cols, rule=paste0("> 0.7"), style = posStylep7)

  conditionalFormatting(wb, ws, rows = rows, cols = cols, rule=paste0("> 0.3"), style = posStylep3)

  conditionalFormatting(wb, ws, rows = rows, cols = cols, rule=paste0("> 0"), style = posStylem3)
  conditionalFormatting(wb, ws, rows = rows, cols = cols, rule=paste0("= 0"), style = zeroStyle)
  conditionalFormatting(wb, ws, rows = rows, cols = cols, rule=paste0("< -0.3"), style = negStylep3)
  addStyle(wb, ws, style = s, rows = rows, cols = cols, gridExpand = TRUE)
  setColWidths(wb, ws, cols = cols, widths = 4.5)

}

#' Generate a xlsx file for a lavaan object
#'
#' @param x lavaan object
#' @param extra.matrix.f  extra matrix to add to x object
#' @param order.matrix.f name of columns to which order the cor matrix
#'
#' @return a openxlsx object
#' @export
#'
#' @examples
#' \dontrun{
#'  library(psych)
#'  library(lavaan)
#'  data(Dwyer)
#'  cfa.1<-cfa("g=~V1+V2+V3+V4+V5+V6+V7+V8",sample.cov=Dwyer,sample.nobs=1000)
#'  lavaan2xlsx(cfa.1)
#'  }
lavaan2xlsx<-function(x,extra.matrix.f=NULL,order.matrix.f=NULL) {
  require(openxlsx)
  require(reshape2)
  sta<-lavaan::standardizedsolution(x)
  sta.fac<-sta[sta$op=="=~",]

  factors=unique(sta.fac$lhs)

  #sta.cor<-sta[sta$op=="~~" & (sta$lhs %in%factors & sta$rhs %in%factors),]
  #sta.cor2<-rbind(data.frame(lhs=sta.cor$lhs,rhs=sta.cor$rhs, est.std=sta.cor$est.std), data.frame(rhs=sta.cor$lhs,lhs=sta.cor$rhs, est.std=sta.cor$est.std))

  m.fac<-dcast(sta.fac,rhs~lhs,fill = 0,value.var="est.std",fun.aggregate=mean)
  #m.cor<-dcast(sta.cor2,rhs~lhs,fill = 0,value.var="est.std",fun.aggregate=mean)
  m.cor<-lavInspect(x,"cor.lv")
  m.cor<-data.frame(item=colnames(m.cor),m.cor)

  row.fac<-nrow(m.fac)
  col.fac<-ncol(m.fac)

  col.fac.for<-2
  if(!is.null(extra.matrix.f)) {
    m.fac<-merge(extra.matrix.f,m.fac,by.x="item",by.y="rhs")

    col.fac.for<-2+ncol(extra.matrix.f)-1
  }
  if(is.null(order.matrix.f)) {
    m.fac<-m.fac[order(m.fac[,1]),]
  } else {
    m.fac<-m.fac[do.call(order,m.fac[,order.matrix.f,drop=F])   ,]
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "measurement")
  openxlsx::addWorksheet(wb, "cor")


  writeData(wb,"measurement",m.fac,startCol=1,startRow=1,rowNames=F,borders="surrounding")

  style.cor(wb,"measurement",rows=2:(row.fac+1), cols = col.fac.for:(col.fac.for+col.fac))
  writeData(wb,"cor",m.cor,startCol=1,startRow=1,rowNames=F,borders="surrounding")
  cor.nrows<-2:(nrow(m.cor)+1)
  cor.ncols<-2:(ncol(m.cor)+1)
  style.cor(wb,"cor",rows=cor.nrows,cols=cor.ncols)


    wb

}


