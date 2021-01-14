#' Generate a standard APA table for linear models on Excel
#'
#' Generate an Excel workbook with one or more lineal
#' models.
#' @param list of lm models
#' @param nested if TRUE, presents R² and F for difference betweeen models
#'
#' @return an openxlsx object. You could use \code{OpenXL} to browse on spreadsheet software
#' @export

lmToXlsx<-function(x,nested=T) {
  ast.valor.p<-function(x) {
    cut(x,c(-1,0.01,0.05,1),labels=c("**","*",""))
  }
  library(openxlsx)
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = paste0("Modelos"), gridLines = FALSE)
    # minrow
    minrow=0
    for(i in 1:length(x)) {
      if(minrow < length(coef(x[[i]]))) {
        minrow=length(coef(x[[i]]))
      }
    }
    mod.pre<-NULL
    r2.pre<-NULL
    if(!is.null(names(x))) {
      n.mods<-names(x)
    } else {
      n.mods<-paste0("Modelo ",1:length(x))
    }
    n.mod<-names(x)
  for(i in 1:length(x)) {
    column=(i-1)*5+1

    lm.i<-x[[i]]
    slm.i<-summary(lm.i)
    cii<-confint(lm.i)
    print(slm.i)
    cii2<-data.frame(IC=sprintf("[%0.3f , %0.3f]", cii[,1],cii[,2]))
    rows<-length(coef(lm.i))

    writeData(wb,sheet=1, n.mods[i], startCol=column,startRow=1)
    writeDataTable(wb, sheet = 1, data.frame(coeficientes=names(coef(lm.i))), startCol = column, startRow = 3)
    writeDataTable(wb, sheet = 1, data.frame(estimador=round(coef(lm.i),3)), startCol = column+1, startRow = 3)
    writeData(wb, sheet = 1, ast.valor.p(slm.i$coefficients[,4]), startCol = column+2, startRow = 4)
    writeDataTable(wb, sheet = 1, cii2, startCol = column+3, startRow = 3)
    #print(slm.i$coefficients[,4])

    # R2
    writeData(wb,sheet=1, "R2",startCol=column,startRow=3+minrow+2)
    writeData(wb,sheet=1, round(slm.i$r.squared,3),startCol=column+1,startRow=3+minrow+2)
    # F
    fst<-slm.i$fstatistic
    writeData(wb,sheet=1, "F",startCol=column,startRow=3+minrow+3)
    writeData(wb,sheet=1, round(fst[1],3),startCol=column+1,startRow=3+minrow+3)
    writeData(wb,sheet=1, ast.valor.p(1-pf(fst[1],fst[2],fst[3])),startCol=column+2,startRow=3+minrow+3)
    if(!is.null(mod.pre) & nested) {
      dr2<-summary(lm.i)$r.squared - summary(mod.pre)$r.squared
      an1<-anova(mod.pre,lm.i)
      writeData(wb,sheet=1, "d R2",startCol=column,startRow=3+minrow+4)
      writeData(wb,sheet=1, round(dr2,3),startCol=column+1,startRow=3+minrow+4)

      writeData(wb,sheet=1, "d F",startCol=column,startRow=3+minrow+5)
      writeData(wb,sheet=1, round(an1[2,5],3),     startCol=column+1,startRow=3+minrow+5)
      writeData(wb,sheet=1, ast.valor.p(an1[2,6]), startCol=column+2,startRow=3+minrow+5)
    }
    r2.pre<-summary(lm.i)$r.squared
    mod.pre<-lm.i
  }
  wb
}
