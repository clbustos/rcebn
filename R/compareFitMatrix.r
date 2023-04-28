#' Compare fit-indexes for lavaan models
#' @param mods x
#' @param method x
#' @param scaled x
#' @param nested x
#' @param srmr.field x
#' @return a data.frame
#' @export
compareFitMatrix<-function(mods,method=NULL,scaled=F,nested=T, srmr.field="srmr") {
  if(scaled & is.null(method)) {
    method="satorra.2000"
  }
  nm<-names(mods)
  an<-NULL
  post<-ifelse(scaled,".scaled","")
  
  .as<-function(x) {paste0(x,post)}
  
  out<-sapply(1:length(mods),function(i) {
    out<-tryCatch( {
      
    fm0<-fitMeasures(mods[[i]],c(paste0(c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue"),post),srmr.field))
    
      #print(fm0)
      fm<-c(  chisq=sprintf("X^2(%d)=%0.2f, p=%0.3f",as.integer(fm0[2]),fm0[1],fm0[3]),
              chisq.nor=round(fm0[1]/fm0[2],3),
              cfi=round(fm0[ .as("cfi") ],3),
              tli=round(fm0[ .as("tli") ],3),
              srmr=round(fm0[srmr.field],3),
              rmsea=sprintf("%0.3f [%0.3f, %0.3f], p=%0.3f", fm0[ .as("rmsea") ], fm0[ .as("rmsea.ci.lower") ],   fm0[.as("rmsea.ci.upper") ], fm0[.as("rmsea.pvalue")]))
      #print(fm)
      if(nested) {
        
        tt.o<-c(diff.cfi=NA,chi.diff=NA)
        if(i>1) {
          chi.diff<-tryCatch({
            tt<-lavTestLRT(mods[[i]],mods[[i-1]],method=method)
            sprintf("X^2(%d)=%0.2f, p=%0.3f",as.integer(tt$`Df diff`[2]), tt$`Chisq diff`[2],tt$`Pr(>Chisq)`[2])
            }, error=function(e) as.character(e)
            )
          
          tt.o<-c(diff.cfi=round(fitMeasures(mods[[i]], .as("cfi") )-fitMeasures(mods[[i-1]], .as("cfi")),3),chi.diff=chi.diff)
        }
        c(fm,tt.o)
      } else {
        fm
      }
    },error=function(cond) {
      if(nested) {
        c(chisq=NA, chisq.nor=NA, cfi=NA, tli=NA, srmr=NA, rmsea=NA, diff.cfi=NA, chi.diff=NA)
      } else {
        c(chisq=NA, chisq.nor=NA, cfi=NA, tli=NA, srmr=NA, rmsea=NA)
      }
    })
    out
  })
  
  out<-t(out)
  if(nested) {
    colnames(out)<-c("X^2","X^2/gl","CFI","TLI","SRMR","RMSEA","Diff CFI","Diff X^2")
  } else {
    colnames(out)<-c("X^2","X^2/gl","CFI","TLI","SRMR","RMSEA")

  }
  rownames(out)<-nm
  out
}
