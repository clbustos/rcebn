#' Create a Mplus definition for Generic Analysis
#' Automagicly assing factor as nominal and ordered as categorical
#' @param data.frame
#' @param basename for files
#' @export
#' @import stringr
createMplusGeneric<-function(x, directory=".", bfile=NULL) {
  
  data.files<-paste0(directory,"/",bfile,".dat")
  inp.files<-paste0(directory,"/",bfile,".inp")
  
  to.file=!is.null(bfile)
  # Create the cat method
  if(!to.file) {
    cat.p<-function(...) {
      cat(paste0(strwrap(paste0(...),80),"\n"))
    }
  } else {
    cat.p<-function(...,starting=FALSE) {
      cat(paste0(strwrap(paste0(...),80),"\n"), file=inp.files, append=!starting)
    }
  }
  
  varnames<-cleanMplusName(colnames(x))
  clases<-sapply(x,class)
  nominal<-varnames[which(clases=="factor")]
  categorical<-varnames[which(clases=="ordered")]
  
  
  if(to.file) {
    writeDataFileMplus(x,data.files)
  } else {
    print(head(x))
  }

  cat.p("TITLE:  Generic analysis\n",starting=T)
  cat.p("DATA:")
  cat.p(paste0("FILE IS '",data.files,"';"))  
  
  cat.p("VARIABLE:")
  cat.p( "NAMES ARE ",paste0(varnames,collapse=" "),";")
  cat.p("USEVARIABLES ARE  ",paste0(varnames,collapse=" "),";")
  if(length(nominal>0)) {
  cat.p("NOMINAL ARE  ",paste0(nominal,collapse=" "),";")
  }
  if(length(categorical>0)) {
  cat.p("CATEGORICAL ARE  ",paste0(categorical,collapse=" "),";")
  }
  cat.p("MISSING ARE ALL (-99);") 
  
  
  cat.p("ANALYSIS:")
  cat.p("!TYPE=MIXTURE;")
  cat.p("!STARTS = 50 20;")
  cat.p("!K-1STARTS = 10 2;")
  cat.p("!OPTSEED=0;")
  cat.p("!LRTSTARTS = 50 20 50 50;")
  
  cat.p("MODEL:")
  cat.p("! Free to play")
  cat.p("SAVEDATA:")
  cat.p(paste0("FILE IS ",bfile,".txt;"))
  cat.p(paste0("SAVE IS CPROB;"))
  cat.p(paste0("FORMAT IS FREE;"))
cat.p("!OUTPUT:  ;")
cat.p("OUTPUT: TECH11 ;")
cat.p("!OUTPUT: TECH11 TECH14 ;")

}
