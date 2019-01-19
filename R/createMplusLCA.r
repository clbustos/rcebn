#' Create a Mplus definition for Latent Class Analysis
#'
#' @param x data.frame with data
#' @param classes number of latent classes
#' @param lc.vars list of variables to perform latent class. By default: all
#' @param predictors variable for regression on latent class. By default: none
#' @param bfile basename for files
#' @export
createMplusLCA<-function(x, classes, lc.vars=colnames(x), predictors, bfile=NULL) {
  to.file<-!is.null(bfile)
  clean.name<-function(x) {
    stringr::str_replace_all(x,"[^a-zA-Z0-9]","")
  }

  data.files<-paste0(bfile,".dat")

  if (to.file) {
    inp.files<-paste0(bfile,".inp")
  } else {
    inp.files<-NULL
  }

  lc.varnames<-clean.name(lc.vars)
  df.pred<-NULL
  categorical<-lc.vars
  usevar<-colnames(x)
  if(!missing(predictors)) {
    if(!is.null(predictors$nominal)) {
      usevar<-colnames(x)[! (colnames(x) %in% predictors$nominal)]
      for(i in predictors$nominal) {
        x[[i]]<-as.numeric(x[,i])
      }
    }
    df.pred<-lapply(predictors,clean.name)
    categorical<-c(categorical,predictors$categorical)
  }
  categorical.varnames<-clean.name(categorical)
  total.pred<-c(df.pred$numerical,df.pred$categorical)


  varnames<-clean.name(colnames(x))


  if(to.file) {
    write.table(x,file=data.files,na="-99",row.names=FALSE,col.names=FALSE)
  } else {
    print(head(x))
  }
  if(to.file) {
    cat.p<-function(t) {
      cat(paste0(t,"\n"),file=inp.files, append=T)
    }
  } else {
    cat.p<-function(t) {
      cat(paste0(t,"\n"))
    }
  }
  if(to.file) {
  cat("TITLE:  Latent class analysis\n",file=inp.files)
  } else {
  cat("TITLE:  Latent class analysis\n")
  }
  cat.p("DATA:")
  cat.p(paste0("FILE IS '",data.files,"';"))

  if(!missing(predictors) & !is.null(predictors$nominal)) {
     cat.p("DEFINE:")

      for(v in predictors$nominal) {
        l.v<-max(x[,v],na.rm=T)
        for(j in 1:l.v){
          cn<-clean.name(v)
          cat.p(paste0(cn,j," = ",cn," ==",j,";"))
          if(j>1) {
            total.pred<-c(total.pred,paste0(cn,j))
            usevar<-c(usevar,paste0(cn,j))
          }
        }
      }
  }
  print(usevar)


  cat.p("VARIABLE:")
  cat.p( str_wrap(paste0("NAMES ARE ",paste0(varnames,collapse=" "),";"),80) )
  cat.p(str_wrap(paste0("USEVARIABLES ARE  ",paste0(clean.name(usevar),collapse=" "),";"),80))
  cat.p(str_wrap(paste0("CATEGORICAL ARE  ",paste0(categorical.varnames,collapse=" "),";"),80))

  cat.p(paste0("CLASSES = C(",classes,");"))
  cat.p("MISSING ARE ALL (-99);")


  cat.p("ANALYSIS:")
  cat.p("TYPE=MIXTURE;")
  cat.p("STARTS = 50 20;")
  cat.p("!K-1STARTS = 10 2;")
  cat.p("!OPTSEED=0;")
  cat.p("LRTSTARTS = 50 20 50 50;")


  if(!missing(predictors)) {
    cat.p("MODEL:")
    cat.p("%OVERALL%")
    cat.p(str_wrap(paste0("c on ",paste0(total.pred,collapse=" "),";"),80))
  }
cat.p("SAVEDATA:")
  cat.p(paste0("FILE IS ",bfile,".txt;"))
  cat.p(paste0("SAVE IS CPROB;"))
  cat.p(paste0("FORMAT IS FREE;"))
cat.p("!OUTPUT:  ;")
cat.p("OUTPUT: TECH11 ;")
cat.p("!OUTPUT: TECH11 TECH14 ;")

}
