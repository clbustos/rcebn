#' Create a Mplus definition for a (multilevel) LGM
#' Requires the clustering on parameter @cluster
#' And the different variables on dataframes of the same rank
#' @param data.frame
#' @param list of time.series
#' @param variable for clustering
#' @param basename for files
#' @export
#' @import stringr
createMplusLGM<-function(x, series, pred.within, pred.between, clusters, model.within=c("i","s"), model.between=c("ib","sb"), bfile=NULL) {
  require("stringr")
  to.file<-!is.null(bfile)
  clean.name<-function(x) {
    str_replace_all(x,"[^a-zA-Z0-9]","")
  }

  data.files<-paste0(bfile,".dat")
  inp.files<-paste0(bfile,".inp")
  full.db<-do.call(cbind,x)
  
  
  n.fac<-names(x)
  varnames<-clean.name(colnames(full.db))
  
  if(to.file) {
    write.table(full.db,file=data.files,na="-99",row.names=FALSE,col.names=FALSE)
  } else {
    print(head(full.db))
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
  
  cat("TITLE:  Latent growth model\n",file=inp.files)
  cat.p("DATA:")
  cat.p(paste0("FILE IS '",data.files,"';"))

  cat.p("VARIABLE:")
  cat.p( str_wrap(paste0("NAMES ARE ",paste0(varnames,collapse=" "),";"),80) )
  cat.p(str_wrap(paste0("USEVARIABLES ARE  ",paste0(varnames,collapse=" "),";"),80))
  if(!missing(clusters)) {
    if(!missing(pred.within)) cat.p(paste0("WITHIN ARE ",paste0(clean.name(pred.within),collapse=" "),";"))
    if(!missing(pred.between)) cat.p(paste0("BETWEEN ARE ",paste0(clean.name(pred.between),collapse=" "),";"))
    
  }
  cat.p("MISSING ARE ALL (-99);") 
  if(!missing(clusters)) {
    cat.p(paste0("CLUSTER = ",clusters))
    cat.p("ANALYSIS:
    TYPE=TWOLEVEL")
  }
  if(F) {
  cat.p("ANALYSIS:
  TYPE IS GENERAL ;
  INTEGRATION = MONTECARLO;
  ESTIMATOR IS MLR;
  ITERATIONS = 1000;
  CONVERGENCE = 0.00005;")
  }
  cat.p("MODEL:")
  # Within section
  if(!missing(clusters)) {
    cat.p("%WITHIN%")
  }
  for(j in 1:length(series)) {
    
    n.serie  <-names(series)[j]
    var.serie<-clean.name(colnames(x[,series[[j]]]))
    int.slope<-paste0(clean.name(n.serie), model.within,collapse=" ")
    times<-seq(0,length(var.serie)-1)
    time.serie<-paste(var.serie,times,sep="@",collapse=" ")
    cat.p(paste0( int.slope , " | ", time.serie,";"))
    # Constraint on variance
    cat.p(paste0(paste0(var.serie,collapse=" ")," (",j,");"))
    
    if(!missing(pred.within)) {
        cat.p(paste0(int.slope," ON ", clean.name(pred.within), ";"))
    }
  }
  
  
  if(!missing(clusters)) {
    cat.p("%BETWEEN%")
    for(j in 1:length(series)) {
      n.serie  <-names(series)[j]
      var.serie<-clean.name(colnames(x[,series[[j]]]))
      int.slope<-paste0(clean.name(n.serie), model.between,collapse=" ")
      times<-seq(0,length(var.serie)-1)
      time.serie<-paste(var.serie,times,sep="@",collapse=" ")
      cat.p(paste0( int.slope , " | ", time.serie,";"))
      # Constraint in mean
      
      

      # Constraint on variance
      cat.p(paste0(var.serie,"@0",";"))
      #cat.p(paste0("[",var.serie,"]",";"))
      
      
      if(!missing(pred.between)) {
        cat.p(paste0(int.slope," ON ", clean.name(pred.between), ";"))
      }
    }
  }
  
  
  
cat.p("OUTPUT:  SAMPSTAT STANDARDIZED TECH1 TECH4 MODINDICES(ALL);")

}
