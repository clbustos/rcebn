#' Create a Mplus definition for a CFA
#' Requires a list of dataframes
#' @param list of dataframes
#' @param basename for files
#' @export
#' @import stringr
createMplusCFA<-function(x,groups,directory=".",bfile=NULL,invariance_factors=F,title="CFA Analysis",difftest=NULL) {
  require("stringr")
  
  data.files<-paste0(directory,"/",bfile,".dat")
  inp.files<-paste0(directory,"/",bfile,".inp")
  
  
  # Create the cat method
  if(is.null(bfile)) {
    cat.p<-function(...) {
      cat(paste0(strwrap(paste0(...),80),"\n"))
    }
  } else {
    cat.p<-function(...,starting=FALSE) {
      cat(paste0(strwrap(paste0(...),80),"\n"), file=inp.files, append=!starting)
    }
  }
  
  
  full.db<-do.call(cbind,x)
  full.db.x<-full.db
  
  cat.names<-varnames<-cleanMplusName(do.call(c,lapply(x,function(x) {colnames(x)})))
  n.fac<-names(x)

  if(!missing(groups)) {
    varnames<-c(varnames,"groups")
    full.db<-cbind(full.db,as.numeric(groups))
    levels.gr<-length(unique(groups))
  }
  
  write.table(full.db,file=data.files,na="-99",row.names=FALSE,col.names=FALSE)

  
  nom.facs<-cleanMplusName(n.fac)
  
  items.factors<-function() {
    out<-character(length(n.fac))
    i<-1
    for(j in 1:length(n.fac)) {
      dd<-x[[ n.fac[j] ]]
      f<-i+ncol(dd)-1
      out[j]<-paste0(nom.facs[j], " BY ", varlistMplus(varnames[i:f],c("@1",rep("*",ncol(dd)-1)), separated=F), ";")
      i<-f+1
    }
    out
  }
  
  cat.p("TITLE:",title,starting=T)
  cat.p("DATA:")
  cat.p("FILE IS '",data.files,"';")

  cat.p("VARIABLE:")
  
  cat.p(parlistMplus("NAMES",       varnames))
  cat.p(parlistMplus("USEVARIABLES",varnames))
  cat.p(parlistMplus("CATEGORICAL", cat.names))

  if(!missing(groups)) {
    def.l<-paste0(1:levels.gr," = g",1:levels.gr, collapse=" ")
    cat.p("GROUPING IS  groups  ( ",def.l,") ");
  }
  cat.p("MISSING ARE ALL (-99);")
  cat.p("ANALYSIS:
  
  PARAMETERIZATION=THETA;
  ITERATIONS=5000;
  SDITERATIONS=30;
  ")
  if(!is.null(difftest)) {
  cat.p("DIFFTEST=",difftest,".dif")
    }
  # Model
  cat.p("MODEL:")
  
  cat.p(items.factors())
  cat.p(varlistMplus(nom.facs,"@0",means=T));


  # Definimos la configuración de invarianza de configuración, al menos...
  if(!missing(groups)) {
    cat.p(varlistMplus(nom.facs, "*"));
    
    for(j in 1:length(n.fac)) {
      dd<-x[[ n.fac[j] ]]
      for(k in 1:ncol(dd)) {
        levels.var<-length(unique(dd[,k]))-1
        n.x<-cleanMplusName(colnames(dd)[k])
        cat.p(thresholdMplus(n.x,dd[,k], type=paste0("t",j,"i",k,"l",1:levels.var) ))
      }
    }
    
    for(gr in 2:levels.gr) {
      cat.p("MODEL g",gr,":")
      cat.p(varlistMplus(nom.facs, "*")); # varianzas de los factores
      cat.p(varlistMplus(nom.facs,"*",means=T)); # medias de los factores
      if(!invariance_factors) {
        cat.p(items.factors())  
      }
      
      for(j in 1:length(n.fac)) {
        dd<-x[[ n.fac[j] ]]
        for(k in 1:ncol(dd)) {
          levels.var<-length(unique(dd[,k]))-1
          n.x<-cleanMplusName(colnames(dd)[k])
          top.l<- 1+as.numeric(k==1)
          cat.p(thresholdMplus(n.x,dd[,k], type=c(paste0("t",j,"i",k,"l ",1:top.l),rep("",levels.var-top.l) )))
        }
      }
      # varianza de los residuales
      cat.p(varlistMplus(cat.names,"*"))
      
      
    }
  }
cat.p("OUTPUT:  SAMPSTAT STANDARDIZED TECH1 TECH2 TECH4 MODINDICES(ALL);")
cat.p("!OUTPUT:  TECH1; STDYX;")
cat.p("SAVEDATA: DIFFTEST=",bfile,".dif;")

}
