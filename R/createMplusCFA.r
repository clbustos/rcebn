#' Create a Mplus definition for a CFA
#' Requires a list of dataframes
#' @param list of dataframes
#' @param basename for files
#' @export
#' @import stringr
createMplusCFA<-function(x,bfile) {
  require("stringr")
  clean.name<-function(x) {
    str_replace_all(x,"[^a-zA-Z0-9]","")
  }

  data.files<-paste0(bfile,".dat")
  inp.files<-paste0(bfile,".inp")
  full.db<-do.call(cbind,x)
  n.fac<-names(x)
  varnames<-clean.name(colnames(full.db))
  write.table(full.db,file=data.files,na="-99",row.names=FALSE,col.names=FALSE)

  cat.p<-function(t) {
    cat(paste0(t,"\n"),file=inp.files, append=T)
  }
  
  cat("TITLE:  Generic analysis\n",file=inp.files)
  cat.p("DATA:")
  cat.p(paste0("FILE IS '",data.files,"';"))

  cat.p("VARIABLE:")
  
  cat.p(paste0("NAMES ARE ",paste0(varnames,collapse=","),";"))
  cat.p(paste0("USEVARIABLES ARE ",paste0(varnames,collapse=","),";"))
  cat.p(paste0("CATEGORICAL ARE ",paste0(varnames,collapse=","),";"))
  cat.p("MISSING ARE ALL (-99);") 
  if(F) {
  cat.p("ANALYSIS:
  TYPE IS GENERAL ;
  INTEGRATION = MONTECARLO;
  ESTIMATOR IS MLR;
  ITERATIONS = 1000;
  CONVERGENCE = 0.00005;")
  }
  cat.p("MODEL:")
  i<-1
  for(j in 1:length(n.fac)) {
    dd<-x[[ n.fac[j] ]]
    f<-i+ncol(dd)-1
    cat.p(paste0(clean.name(n.fac[j]), " BY ", paste0(varnames[i:f], collapse=" "),";"))
    i<-f+1
  }
cat.p("OUTPUT:  SAMPSTAT STANDARDIZED TECH1 TECH4 MODINDICES(ALL);")

}
