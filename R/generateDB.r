#' Follow a variable definition, composed of 1 and 0
#' and create both a database complete set
#' and a means dataframe.
#' Like scoreItems, but less cumbersome
#' @param db the database
#' @param def the definition matrix. Should have 'v' parameter on in
#' @param v name of the column names 
#' @param ignore names of columns to ignore. By default, 'n'
#' @export
generateDB<-function(db, def, v='var',ignore=c()) {
  if(!v %in% colnames(def)) {
    stop(paste0("you should have a column called ",v))
  }
  to_ignore=c(v,ignore)
  
  escalas<-names(def)[!(names(def) %in% to_ignore)]
  
  def.2<-def[,escalas]
  vars<-as.character(def[,v])
  out.db<-list()
  out.means<-list()
  out.sums<-list()
  for(i in 1:length(escalas)) {
    n.e.actual<-as.numeric(escalas[i])
    sel<-def.2[,i]
    
    sel[is.na(sel)]<-0
    sel.l <- sel!=0
    v.act<-vars[sel.l]
    ind<-sel[sel.l]
    #print(escalas[i])
    # print(ind)
    #print(v.act)
    if(length(v.act)>0) {
      print(v.act)
      n.vars<-length(v.act)
      out.db[[ escalas[i] ]]<-db[,v.act]
     # print(str(db[,v.act]))
      if(any(ind<0)) {
        for(j in which(ind<0)) {
          out.db[[ escalas[i] ]][,j]<- -ind[j] - out.db[[ escalas[i] ]][,j] 
        }
      }
      out.means[[ escalas[i] ]]<- rowMeans(out.db[[ escalas[i] ]],na.rm=T)
      out.sums[[ escalas[i] ]]<- out.means[[ escalas[i] ]]*n.vars

    }
  }
  
  out<-list(db=out.db,means=data.frame(out.means),sums=data.frame(out.sums))
  class(out)<-"generateDB"
  invisible(out)
}
