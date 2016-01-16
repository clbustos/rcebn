#' Calculate likehood function for a factorial solution
#' @param s.c sample covariance matrix
#' @param model estimated model
#' @param ll optional loadings 
#' @export

calculate.ml.f<-function(s.c,model=NULL,ll=NULL) {
  if(is.null(model) && is.null(ll)) {
    stop("You should use a model or loadings")
  }
  if(!is.null(ll)) {
    model<-ll%*%t(ll)
  } 
  p<-nrow(s.c)
  diag(model)<-rep(1,p)
  
  as.numeric(determinant(model)$modulus-determinant(s.c)$modulus + tr(s.c%*%solve(model))-p)
}

#' Se supone que la función objetivo se multiplica por (n-1), pero en 
#' fa se multiplica por cualquier custion
#' n * (n - 1)/2 - n * nfactors + (nfactors *  (nfactors - 1)/2)
#' para ser exactos
