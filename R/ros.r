#' Region of significance analysis
#' @param lm or lmer model
#' @param predictor Name of predictor variable, as appears on coef calling
#' @param moderador Name of moderator variable, as appears on coef calling
#' @param points Matrix, with first columns values for predictor and second for moderator
#' @param simple_slopes Vector with moderator values
#' @param alpha Level of significance for test
#' @param f.test F-test is valid for all points. If false, t.test would test for one specific point  
#' @export
ros<-function(model,predictor,moderador,points=NULL,simple_slopes=NULL,alpha=0.05) {
  v<-vcov(model)
  if(class(model)=="lm") {
    ef<-coef(model)
    n<-nrow(model$model)
  } else {
    ef<-lme4::fixef(model)
    n<-nrow(lme4::getME(model,"X"))
  }

  inter<-paste0(predictor,":",moderador)
  if(!inter %in% names(ef)) {
    inter<-paste0(moderador,":",predictor)
  }


#  print(inter)
  f.y00<-ef["(Intercept)"]
  f.y11<-ef[inter]
  f.y10<-ef[predictor]
  f.y01<-ef[moderador]
  if(is.na(f.y10)) {
    stop("Incorrect predictor name")
  }
  if(is.na(f.y01)) {
    stop("Incorrect moderator name")
  }
  if(is.na(f.y11)) {
    stop("No interaction effect was modeled")
  }


#  print(f.y11)
  v.y11<-v[inter,inter]
  v.y10<-v[predictor,predictor]
  v.y01<-v[moderador,moderador]
  c.y10.y11<-v[predictor,inter]
  df<-df.residual(model)
  # Los grados de libertad lo saque de interactions::johnson_neyman
  # También están en https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3203541/ como n-4
  # que se entiende porque son n-constante-f1-f2-int
  
  t.crit<-(qt(1-(alpha/2),df))^2
  #print(t.crit)
  i.n<-"(Intercept)"
  # Ordered Variance/Covariance matrix
  vcv.o<-matrix(
    c(v[i.n,i.n], v[i.n,predictor], v[i.n,moderador],v[i.n,inter],
    v[predictor,i.n], v[predictor,predictor], v[predictor,moderador],v[predictor,inter],
    v[moderador,i.n], v[moderador,predictor], v[moderador,moderador],v[moderador,inter],
    v[inter,i.n], v[inter,predictor], v[inter,moderador],v[inter,inter]
    ),
    4,4
  )

  #t.crit<-1.96

  a1<-t.crit * v.y11 - f.y11^2

  b1<- 2 * ( t.crit* c.y10.y11-f.y10*f.y11)

  c1<-t.crit*v.y10-f.y10^2
  #cat("a:",a1," b:",b1," c:",c1,"\n")
  r1<- (-b1 - sqrt(b1^2-4*a1*c1))/(2*a1)
  r2<- (-b1 + sqrt(b1^2-4*a1*c1))/(2*a1)
  x1<-min(r1,r2)
  x2<-max(r1,r2)
  #cat("x1:",x1,"\nx2:",x2,"\n")
  d.f.points<-NULL
  d.f.ss<-NULL
  if(!is.null(points)) {
    pp<-cbind(rep(1,nrow(points)),as.matrix(points),points[,1]*points[,2])

    bb<-matrix(c(f.y00,f.y10,f.y01,f.y11),4,1)
    res<-pp%*%bb
    de.res<-sqrt(diag(pp%*%vcv.o%*%t(pp)))
    d.f.points<-data.frame(pred=points[,1],mod=points[,2],res=res, de.res=de.res)
  }
  if(!is.null(simple_slopes)) {
    zeros<-rep(0,length(simple_slopes))
    ones<-rep(1,length(simple_slopes))
    pp<-cbind(zeros,ones,zeros,simple_slopes)

    bb<-matrix(c(f.y00,f.y10,f.y01,f.y11),4,1)
    res<-pp%*%bb

    de.res<-sqrt(diag(pp%*%vcv.o%*%t(pp)))
    #print(de.res)
    d.f.ss<-data.frame(z=simple_slopes,res=res, de.res=de.res)
  }
  out<-list(ros=c(x1,x2),points=d.f.points,simple_slopes=d.f.ss)
  class(out)<-"ros"
  out
}

# Based on a pool object
micombine.ros.mixed<-function(pool,...) {
  lapply(pool$analyses,
    function(x) {
      ros(model=x,...)
    }
  )
}

#' Based on mi.mixed.ancova.analysis
#' generates a ros object
#' @param ca Modelo desde un calcular ancova propiop
#' @param moderador la variable moderadora (usualmente el pre) TAL CUAL APARECE EN EL MODELO
#' @param se simple slopes -> puntos donde se define el efecto
#' @export
ros.from.maa<-function(ca,moderador,se) {

  ca.1.m<-micombine.ros.mixed(ca$pool.slope,predictor="experimental2",moderador=moderador,simple_slopes=se)
  res1<-apply(sapply(ca.1.m,function(x) {x$ros}),1,median,na.rm=T)
  res2<-rowMeans(sapply(ca.1.m,function(x) {x$simple_slopes$res}))
  res3<-rowMeans(sapply(ca.1.m,function(x) {x$simple_slopes$de.res}))
  list(ros=res1,ss=data.frame(mod=se,efecto=res2,ee=res3))
}
