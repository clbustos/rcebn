calculate.moderation.windows<-function(x,y,m,k=NULL) {
  if(is.null(k)) {
    k=floor(length(x)/10)
  }
  df.1<-data.frame(x=x,m=m,y=y)
  df.2<-df.1[order(df.1$m),]
  seq.k<-seq(1,nrow(df.2),k)
  res<-data.frame(m=numeric(length(seq.k)), b=numeric(length(seq.k)), se=numeric(length(seq.k)))
  
  ii<-1
  for(i in seq.k) {
    df.local<-df.2[i:(i+k),]
    mm<-mean(df.local$m,na.rm=T)
    lm.1<-lm(y~x,df.local)
    
    ss<-summary(lm.1)$coefficients
    
    res[ii,]<-c(m=mm,b=ss[2,1],se=ss[2,2])
    ii<-ii+1
  }
  res
}

calculate.moderation.kernel<-function(x,y,m,h=NULL) {
  n<-length(y)
  df.1<-data.frame(x=x,m=m,y=y)
  df.2<-df.1[order(df.1$m),]
  if(is.null(h)) {
    h<-bw.SJ(df.2$m)
  }
  
  res<-sapply(1:n,function(i) {
    w <- (1/h)*dnorm( (df.2$m[i]-df.2$m)/h)
    lm.local<-lm(y~x,weights = w,data=df.2)
    suppressWarnings(ss<-summary(lm.local)$coefficients)
    if(nrow(ss)==1) {
      c(m=df.2$m[i], b=NA,se=NA)
    } else {
      c(m=df.2$m[i], b=ss[2,1],se=ss[2,2] )
    }
  })
  
  data.frame(t(res))
}


#' Plot a moderation chart.
#'
#' Moderation plot allows to evaluate if a moderation is lineal, polynomial or have another form.
#' Two methods to calculate the moderation are used:
#' - Windows: Windows of size k are taken from the sample and on each the regression is calculated
#' - Kernel: A weigthed regression is calculated for each point, using a gaussian kernel, using the
#'          parameter h as bandwidth
#'
#' You could play with k and h parameters, until you have a good approximation to data
#' @param x Predictor
#' @param y Variable to predict
#' @param m Moderator
#' @param k Number of cases to create a windows
#' @param h
#'
#' @return ggplot
#' @export
#'
#' @examples
plotModeration<-function(x,y,m,k=NULL,h=NULL) {
  if(class(m) %in% c("factor","character")) {
    lm.1<-lm(y~x*m)
    sjPlot::plot_model(lm.1,type="pred",terms=c("x","m"))
  } else {
    library(ggplot2)
    res1<-calculate.moderation.windows(x,y,m,k=k)
    res2<-calculate.moderation.kernel(x,y,m,h=h)
    ggplot(res2,aes(x=m,y=b))+geom_line()+geom_point(mapping=aes(x=m,y=b),data=res1)+geom_errorbar(data=res1,width=0.1,aes(x=m,ymin=b-se*1.96,ymax=b+se*1.96))+geom_smooth(data=res1,aes(x=m,y=b), method="loess")
  }
}


