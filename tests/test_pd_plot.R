# Partial dependence plot (Supervised Algorithms > Partial dependence) drawn with ggplot2, without
# autoplot() of pdp (students got "Objects of class <partial> are not supported by autoplot")
set.seed(2)
d<-data.frame(a=rnorm(50),b=rnorm(50),f=factor(sample(c("p","q"),50,TRUE)))
d$y<-d$a*2+d$b+rnorm(50,sd=0.2)
m<-caret::train(d[,c("a","b","f")],d$y,method="rf",trControl=caret::trainControl(method="none"),tuneGrid=data.frame(mtry=2),ntree=20)
pd<-pdp::partial(m,pred.var=c("a","b"),type="regression",grid.resolution=5,chull=TRUE)
p<-plot_pd_single(pd,main="",legend.title="Partial\ndependence")
check("PD plot of two numeric predictors: ggplot with tiles and contours, axis labels",
      inherits(p,"ggplot")&&length(p$layers)==2&&identical(p$labels$x,"a")&&identical(p$labels$y,"b")&&
        !inherits(try(print(p),silent=TRUE),"try-error"))
pdf<-pdp::partial(m,pred.var=c("f","b"),type="regression",grid.resolution=5)
pf<-plot_pd_single(pdf,main="t")
check("PD plot with a factor predictor: tiles only, drawn",
      length(pf$layers)==1&&!inherits(try(print(pf),silent=TRUE),"try-error"))
