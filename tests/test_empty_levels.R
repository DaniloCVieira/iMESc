# Factors with levels without observations (e.g. a class absent after filtering a Datalist):
# the levels are kept, so each class keeps its colour, and the tools that need classes with
# observations drop them
d<-araca_data()
envi<-d$envi
set.seed(1)
cl<-factor(sample(1:6,nrow(envi),replace=TRUE))
fac<-attr(envi,"factors")
fac$HC6<-cl
fac$txt<-as.character(fac[[1]])
attr(envi,"factors")<-fac
p1<-envi[cl!=5,]
p1<-data_migrate(envi,p1)
f1<-attr(p1,"factors")

# Filter observations (data_migrate) keeps the levels
check("data_migrate keeps the levels without observations",identical(levels(f1$HC6),as.character(1:6))&&!any(f1$HC6==5))
check("data_migrate keeps columns that are not factors",identical(f1$txt,fac$txt[cl!=5]))

# palettes over all the levels, named by level
pal<-viridis::turbo
cols<-pal_levels(pal,f1$HC6)
check("pal_levels: all levels, named",identical(unname(cols),pal(6))&&identical(names(cols),as.character(1:6)))
g<-ggplot2::ggplot(data.frame(x=seq_along(f1$HC6),y=1,f=f1$HC6),ggplot2::aes(x,y,color=f))+ggplot2::geom_point()+ggplot2::scale_color_manual(values=cols)
b<-ggplot2::ggplot_build(g)$data[[1]]
check("ggplot: class 6 keeps its colour when class 5 is empty",all(b$colour[f1$HC6==6]==pal(6)[6])&&all(b$colour[f1$HC6==4]==pal(6)[4]))

# map circles: the colours of the present classes from the palette over all the levels
mc<-col_factor_map(list(turbo=pal),"turbo",data.frame(f1$HC6),FALSE)
check("map: colours of the present classes",identical(unname(mc),pal(6)[-5]))
# map pies: the colour of each slice is the colour of its class
dfp<-data.frame(x=1,y=1,A_split_1=1,A_split_6=2,A_split_10=1)
f10<-factor(c(1,6,10),levels=c(1:6,10))
pc<-get_cols_factor_chart(dfp,data.frame(z=f10),pal(7))
check("map pies: slices coloured by class (not alphabetical)",identical(substr(pc,1,7),substr(pal(7)[c(1,6,7)],1,7)))

# Descriptive: scatter and histogram colour a class by its level
df<-desc_scatter_data(setNames(p1[,1],rownames(p1)),setNames(p1[,2],rownames(p1)),data.frame(HC6=f1$HC6,row.names=rownames(p1)))
fr<-desc_scatter_fit(df,"lm",band="confidence",level=0.95)
gs<-ggplot2::ggplot_build(gg_desc_scatter(df,colors=pal))$data
check("scatter: class 6 keeps its colour",identical(sort(unique(gs[[1]]$colour)),sort(pal(6)[-5])))
gs<-ggplot2::ggplot_build(gg_desc_scatter(df,fitres=fr,colors=pal))$data
rib<-gs[[which(vapply(gs,function(x) "ymin"%in%names(x),logical(1)))[1]]]
check("scatter: fit bands coloured by class",identical(sort(unique(rib$fill)),sort(pal(6)[-5])))
long<-data.frame(variable=factor("v"),value=p1[,1],group=droplevels(f1$HC6))
attr(long,"group_levels")<-levels(f1$HC6)
gh<-ggplot2::ggplot_build(gg_desc_hist(long,colors=pal))$data[[1]]
check("histogram: class 6 keeps its colour",identical(sort(unique(gh$fill)),sort(pal(6)[-5])))

# Supervised: the training response drops the empty class; observed and predicted on the same levels
vals<-shiny::reactiveValues(saved_data=list(p1=p1),newcolhabs=list(turbo=pal))
shiny::testServer(sl_model_setup$server,args=list(vals=vals),{
  session$setInputs(data_y="p1",model_type="Classification",var_y="HC6",partition="None")
  check("SL: training response without the empty class",identical(levels(y_train()[,1]),c("1","2","3","4","6")))
  check("SL: training response passed on (rfGA, two-class check, spatial folds)",identical(levels(vals$cur_var_y[,1]),c("1","2","3","4","6")))
})
m<-tryCatch(caret::train(p1[,1:3],droplevels(f1$HC6),method="rpart",trControl=caret::trainControl(method="none"),tuneGrid=data.frame(cp=0.01)),error=function(e) e)
check("SL: caret trains with the empty class dropped",inherits(m,"train"))
obs<-factor(c("1","2","9"),levels=c("1","2","5","9"))
po<-sl_align_classes(obs,factor(c("1","2","2"),levels=c("1","2","3")))
tb<-table(po$obs,po$pred)
check("SL: confusion matrix square (model classes + observed unknown class)",nrow(tb)==ncol(tb)&&identical(levels(po$obs),c("1","2","3","9")))
cm<-tryCatch(caret::confusionMatrix(tb),error=function(e) e)
check("SL: caret confusionMatrix runs",!inherits(cm,"error"))

# spatial validation: class counts per fold aligned with the classes
fold_cl<-evaluate_fold_classes(data.frame(y=f1$HC6),list(folds_ids=rep(1:3,length.out=nrow(p1))))
check("spatial folds: the empty class is not counted as absent",all(fold_cl$n_classes_absent_test==0|fold_cl$min_n_test_class>0)&&all(fold_cl$n_classes_test<=5))
if(requireNamespace("sf",quietly=TRUE)){
  set.seed(2)
  n<-120
  pts<-sf::st_as_sf(data.frame(x=runif(n,-48,-47),y=runif(n,-25,-24),cl=factor(sample(c("a","b","d"),n,TRUE),levels=c("a","b","c","d"))),coords=c("x","y"),crs=4326)
  sb<-tryCatch(suppressWarnings(suppressMessages(cv_spatial2(pts,column="cl",k=3,size=20000,selection="random",iteration=5,progress=FALSE,plot=FALSE,report=FALSE))),error=function(e) e)
  if(!inherits(sb,"error")){
    # in each fold, train + test of a class = all its observations; each observation is tested once
    rec<-as.matrix(sb$records)
    n_cl<-as.vector(table(droplevels(pts$cl)))
    check("spatial blocks: counts per class match the data (empty class ignored)",
          ncol(rec)==6&&all(sweep(rec[,1:3]+rec[,4:6],2,n_cl)==0)&&all(colSums(rec[,4:6])==n_cl))
  } else check(paste("spatial blocks run (",conditionMessage(sb),")"),FALSE)
}

# Exchange factor -> numeric (binary): no all-zero column for the empty class
bin<-do.call(cbind,lapply(droplevels(f1["HC6"]),kohonen::classvec2classmat))
check("Exchange binary: one column per class with observations",ncol(bin)==5&&all(colSums(bin)>0))

# segRDA breaks from a factor: an empty class in the middle does not hide a break
bp<-droplevels(factor(c(1,1,2,2,4,4),levels=1:4))
check("segRDA: breaks found across an empty class",identical(which(diff(as.numeric(bp[order(as.numeric(bp))]))==1),c(2L,4L)))

# the savepoint of the report: the filtered Datalists keep the levels when filtered again
sp_file<-Sys.getenv("IMESC_EMPTY_LEVELS_SP","")
if(nzchar(sp_file)&&file.exists(sp_file)){
  sd<-readRDS(sp_file)$saved_data
  par<-sd[["Fito_01_10_2026_hellinger"]]
  per<-attr(par,"factors")$PERIODO
  q1<-data_migrate(par,par[per=="Periodo 1",])
  q2<-data_migrate(par,par[per=="Periodo 2",])
  h1<-attr(q1,"factors")$HC6_DS; h2<-attr(q2,"factors")$HC6_DS
  check("savepoint: both periods keep the six HC classes",identical(levels(h1),levels(h2))&&nlevels(h1)==6)
  mc1<-col_factor_map(list(turbo=pal),"turbo",data.frame(h1),FALSE)
  mc2<-col_factor_map(list(turbo=pal),"turbo",data.frame(h2),FALSE)
  common<-intersect(levels(droplevels(h1)),levels(droplevels(h2)))
  check("savepoint: the classes have the same colours in both periods",
        identical(unname(mc1[match(common,levels(droplevels(h1)))]),unname(mc2[match(common,levels(droplevels(h2)))])))
}
