# Spatial CV folds saved in the Factor-Attribute and read back (Supervised Algorithms > Spatial CV >
# 'Save folds to Factor-Attribute' / 'Folds from Factor-Attribute')
set.seed(5)
n<-80
co<-data.frame(lon=runif(n,-46,-45),lat=runif(n,-24,-23),row.names=paste0("s",1:n))
y<-factor(sample(c("A","B","C"),n,replace=TRUE))
sf_dat<-sf::st_transform(sf::st_as_sf(data.frame(co,y=y),coords=c("lon","lat"),crs=4326),3857)
scheme<-suppressWarnings(suppressMessages(cv_spatial3(sf_dat,k=4,repeats=2,size=20000,selection="systematic",iteration=10,hexagon=FALSE,seed=1)))

ids<-rownames(co)
folds<-cvsp_folds_to_factors(scheme,ids,"spcv",existing="spcv_Rep1")
check("folds to columns: one factor column per repeat, unique names",
      identical(colnames(folds),c("spcv_Rep1_1","spcv_Rep2"))&&all(sapply(folds,is.factor))&&identical(rownames(folds),ids))
check("folds to columns: the fold of each observation",
      all(as.character(folds[[1]])==paste0("Fold",scheme[[1]]$folds_ids)))

# Factor-Attribute of all observations of X (some not used in training)
all_ids<-c(ids,"extra1","extra2")
fac<-data.frame(season=factor(rep(c("w","s"),length.out=length(all_ids))),row.names=all_ids)
fac2<-cvsp_add_fold_columns(fac,folds,all_ids)
check("fold columns added to the Factor-Attribute, NA for observations out of training",
      all(c("season",colnames(folds))%in%colnames(fac2))&&all(is.na(fac2["extra1",colnames(folds)]))&&
        identical(as.character(fac2[ids,"spcv_Rep2"]),as.character(folds$spcv_Rep2)))

# reading back gives the same caret folds as the original scheme
back<-cvsp_scheme_from_factors(fac2,colnames(folds),ids,y=y)
orig_caret<-repeated_blockcv_to_caret_from_list(scheme)
back_caret<-repeated_blockcv_to_caret_from_list(back)
check("folds read back: the same caret train/test indices as the scheme",
      identical(lapply(orig_caret$indexOut,sort),lapply(back_caret$indexOut,sort))&&identical(lapply(orig_caret$index,sort),lapply(back_caret$index,sort)))
check("folds read back: repeats, k and the source columns",
      identical(names(back),c("Rep1","Rep2"))&&back[[1]]$k==scheme[[1]]$k&&identical(attr(back,"source"),colnames(folds)))
check("folds read back: records per class (test counts add up to n)",
      all(grepl("^(train|test)_",colnames(back[[1]]$records)))&&sum(back[[1]]$records[grep("^test_",colnames(back[[1]]$records))])==n)

# another Y (numeric) for the same observations, and a subset of the observations (e.g. NA in the new Y)
yn<-rnorm(n)
back_n<-cvsp_scheme_from_factors(fac2,"spcv_Rep2",ids,y=yn,groups=4)
check("another (numeric) Y: records per quantile group",nrow(back_n$Rep1$records)==back_n$Rep1$k&&ncol(back_n$Rep1$records)==6)
sub<-ids[-(1:5)]
back_s<-cvsp_scheme_from_factors(fac2,"spcv_Rep1_1",sub,y=y[-(1:5)])
check("subset of the observations: folds keep the same membership",
      all(back_s$Rep1$folds_ids==as.integer(factor(as.character(fac2[sub,"spcv_Rep1_1"]),levels=back_s$Rep1$fold_names))))

# errors in plain words
msg<-function(expr) tryCatch({expr;""},error=function(e) conditionMessage(e))
check("training observation without a fold stops with a message",
      grepl("no fold for 2 training",msg(cvsp_scheme_from_factors(fac2,"spcv_Rep2",c(ids,"extra1","extra2")))))
check("a single fold stops with a message",grepl("at least 2 folds",msg(cvsp_scheme_from_factors(fac2,"season",ids[1:2*2]))))
check("missing column stops with a message",grepl("not found",msg(cvsp_scheme_from_factors(fac2,"nope",ids))))

# folds with numeric names are ordered as numbers (Fold2 before Fold10)
f10<-data.frame(f=factor(paste0("Fold",rep(c(1,2,10),length.out=n))),row.names=ids)
b10<-cvsp_scheme_from_factors(f10,"f",ids)
check("fold names ordered as numbers",identical(b10$Rep1$fold_names,c("Fold1","Fold2","Fold10")))

# the scheme read back can be plotted like the created one
pdf(NULL)
check("plots of folds read from the Factor-Attribute",
      inherits(cv_plot2(back,x=sf_dat,repeats="Rep1"),"ggplot")&&inherits(plot_test_fold_heatmap(back$Rep1$records),"ggplot"))
