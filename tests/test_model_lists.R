# Models stored in a Datalist and the lists that show them (Datalist manager, rename tool,
# Datalist overview): SOM-like, K-means, HC and density-based models
dl<-data.frame(a=rnorm(20),b=rnorm(20))
attr(dl,"hc")<-list(HC3_numeric=structure(list(),hc.object=stats::hclust(dist(dl)),params=list(target="data")))
attr(dl,"kmeans")<-list(Kmeans=structure(list(cluster=data.frame(cluster=1:20)),class="ikmeans"))
attr(dl,"dbscan")<-list(HDB=structure(list(method="hdbscan",cluster=rep(1,20)),class="imesc_dbscan"))
vals<-list(saved_data=list(dl=dl))

lm_<-list_models(dl,imesc_models=imesc_models)
check("list_models lists hc, kmeans and dbscan",all(c("hc","kmeans","dbscan")%in%names(lm_))&&isTRUE(lm_$hc$HC3_numeric))
tr<-getTree_saved_data(vals,FALSE,imesc_attrs=imesc_attrs,imesc_models=c("pwRDA","som","kmeans","hc","dbscan"))
nm<-names(unlist(tr))
check("Datalist manager tree shows the three models",all(c("dl.hc.HC3_numeric","dl.kmeans.Kmeans","dl.dbscan.HDB")%in%nm))
ov<-paste(as.character(datalist_overview(dl,available_models=c("rf"))),collapse=" ")
check("Datalist overview labels kmeans, hclust and density-based models",grepl("kmeans \\(models:1\\)",ov)&&grepl("hclust \\(models:1\\)",ov)&&grepl("density-based-hdbscan",ov))
for(a in c("hc","kmeans","dbscan")) check(paste0("get_attr_imesc reads the ",a," model"),get_attr_imesc("dl",a,names(attr(dl,a))[1],vals=vals)[["attr"]]==a)
