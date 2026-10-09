# Model registry and model functions (inst/www/funs_models.R), with a Datalist as older
# iMESc versions saved it: unsaved placeholders, a legacy type (svm), supervised entries
# wrapped as list(m=...), HC records and a density-based model of a SOM codebook
types<-imesc_model_types()
check("registry has every supervised model of SL_models.rds",all(SL_models$models%in%types$type))
check("registry keeps the legacy types svm and sgboost",all(c("svm","sgboost")%in%types$type))
check("imesc_models = registry types",identical(imesc_models,types$type))

som_a<-structure(list(codes=list(matrix(1:4,2))),class="kohonen")
dl<-data.frame(a=1:5,b=5:1)
attr(dl,"som")<-list("new som (unsaved)"=som_a,SOM1=som_a,SOM2=som_a)
attr(dl,"svm")<-list(old_svm=list(m=structure(list(method="svmRadial"),class="train")))
attr(dl,"rf")<-list("new model"=list(m=structure(list(method="rf"),class="train"),feature_rands=1),RF1=list(m=structure(list(method="rf"),class="train")))
attr(dl,"hc")<-list(HC3=structure(list(),hc.object=stats::hclust(dist(dl))))
attr(dl,"dbscan")<-list(HDB=structure(list(method="hdbscan",target="som",som_model="SOM1",cluster=1:2),class="imesc_dbscan"))

check("saved names leave out the unsaved placeholder",identical(imesc_model_names(dl,"som"),c("SOM1","SOM2")))
check("all names include the placeholder",length(imesc_model_names(dl,"som",saved=FALSE))==3)
check("legacy svm model read and unwrapped",inherits(imesc_model_get(dl,"svm","old_svm",unwrap=TRUE),"train"))
check("HC record (empty list with attributes) listed",identical(imesc_model_names(dl,"hc"),"HC3"))
check("model table lists the saved models of every type",
      all(c("SOM1","SOM2","old_svm","RF1","HC3","HDB")%in%imesc_model_table(dl)$name)&&!any(imesc_model_table(dl)$name%in%c("new som (unsaved)","new model")))

d2<-imesc_model_set(dl,"som","SOM1",structure(list(x=1),class="kohonen"))
check("replacing a model keeps its position",identical(names(attr(d2,"som")),c("new som (unsaved)","SOM1","SOM2"))&&identical(attr(d2,"som")$SOM1$x,1))
d2<-imesc_model_set(dl,"som","new som (unsaved)",som_a,first=TRUE)
check("unsaved model at the top",names(attr(d2,"som"))[1]=="new som (unsaved)"&&length(attr(d2,"som"))==3)
d2<-imesc_model_save_unsaved(dl,"rf","RF2",list(m=structure(list(method="rf"),class="train"),feature_rands=1))
check("saving the unsaved model removes the placeholder",identical(names(attr(d2,"rf")),c("RF1","RF2"))&&!is.null(attr(d2,"rf")$RF2$feature_rands))
d2<-imesc_model_delete(dl,"svm","old_svm")
check("deleting the last model removes the type",is.null(attr(d2,"svm")))
check("unique name",imesc_model_unique_name(dl,"som","SOM1")=="SOM1_1")
check("name issues: empty and duplicated",!is.null(imesc_model_name_issue(dl,"som",""))&&!is.null(imesc_model_name_issue(dl,"som","SOM2"))&&
        is.null(imesc_model_name_issue(dl,"som","SOM2",old="SOM2"))&&is.null(imesc_model_name_issue(dl,"som","SOM9")))

d2<-imesc_model_rename(dl,"som",c("new som (unsaved)","SOM_A","SOM2"))
check("renaming a SOM updates the density-based model that uses it",identical(attr(d2,"dbscan")$HDB$som_model,"SOM_A"))
check("renaming keeps the models",identical(names(attr(d2,"som")),c("new som (unsaved)","SOM_A","SOM2")))
check("duplicated names are refused",inherits(tryCatch(imesc_model_rename(dl,"som",c("x","SOM2","SOM2")),error=function(e) e),"error"))
check("empty names are refused",inherits(tryCatch(imesc_model_rename(dl,"som",c("x","","y")),error=function(e) e),"error"))

# Rename models tool (Pre-processing)
vals<-shiny::reactiveValues(saved_data=list(old=dl))
shiny::testServer(tool2_tab6$server,args=list(vals=vals),{
  session$setInputs(datalist="old")
  ml<-get_model_list()$old
  check("rename tool lists the saved models of the old Datalist (no placeholders)",nrow(ml)==sum(vapply(imesc_models,function(t) length(imesc_model_names(dl,t)),integer(1)))&&!any(ml$model_names%in%c("new som (unsaved)","new model")))
  new<-ml$model_names
  new[new=="SOM1"]<-"SOM_renamed"
  args<-stats::setNames(as.list(new),paste0("newname_datalist_",seq_along(new)))
  do.call(session$setInputs,args)
  session$setInputs(run_rename=1)
  check("rename tool renames the SOM and updates the density-based model",
        "SOM_renamed"%in%names(attr(vals$saved_data$old,"som"))&&identical(attr(vals$saved_data$old,"dbscan")$HDB$som_model,"SOM_renamed"))
  dup<-new; dup[dup=="SOM2"]<-"SOM_renamed"
  do.call(session$setInputs,stats::setNames(as.list(dup),paste0("newname_datalist_",seq_along(dup))))
  session$setInputs(run_rename=2)
  check("rename tool refuses duplicated names (nothing renamed)","SOM2"%in%names(attr(vals$saved_data$old,"som")))
})

# Datalist overview of the old Datalist: unsaved placeholders are not counted
ov<-paste(as.character(datalist_overview(dl,available_models=SL_models$models)),collapse=" ")
check("overview counts the saved SOMs only",grepl("som \\(models:2\\)",ov))
check("overview lists the legacy svm model",grepl("train",ov))

# K-means module: unsaved model, save (create and replace), delete
d<-araca_data()
vals<-shiny::reactiveValues(saved_data=list(envi=d$envi),cur_data="envi",newcolhabs=list(turbo=viridis::turbo))
shiny::testServer(k_means_module$server,args=list(vals=vals),{
  session$setInputs(data_kmeans="envi",model_or_data="data",km_centers=3,km_itermax=10,km_nstart=1,kmeans_seed=1,km_alg="Hartigan-Wong")
  session$setInputs(kmeans_run=1)
  check("K-means: unsaved model in the Datalist",identical(names(attr(vals$saved_data$envi,"kmeans")),"new kmeans (unsaved)"))
  session$setInputs(save_kmeans_models=1)
  session$setInputs(`model_store-name`="KM3",`model_store-confirm_save`=1)
  check("K-means: saved under its name, placeholder removed",identical(names(attr(vals$saved_data$envi,"kmeans")),"KM3")&&inherits(attr(vals$saved_data$envi,"kmeans")$KM3,"ikmeans"))
  session$setInputs(km_centers=4,kmeans_run=2)
  session$setInputs(save_kmeans_models=2)
  session$setInputs(`model_store-name`="KM3",`model_store-confirm_save`=2)
  check("K-means: duplicated name refused by the save window",nrow(attr(vals$saved_data$envi,"kmeans")$KM3$centers)==3)
  session$setInputs(`model_store-mode`="replace",`model_store-replace`="KM3",`model_store-confirm_save`=3)
  check("K-means: replaced in place",identical(names(attr(vals$saved_data$envi,"kmeans")),"KM3")&&nrow(attr(vals$saved_data$envi,"kmeans")$KM3$centers)==4)
  session$setInputs(kmeans_models="KM3",trash_kmeans=1)
  session$setInputs(`model_store-delete_pick`="KM3",`model_store-confirm_delete`=1)
  check("K-means: deleted (type removed when empty)",is.null(attr(vals$saved_data$envi,"kmeans")))
})

# SOM module: save the unsaved SOM (create and replace) and delete models
d<-araca_data()
set.seed(1)
m<-kohonen::som(scale(as.matrix(d$envi)),grid=kohonen::somgrid(4,4,"hexagonal"))
names(m$data)<-"X"; attr(m,"Datalist")<-"envi"
envi<-d$envi
attr(envi,"som")<-list("new som (unsaved)"=m,SOM_old=m)
vals<-shiny::reactiveValues(saved_data=list(envi=envi),cur_data="envi",cursomtab="som_tab2",som_res="train_tab1",newcolhabs=list(turbo=viridis::turbo))
shiny::testServer(imesc_supersom$server,args=list(vals=vals),{
  session$setInputs(data_som="envi",som_models="new som (unsaved)",som_tab="som_tab2")
  session$setInputs(tools_savesom=1)
  session$setInputs(`model_store-mode`="create",`model_store-name`="SOM_new",`model_store-confirm_save`=1)
  check("SOM: saved under its name, placeholder removed",identical(names(attr(vals$saved_data$envi,"som")),c("SOM_old","SOM_new")))
  attr(vals$saved_data$envi,"som")<-c(list("new som (unsaved)"=m),attr(vals$saved_data$envi,"som"))
  session$setInputs(som_models="new som (unsaved)")
  session$setInputs(tools_savesom=2)
  session$setInputs(`model_store-mode`="replace",`model_store-replace`="SOM_old",`model_store-confirm_save`=2)
  check("SOM: replaced in place",identical(names(attr(vals$saved_data$envi,"som")),c("SOM_old","SOM_new")))
  session$setInputs(som_model_delete=1)
  session$setInputs(`model_store-delete_pick`=c("SOM_old","SOM_new"),`model_store-confirm_delete`=1)
  check("SOM: deleted (type removed when empty)",is.null(attr(vals$saved_data$envi,"som")))
})

# Supervised models (setup module): save the unsaved model (create, replace) and delete
d<-araca_data()
fake<-function(id) structure(list(method="rf",id=id),class="train")
envi<-d$envi
attr(envi,"rf")<-list(RF_old=list(m=fake(0)),"new model"=list(m=fake(1),permimp_metrics="computed"))
vals<-shiny::reactiveValues(saved_data=list(envi=envi),cur_data="envi",cmodel="rf",cur_caret_model=fake(1),
                            trainSL_args=list(var_y="y",data_x="envi"))
shiny::testServer(sl_model_setup$server,args=list(vals=vals),{
  session$setInputs(data_x="envi")
  vals$cmodel<-"rf"   # set by the model choice in the app (cleared when the Datalist changes)
  session$setInputs(save_model=1)
  session$setInputs(`model_store-mode`="create",`model_store-name`="RF_new",`model_store-confirm_save`=1)
  rf<-attr(vals$saved_data$envi,"rf")
  check("SL: saved under its name, placeholder removed",identical(names(rf),c("RF_old","RF_new")))
  check("SL: results computed for the unsaved model are kept",identical(rf$RF_new$permimp_metrics,"computed")&&identical(attr(rf$RF_new$m,"model_name"),"RF_new"))
  attr(vals$saved_data$envi,"rf")[["new model"]]<-list(m=fake(2))
  vals$cur_caret_model<-fake(2)
  session$setInputs(save_model=2)
  session$setInputs(`model_store-mode`="replace",`model_store-replace`="RF_old",`model_store-confirm_save`=2)
  rf<-attr(vals$saved_data$envi,"rf")
  check("SL: replaced in place",identical(names(rf),c("RF_old","RF_new"))&&rf$RF_old$m$id==2)
  session$setInputs(trash_model=1)
  session$setInputs(`model_store-delete_pick`=c("RF_old","RF_new"),`model_store-confirm_delete`=1)
  check("SL: deleted (type removed when empty)",is.null(attr(vals$saved_data$envi,"rf")))
})
