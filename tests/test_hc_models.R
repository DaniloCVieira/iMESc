# HC module: save (with the clusters in the Factor-Attribute), rename and delete through the
# shared windows, for HC of the Numeric-Attribute (models in the Datalist) and of a SOM
# codebook (models in the SOM)
d<-araca_data()
envi<-d$envi
set.seed(1)
m<-kohonen::som(scale(as.matrix(envi)),grid=kohonen::somgrid(5,4,"hexagonal"))
names(m$unit.classif)<-rownames(envi)
attr(envi,"som")<-list(som1=m)
vals<-shiny::reactiveValues(saved_data=list(envi=envi),cur_data="envi",newcolhabs=list(turbo=viridis::turbo))
st<-function(...) session$setInputs(...)
shiny::testServer(hc_module$server,args=list(vals=vals),{
  # HC of the Numeric-Attribute
  session$setInputs(data_hc="envi",model_or_data="data",method.hc0="ward.D2",customKdata=3,hc_fun="hclust",disthc="euclidean",hc_sort=FALSE)
  session$setInputs(run_hc=1)
  check("HC: unsaved model after Run",!is.null(phc()))
  session$setInputs(tools_savehc_model=1)
  session$setInputs(`model_store-name`="HC3",`model_store-save_factor`=TRUE,`model_store-factor_name`="HC3_col",`model_store-confirm_save`=1)
  hc<-attr(vals$saved_data$envi,"hc")
  check("HC: saved in the Datalist with its tree",identical(names(hc),"HC3")&&!is.null(attr(hc$HC3,"hc.object")))
  check("HC: clusters saved in the Factor-Attribute",isTRUE("HC3_col"%in%colnames(attr(vals$saved_data$envi,"factors"))))
  check("HC: unsaved model cleared",is.null(phc()))
  # a second model, then rename (a duplicated name is refused) and delete
  session$setInputs(customKdata=4,run_hc=2)
  session$setInputs(tools_savehc_model=2)
  session$setInputs(`model_store-name`="HC4",`model_store-confirm_save`=2)
  session$setInputs(tools_edithc_model=1)
  session$setInputs(`model_store-rename_from`="HC3",`model_store-rename_to`="HC4",`model_store-confirm_rename`=1)
  check("HC: duplicated name refused",identical(names(attr(vals$saved_data$envi,"hc")),c("HC3","HC4")))
  session$setInputs(`model_store-rename_to`="HC_three",`model_store-confirm_rename`=2)
  check("HC: renamed",identical(names(attr(vals$saved_data$envi,"hc")),c("HC_three","HC4")))
  session$setInputs(tools_edithc_model=2)
  session$setInputs(`model_store-delete_pick`=c("HC_three","HC4"),`model_store-confirm_delete`=1)
  # (the HC module keeps an empty list of HC models in the Datalist)
  check("HC: deleted",!length(attr(vals$saved_data$envi,"hc")))

  # HC of the SOM codebook: stored in the SOM model
  session$setInputs(model_or_data="som codebook",som_model_name="som1",customKdata=3)
  session$setInputs(run_hc=3)
  session$setInputs(tools_savehc_model=3)
  session$setInputs(`model_store-name`="HC_som",`model_store-save_factor`=FALSE,`model_store-confirm_save`=3)
  sm<-attr(vals$saved_data$envi,"som")$som1
  check("HC of the SOM codebook: saved in the SOM model",identical(names(attr(sm,"hc")),"HC_som")&&!length(attr(vals$saved_data$envi,"hc")))
  session$setInputs(tools_edithc_model=3)
  session$setInputs(`model_store-delete_pick`="HC_som",`model_store-confirm_delete`=2)
  check("HC of the SOM codebook: deleted",!length(attr(attr(vals$saved_data$envi,"som")$som1,"hc")))
})
