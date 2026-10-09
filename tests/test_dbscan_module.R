# Density-based clustering module (server): training, views, saved models, sorting,
# saved clusters, SOM codebook, prediction
d<-araca_data()
envi<-d$envi
set.seed(1)
m<-kohonen::som(scale(as.matrix(envi)),grid=kohonen::somgrid(6,5,"hexagonal"))
names(m$unit.classif)<-rownames(envi)
attr(envi,"som")<-list(som1=m)
pals<-list(turbo=viridis::turbo,black=grDevices::colorRampPalette("black"),white=grDevices::colorRampPalette("white"))
vals<-shiny::reactiveValues(saved_data=list(envi_araca=envi,nema_hellinger=d$hel),cur_data="envi_araca",newcolhabs=pals,
                            colors_img=data.frame(val=names(pals),img=names(pals)))
unsaved<-"New model (unsaved)"
has_plot<-function(o) !is.null(o$src)
var1<-colnames(envi)[1]

shiny::testServer(dbscan_module$server,args=list(vals=vals),{
  session$setInputs(data_db="nema_hellinger",target="data",method="hdbscan",metric="euclidean",scale=FALSE,reduce=TRUE,axes_rule="var50",n_axes=NA,
                    palette="turbo",hulls=TRUE,pt_size=2,base_size=12,title="",border="white",som_points=TRUE,points_palette="black",points_factor="None",
                    som_pt_size=1,assign_noise=FALSE,guide="kdist",allow_single=FALSE,eps=0.5,selection="eom",eps_sel=0,som_weight=TRUE,
                    glosh_q=0.9,mark_glosh=FALSE,plot_type="clusters",sens_vary="par",sort_clusters=FALSE)
  check("NEMA Hellinger: 6 principal axes (50% rule)",prep()$reduced$k==6)
  session$setInputs(minPts=suggested_minpts(),mcs=suggested_mcs(),run=1); session$setInputs(dbs_model=unsaved)
  check("NEMA Hellinger: HDBSCAN finds 3 groups",length(unique(model()$cluster[model()$cluster>0]))==3)
  for(v in c("clusters","ctree","dendro")){ session$setInputs(plot_type=v); check(paste0("view '",v,"' is drawn"),has_plot(output$plot)) }
  session$setInputs(plot_type="clusters")
  session$setInputs(guide="ctree"); check("condensed tree in the parameter guide",has_plot(output$ctree_guide))
  session$setInputs(guide="sens"); x<-output$guide_ui
  session$setInputs(sens_vary="axes",run_sens=1); check("sensitivity over the number of axes",nrow(sens()$tab)==9)
  # DBSCAN with Suggest eps
  session$setInputs(method="dbscan",guide="kdist",suggest_eps=1)
  check("Suggest eps on NEMA: stable 3 groups",grepl("Stable: 3 clusters",eps_res()$note))
  session$setInputs(eps=signif(eps_res()$eps,3),run=2); session$setInputs(dbs_model=unsaved)
  check("DBSCAN with the suggested eps: 3 groups",length(unique(model()$cluster[model()$cluster>0]))==3)

  # Numeric-Attribute of envi: save, reload, delete models
  session$setInputs(data_db="envi_araca",method="hdbscan",reduce=FALSE,scale=TRUE)
  session$setInputs(minPts=4,mcs=8,run=3); session$setInputs(dbs_model=unsaved)
  session$setInputs(save_model=1); session$setInputs(model_name="HDB_envi",confirm_save_model=1)
  check("model saved in the Datalist",identical(names(attr(vals$saved_data$envi_araca,"dbscan")),"HDB_envi"))
  session$setInputs(dbs_model="HDB_envi")
  check("saved model reloaded",grepl("Saved model 'HDB_envi'",out_text(output$summary)))

  # sorted clusters and the saved-clusters check
  f0<-obs_clusters()
  session$setInputs(save_clusters=1); session$setInputs(factor_name="HDB_x",confirm_save=1)
  check("saved clusters recognised in the Factor-Attribute",identical(clusters_saved(),"HDB_x"))
  session$setInputs(sort_clusters=TRUE,sort_datalist="envi_araca",sort_var=var1)
  f1<-obs_clusters()
  mns<-tapply(envi[names(f1),var1],f1,mean)
  check("sorted clusters: increasing means",!is.unsorted(mns[names(mns)!="Noise"]))
  check("sorted clusters: same partition",max(apply(table(f0,f1),1,function(z) sum(z>0)))==1)
  check("relabelled clusters are no longer the saved column",length(clusters_saved())==0)
  session$setInputs(sort_clusters=FALSE)

  # prediction with the saved model
  session$setInputs(new_data="envi_araca",run_pred=1)
  check("prediction of the training data",length(pred()$f)==nrow(envi))
  session$setInputs(save_pred=1)
  check("saved predictions recognised",grepl("are saved in the Factor-Attribute",output$pred_note$html))

  # SOM codebook weighted by hits, with codebook principal axes
  session$setInputs(target="som",som_model="som1",reduce=TRUE,run=4); session$setInputs(dbs_model=unsaved)
  check("SOM: empty neurons left out",prep()$n<prep()$n_neurons)
  check("SOM: map drawn",has_plot(output$plot))
  check("SOM: dendrogram drawn",{ session$setInputs(plot_type="dendro"); has_plot(output$plot) })
  session$setInputs(plot_type="clusters")
  check("SOM: one cluster label per observation",length(obs_clusters())==nrow(envi))
  session$setInputs(save_model=2); session$setInputs(model_name="HDB_som",confirm_save_model=2)
  session$setInputs(dbs_model="HDB_envi",delete_model=1,confirm_delete_model=1)
  check("model deleted",identical(names(attr(vals$saved_data$envi_araca,"dbscan")),"HDB_som"))
})
