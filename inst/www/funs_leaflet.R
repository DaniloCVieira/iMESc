#' @export
mode_function <- function(x, na.rm = F) {
  levels_data <- levels(x)
  level_codes <- 1:length(levels_data)
  names(level_codes) <- levels_data
  if(isTRUE(na.rm)){
    x<-na.omit(x)
  }
  x <- level_codes[as.character(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#' @export
rst0<-function(data){
  coords<-attr(data,"coords")
  ymin=length(unique(coords[,2]))
  xmin=length(unique(coords[,1]))
  ncol=round(sqrt(xmin))
  nrow=round(sqrt(ymin))
  prop<-ncol/nrow
  ncol*nrow
  len<-length(data[,1])
  x<-round(sqrt(len)*prop)
  y<-round(len/round(sqrt(len)*prop))
  # set up an 'empty' raster, here via an extent object derived from your data
  e <- extent(to_spatial(coords))
  my_rst <- raster(e, ncol=x, nrow=y)
  my_rst
}
#' @export
rst_num<-function(my_rst,data){
  coords<-attr(data,"coords")
  my_rst <- rasterize(coords, my_rst, data[,1], fun=mean)
  my_rst<-raster::ratify(my_rst)
  my_rst
}
#' @export
rst_factor<-function(my_rst,data){

  levels_data <- levels(data[,1])

  coords<-attr(data,"coords")
  my_rst <- rasterize(coords, my_rst, data[,1], fun=mode_function)

  llevs<-levels(data[,1])[as.numeric(  levels(factor(values(my_rst))))]

  values(my_rst)<-factor(values(my_rst), labels=llevs)

  my_rst
}
#' @export

map_rst<-function(data,cut_shape=T, crs="+proj=longlat +datum=WGS84 +no_defs"){
  coords<-attr(data,"coords")
  my_rst<-rst0(data)
  if(is.factor(data[,1])){
    my_rst<-rst_factor(my_rst,data)
  } else{
    my_rst<-rst_num(my_rst,data)
  }

  base_shape0<-st_as_sf(x =  coords,coords = c(colnames( coords)))
  base_shape0<-st_set_crs(base_shape0,crs)
  if(isTRUE(cut_shape)){
    base_shape<-attr(data,'base_shape')
    if(!is.null(base_shape)) {
      my_rst<-raster::mask( raster::crop(my_rst, raster::extent(base_shape)), base_shape)}
  }

  crs(my_rst) <- CRS(crs)
 # my_rst<-  raster::ratify(my_rst)
  my_rst
}
#' @export
get_chart_data <- function(data,factor_chart=2, distance = 50, fun="sum") {
  coords<-attr(data,"coords")
  colnames(coords)<-c("x","y")
  factors<-attr(data,"factors")[factor_chart]
  distance_km<-distance*1000
  di<-geodist(coords,measure="geodesic")
  coords<-do.call(rbind,lapply(1:nrow(di), function(i){
    pic<-unique(which(di[i,]<distance_km),
                which(di[,i]<distance_km))
    apply(coords[pic,],2,mean)
  }))
  df<-data.frame(coords,fac=factors[,1],z=data[,1])
  if(is.factor(df$z)){
    df$factor_new<- apply(df[-c(1:2)],1,function(x) paste(x, collapse = "_split_"))
  } else{
    df$factor_new<- df$fac
  }
  dfk<-cbind(df[,1:2],kohonen::classvec2classmat( df$factor_new))
  if(!is.factor(df$z)){
    dfk<-fill_z_chart(dfk,df)
  }
  if(fun=="sum"){
    resultado <- dfk  |>
      group_by(x, y) |>
      summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop')
  } else if(fun=="mean"){
    resultado <- dfk |>
      group_by(x, y) |>
      summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')}


  #colnames(resultado)<-gsub("_split_",".",colnames(resultado))
  return(resultado)

}
#' @export
get_df_chart2<-function(resultado,result="combined"){
  class_factor<-gsub("_split_*.","",colnames(resultado))[-c(1:2)]
  z_factor<-gsub(".*_split_","",colnames(resultado))[-c(1:2)]
  resultado_new<-data.frame(resultado[-c(1:2)])
  for(i in 1:nrow(resultado_new)){
    vec<-unlist(resultado_new[i,])

    vals<-z_factor[vec>0]
    cols_index<-which(vec>0)
    newvec<-if(result=="combined"){
      paste(class_factor[vec>0],vals, sep=":")
    } else{
      vals
    }
    resultado_new[i,cols_index]<-    newvec
  }
  colnames(resultado_new)<-gsub("_split_",".",colnames(resultado_new))
  resultado_new

}
#' @export
fill_z_chart<-function(dfk,df){
  dfk2<-dfk[-c(1:2)]
  i=1
  for(i in 1:nrow(dfk2)){
    pic<-which(dfk2[i,]>0)
    dfk2[i,pic]<-df$z[i]

  }
  cbind(dfk[c(1:2)],dfk2)
}
#' @export
get_factor_names<-function(df){
  class_factor<-gsub("_split_*.","",colnames(df))[-c(1:2)]
  z_factor<-gsub(".*_split_","",colnames(df))[-c(1:2)]
  list(class_factor=class_factor,z_factor=z_factor)
}
#' @export
get_cols_factor_chart<-function(df,data, pal){
  names_factors<-get_factor_names(df)
  class_factor<-names_factors$class_factor
  z_factor<-names_factors$z_factor

  min_opacity_factor<-0.6
  alphas<-seq(1,min_opacity_factor,len=length(unique(class_factor)))[as.factor(class_factor)]
  cols<-colorRampPalette(pal)(nlevels(data[,1]))
  cols<-cols[as.factor(z_factor)]
  cols<-sapply(seq_along(cols),function(i) adjustcolor(cols[i],alphas[i]))
  cols
}
#' @export
get_popup_chart<-function(df){
  facnames<-get_factor_names(df)
  aa<-get_df_chart2(df, "not")
  aa<-data.frame(t(aa))
  do.call(rbind,lapply(aa,function(x){
    pic<-x!=0
    xpic<-x[pic]
    clapic<-facnames[[1]][pic]
    sapply(unique(clapic), function(x){
      paste(xpic[clapic==x],collapse  =";")
    })
  }))
}
#' @export
add_pie_chart<-function(map,data,factor_chart,buffer_zize,fun, min_radius,max_radius, pal){
  max_radius<-max_radius*2
  min_radius=min_radius*2
  df0<-get_chart_data(data,factor_chart, distance=buffer_zize, fun=fun)
  df<-data.frame(df0)
  colnames(df)<-colnames(df0)

  popup<-if(is.factor(data[,1])){
    popupArgs(showTitle =F,showValues =F,supValues =get_popup_chart(df))
  } else{
    popupArgs()
  }
  facddf<-df[-c(1:2)]
  fac<-attr(data,"factors")[factor_chart]
  colors <- colorRampPalette(pal)(ncol(facddf))
  if(is.factor(data[,1])){
    colors=   get_cols_factor_chart(df,data, pal)
    width=max_radius
  } else{
    width = scales::rescale(rowSums(facddf),c(min_radius,max_radius))
  }
  colnames(facddf)<-gsub("_split_",".",colnames(facddf))
  map |> addMinicharts(
    lng = df[,1],
    lat = df[,2] ,
    type="pie",
    chartdata = facddf,
    #layerId=fac[,1],
    colorPalette =colors,
    popup = popup,
    legend =T,
    width=width
  )

}
#' @export
add_base_shape<-function(map,data,shape_attr="base_shape",color="red",fillOpacity =1,stroke =T,weight =1, shape=T, fill=T){
  if(isTRUE(shape)){
    if(!is.null(attr(data,shape_attr))){
      base_shape<-st_as_sf(attr(data,shape_attr))[,"geometry"]
      if(!is.null(base_shape)){
        for(i in 1:nrow(base_shape)){
          geom<-sf::st_geometry_type(base_shape[i,])
          base_temp<-st_cast(base_shape[i,],"POLYGON")
          for(j in 1:nrow(base_temp)){
            co<-st_coordinates(base_temp[j,])[,1:2]
            map<-map |> addPolygons(co[,1],co[,2],color =color ,fill=fill,opacity=1,fillOpacity =fillOpacity,stroke=stroke,weight =weight,group="base_shape")
          }
        }


      }

    }}
  map
}
#' @export
add_shapes1<-function(map,data,layer_shape_args,base_shape_args){

  if(isTRUE(base_shape_args$shape)){
    base_shape_args$map<-map
    base_shape_args$data<-data
    base_shape_args$shape_attr<-"base_shape"

    map<-do.call(add_base_shape,base_shape_args)

  }
  if(isTRUE(layer_shape_args$shape)){
    layer_shape_args$shape_attr<-"layer_shape"
    layer_shape_args$map<-map
    layer_shape_args$data<-data
    map<-do.call(add_base_shape,layer_shape_args)
  }

  map

}
#' @export
control_layers<-function(addCircles,addMinicharts,rst,base_shape_args,layer_shape_args,args_labels=NULL){
  layers_data<-c()
  if(isTRUE(addCircles)){
    layers_data[length(layers_data)+1]<-"circles"
  }

  if(!is.null(rst)){
    layers_data[length(layers_data)+1]<-"raster"
  }
  if(!is.null(args_labels)){
    if(isTRUE(args_labels$show_labels)){
      layers_data[length(layers_data)+1]<-"labels"
    }

  }

  layers_shape<-c()
  if(is.null(rst)){
    if(isTRUE(base_shape_args$shape)){
      layers_shape[length(layers_shape)+1]<-"base_shape"
    }
    if(isTRUE(layer_shape_args$shape)){
      layers_shape[length(layers_shape)+1]<-"layer_shape"
    }
  }

  c(layers_shape,layers_data)

}
#' @export
add_extraLL<-function(map,data,args_extra_shape){
  if(is.null(args_extra_shape)){
    return(map)
  }
  map2<-map
  ea<-data.frame(args_extra_shape)
  ea[,1]<-as.logical(ea[,1])
  ea[,3]<-as.numeric(ea[,3])
  ea[,5]<-as.numeric(ea[,5])

  for(k in 1:nrow(args_extra_shape)){
    e<-attr(data,"extra_shape")[[k]]
    ei<-ea[k,]
    if(!is.null(e)){
      if(ei$layers){
        extra<-st_as_sf(e)[,"geometry"]
        if(!is.null(extra)){
          for(i in 1:nrow(extra)){
            geom<-sf::st_geometry_type(extra[i,])
            base_temp<-st_cast(extra[i,],"LINESTRING")
            for(j in 1:nrow(base_temp)){
              co<-st_coordinates(base_temp[j,])[,1:2]
              map2<-map2 |> addPolylines(co[,1],co[,2],color =ei$colors ,opacity=1,weight =ei$sizes,group="extra_shape")
            }
          }
        }
      }
    }
  }
  return(map2)
}
#' @export

lab_nmax<-span("nmax:", tiphelp("for local kriging, the number of nearest observations that should be used for a kriging prediction or simulation, where nearest is defined in terms of the space of the spatial locations. By default (empty), all observations are used"))
lab_nmin<-span( "nmin:",tiphelp("for local kriging, if the number of nearest observations within distance maxdist is less than nmin, a missing value will be generated- see maxdist"))
lab_omax<-span("omax:",tiphelp("maximum number of observations to select per octant (3D) or quadrant (2D); only relevant if maxdist has been defined as well"))
lab_maxdist<-span("maxdist:",tiphelp("for local kriging, only observations within a distance of maxdist from the prediction location are used for prediction; if combined with nmax, both criteria apply"))
lab_idp<-span("idp:",tiphelp("numeric; specify the inverse distance weighting power"))
lab_k<-span("k:",tiphelp("number of neighbours considered."))
lab_resolution<-span("resolution:",tiphelp("Interpolation resolution"))
#' @export
get_idw_leaf<-function(data, cut_shape=T, resolution=3000,crs.info="+proj=longlat +datum=WGS84 +no_defs",...){
  base_shape<-attr(data,"base_shape")
  layer_shape<-attr(data,"layer_shape")


  coords<-attr(data,"coords")
  data<-na.omit(data)
  coords<-coords[rownames(data),]
colnames(coords)<-c("x","y")
limits<-get_limits(limits=NULL,base_shape,layer_shape,coords)

  newgrid<-get_grid(coords,limits,crs.info,resolution)
  data_spat<-to_spatial(data.frame(cbind(coords,z=data[,1])))
  interp_model=df_idw = gstat::idw(z~1, data_spat, newgrid)
  df_raster <- raster::raster(df_idw)
  if(isTRUE(cut_shape)){
    if(!is.null(base_shape)){
      df_raster<-  raster::mask(df_raster, base_shape)
    }
  }
  df_raster<-  raster::ratify(df_raster)
  return(df_raster)
}
#' @export
get_knn_leaf<-function(data,k=5,cut_shape=T,resolution){
  coords<-attr(data,"coords")
  base_shape<-attr(data,"base_shape")
  layer_shape<-attr(data,"layer_shape")
  limits<-get_limits(limits=NULL,base_shape,layer_shape,coords)
  coords<-attr(data,"coords")
  data<-na.omit(data)
  coords<-coords[rownames(data),]


  newgrid<-get_grid(coords,limits,crs.info,resolution)
  z=data[,1]
  interp_model<-predy<-class::knn(coords,
                           coordinates(newgrid),
                           z,use.all=T, k=k,l=0)

  knn_raster <- raster::raster(SpatialGridDataFrame(newgrid,data.frame(predy)))
  if(isTRUE(cut_shape)){
    if(!is.null(base_shape)){
      knn_raster<-  raster::mask(knn_raster, base_shape)
    }
  }
  knn_raster<-  raster::ratify(knn_raster)
  return(knn_raster)

}
#' @export
interp_leaflet<-function(data, cut_shape=T, resolution=3000,crs.info="+proj=longlat +datum=WGS84 +no_defs",k=5,...){
  if(is.factor(data[,1])){
    get_knn_leaf(data,k,cut_shape,resolution)
  } else{
    get_idw_leaf(data, cut_shape, resolution,crs.info="+proj=longlat +datum=WGS84 +no_defs",...)

  }
}
#' @export
migrate_rst<-function(rst,data){
  attr(rst,"coords")<-attr(data,"coords")
  attr(rst,"factors")<-attr(data,"factors")
  attr(rst,"base_shape")<-attr(data,"base_shape")
  attr(rst,"layer_shape")<-attr(data,"layer_shape")
  attr(rst,"extra_shape")<-attr(data,"extra_shape")
  attr(rst,'z_name')<-colnames(data[1])
  rst
}
#' @export
get_raster_template<-function(kc,resolution, sample=F){
  cd<-coordinates(kc)


  res1<-dist(sort( cd[,1])[1:2])
  res2<-dist(abs(sort( cd[,2])[1:2]))
  resol<-round(sqrt(resolution/2))
  resol<-c(resol[1],resol[2])


  example_raster <- raster(crs = crs(kc), vals = 0, nrows=resol[1],ncols=resol[2], ext = extent(kc))
  example_raster
}
#' @export
variogram_parameters<-function(df,model="Gau",var=1, log=F, g=NULL){

  log_params=NULL
  if(isTRUE(log)){
    log_var<-decostand(df[,var],"log")
    log_params<-attr(log_var,"parameters")
    df[,var]<-log_var
  }
  coords<-attr(df,"coords")[rownames(df),]
  colnames(coords)<-c("x","y")
  z<-df[var]
  if(!is.null(g)){
    if(length(g$data)>0){
      newnames<-make.unique(c(names(g$data),var))
      var<-newnames[length(newnames)]
      colnames(z)<-var
    }
  }
  dd<-cbind(coords,z)
  formula<-as.formula(paste(var,"~ 1"))
  dsp<-to_spatial(dd)



  if(nrow(zerodist(dsp))>0){
    dsp <- dsp[-zerodist(dsp)[,1],]
  }
  TheVariogram=NULL


  auto_z1<-automap::autofitVariogram(formula, dsp,model=model)
  vgm_z1<-auto_z1[[1]]
  model_z1<-auto_z1[[2]]
  return(list(variogram=vgm_z1,auto_z1=auto_z1, formula=formula,dsp=dsp, id=var, log=log,log_params=log_params))
}
#' @export
predict_cokrige<-function(g,newdata=NULL, crs.info="+proj=longlat +datum=WGS84 +no_defs"){


  coords<-attr(g,"coords")
  base_shape<-attr(g,"base_shape")
  layer_shape<-attr(g,"layer_shape")
  limits<-get_limits(limits=NULL,base_shape,layer_shape,coords)
  if(is.null(newdata)){
      #newgrid<-get_grid(coords,limits,crs.info,res=resolution)
    newgrid<-data.frame(coords,lapply(g$data,function(x) data.frame(x$data)[3]))
  } else{
    newgrid<-newdata
  }

  grd<-to_spatial(data.frame(newgrid),crs.info=crs.info)
  k.c <- predict(g, grd)
  #pred1<-predict(g, newdata =newgrid )
  result<-data.frame(k.c)
  result["optional"]<-NULL
  islog=attr(g,"log")

  if(!is.null(islog)){
    names(islog)<-paste0(names(g$data),".pred")
    for(i in 1:length(islog)){
      if(!is.null(islog[[i]])){
        pred<-result[,names(islog)[i]]
        parameters<-islog[[i]]
        result[,names(islog)[i]]<-(2^pred) * parameters$minpos
      }
    }
  }
  atts<-attributes(result)
  result<-predict_cokrige_factors(result,g)

  attr(result,"k.c")<-k.c
  attr(result,"coords")<-coords
  attr(result,"base_shape")<-base_shape
  attr(result,"layer_shape")<-layer_shape
  result
}
#' @export
predict_cokrige_factors<-function(pred,g){
  isfac<-attr(g,"is_factor")
  if(!any(isfac)){
    return(pred)
  }
  factor_column<-attr(g,"factor_column")
  factor_level<-attr(g,"factor_level")
  preds0<-preds<-pred[grep("\\.pred",colnames(pred))]
  colnames(preds0)<-gsub("gstat_level_","",colnames(preds0))
  predfac_results<-list()
  for(var in unique(factor_column)){
    preds<-preds0
    pic_var<-which(factor_column==var)
    preds<-preds[pic_var]
    isfac2<-isfac[pic_var]
    if(all(isfac2)){
      pred_fac<-preds[which(isfac2)]
      pred_tab<-decostand(pred_fac,"range")
      probs<-pred_tab
      colnames(probs)<-colnames(pred_fac)
      predictions<-kohonen::classmat2classvec(probs)
      df_pred<-data.frame(pred=factor(predictions, labels=unique(factor_level[pic_var])))
      colnames(df_pred)<-var
      predfac_results[[var]]<-df_pred
    }
  }

  pred_class<-data.frame(do.call(cbind,predfac_results))
  colnames(pred_class)<-paste0(names(predfac_results),".pred")
  rownames(pred_class)<-rownames(pred)
  pred_result<-cbind(pred,pred_class)
  return(pred_result)

}
#' @export
df_gpred<-function(pred, var){
  kc<-attr(pred,"k.c")
  coords<-coordinates(kc)
  z<-pred[var]
  attr(z,"coords")<-coords
 z
}
#' @export
raster_gpred<-function(pred, var, resolution,cut_shape,sample){
  kc<-attr(pred,"k.c")
  raster_empty<-get_raster_template(kc,resolution,sample)
  coords<-coordinates(kc)
  z<-pred[var]
  attr(z,"coords")<-coords
  rz<-rst_num(raster_empty,z)
  base_shape<-attr(pred,"base_shape")
  rz<-cut_shape_fun(rz,cut_shape,base_shape)
  rz
}
#' @export
cut_shape_fun<-function(my_rst,cut_shape=F,base_shape){
  if(isTRUE(cut_shape)){
    if(!is.null(base_shape)) {
      my_rst<-raster::mask(my_rst, base_shape)}
  }
  my_rst
}
#' @export
gcv_df<-function(out){
  obs<-attr(out,"obs")
  result<-list()

  for(i in 1:ncol(obs)){

  }
  ME<-mean(out$residual)
  MSE<-mean(out$residual^2)
  MSNE<-mean(out$zscore^2)
  COP<-cor(out$observed, out$observed - out$residual)
  CPR<- cor(out$observed - out$residual, out$residual)
  TSS = sum((out$observed - mean(out$observed))^2)
  RSS = sum(out$residual^2)
  R2 = 1 - (RSS/TSS)
  df<-data.frame(ME,MSE,MSNE,COP,CPR,R2)
  df<-round(df,5)
  df
}
#' @export
gcv_df<-function(out, round=5){
  obs<-attr(out,"obs")
  result<-list()
  for(i in 1:ncol(obs)){
    residual<-out[,i]
    observed<-obs[,i]
    ME<-mean(residual)
    MSE<-mean(residual^2)
    #MSNE<-mean(out$zscore^2)
    COP<-cor(observed, observed - residual)
    CPR<- cor(observed - residual, residual)
    TSS = sum((observed - mean(observed))^2)
    RSS = sum(residual^2)
    R2 = 1 - (RSS/TSS)
    df<-data.frame(ME,MSE,COP,CPR,R2)
    df<-round(df,round)
    result[[i]] <-df
  }
  names(result)<-colnames(obs)
  result<-do.call(rbind,result)
  result
}
content = paste0("<p>Waiting time between ",
                 "eruptions and the duration of the eruption for the Old Faithful geyser ",
                 "in Yellowstone National Park, Wyoming, USA.</p><p>Azzalini, A. and ",
                 "Bowman, A. W. (1990). A look at some data on the Old Faithful geyser. ",
                 "Applied Statistics 39, 357-365.</p>")
#' @export
gcv_summary<-function(out){
 df<-gcv_df(out)
  labels<-paste0(
    p("ME: mean error, ideally 0"),
    p("ME: Mean square error, ideally small"),
    #p("MSNE:: Mean square normalized error, ideally close to 1"),
    p("COP: correlation observed and predicted, ideally 1"),
    p("CPR:correlation predicted and residual, ideally 0"),
    p("R²:R-squared")
  )
  labels
}
#' @export
help_autofit<-function(){
  popify(a(icon("fas fa-question-circle",style="color:  #3c8dbc")),"autofitVariogram {automap}",
         paste0(
           p(
             HTML(paste0("iMESc uses the function ",code('autofitVariogram')," from ",code('automap')," package to automatically fit a experimental variogram."))
           ),
           p("The initial sill is estimated as the mean of the max and the median of the semi-variance. The inital range is defined as 0.10 times the diagonal of the bounding box of the data. The initial nugget is defined as the min of the the semi-variance"),
           p("Users can also adjust the parameters manually.")
         ),"right"

  )
}
#' @export
to_ordinal <- function(vec) {
  # Função auxiliar para determinar o sufixo correto
  get_suffix <- function(number) {
    if (number %% 100 %in% c(11, 12, 13)) {
      return("th")
    }
    switch(as.character(number %% 10),
           `1` = "st",
           `2` = "nd",
           `3` = "rd",
           "th")
  }

  # Aplicar a função auxiliar a cada elemento do vetor
  sapply(vec, function(x) paste0(x, get_suffix(x)))
}
pie_chart_help <- div(
  p("This function utilizes the ", code('geodist'), " package to calculate geodesic distances between each pair of points in your dataset."),
  p("Using the ", strong("buffer (km)"), " parameter, the function defines a circular buffer zone around each point with a radius equal to the specified distance. Points within this buffer are identified and considered for analysis, based on their proximity to each other."),
  p("The function then uses a selected ",strong("factor")," to categorize and aggregate data within each buffer zone. This aggregation is visually represented through pie charts on the map, where each pie chart corresponds to a buffer zone and displays the summarized data by factor levels."),
  p("Essentially, the resulting map showcases spatial pie charts, with each chart summarizing data within its respective buffer zone.")
)


map_discrete<-function(data, pal=viridis(100),nbreaks=5,min_radius=1,max_radius=5,scale_radius=F,fillOpacity=0.8, providers="Esri.WorldTopoMap", addCircles=T,addMinicharts=F,factor_chart=2,buffer_zize=50, fun="sum",base_shape_args=NULL,layer_shape_args=NULL, rst=NULL,args_extra_shape=NULL,args_labels=NULL,...){


  if(is.factor(data[,1])){
    max_radius<-max_radius-2
  }


  max_radius<-max_radius*3
  min_radius=min_radius*3
  radius<-max_radius
  coords<-attr(data,"coords")
  if(class(coords)[1]=="matrix"){
    coords<-data.frame(coords)
  }
  colnames(coords)<-c("x","y")
  var<-colnames(data)
  df<-cbind(coords,z=data[,var])
  if(is.factor(df[,3])){
    palette <- colorFactor(pal,domain = df[,3])
    radius<-radius
  } else{
    radius<-if(isTRUE(scale_radius)){scales::rescale(df$z,c(min_radius,radius))}else{radius}
    palette <- colorBin(pal,domain = df[,3], bins = nbreaks)
  }
  map <- leaflet(df, options =leafletOptions()  )

  map<-add_shapes1(map,data,layer_shape_args,base_shape_args)

  map<-add_extraLL(map,data,args_extra_shape)

  if(isTRUE(addCircles)){
    map <- map |> addCircleMarkers(
      lng = ~x,
      lat = ~y ,
      fillOpacity = fillOpacity,
      stroke =F,
      radius =radius,
      color = ~palette(z),
      fillColor = ~palette(z),
      popup = ~paste(var, z),
      group="circles"
    )
  }

  if(isTRUE(addMinicharts)){
    map<-add_pie_chart(map,data,factor_chart,buffer_zize,fun, min_radius,max_radius, pal)
  }
  map <- map |> addProviderTiles(providers)


  map0<-map
  if(!is.null(rst)){
    if(!is.factor(df[,3])){

      pal <- colorNumeric(pal,domain = df[,3], na.color ='#00000000')


    }

    map <- map |> addRasterImage(rst, colors = pal, opacity = fillOpacity, group="raster")
  }



  if(!is.null(args_labels)){
    if(isTRUE(args_labels$show_labels)){
      if(!is.null(args_labels$labels)){
      labels<-attr(data,"factors")[,args_labels$labels]
      map<- map |>
        addLabelOnlyMarkers( lng = df$x,
                             lat = df$y, label =  ~as.character(labels),
                             group="labels",
                             labelOptions = labelOptions(
                               style =list('color'=args_labels$col.fac),
                               direction = 'center',

                               noHide = T,
                               textOnly = T,
                               textsize=paste0(args_labels$cex.fac*2,"px")))}

  }}


  if(!isTRUE(addMinicharts)){
    map <- map |> addLegend("bottomright",
                            pal = palette,
                            values = ~z,
                            title = var,
                            opacity = 1)
  }
  overlayGroups<-control_layers(addCircles,addMinicharts,rst,base_shape_args,layer_shape_args,args_labels)
  map <- map |> addLayersControl(overlayGroups = overlayGroups)
  map
}
gg_add_extra_shape<-function(p, rst,extra_shape_args  ){
  if(is.null(extra_shape_args)){
    return(p)
  }
  extralayers<-data.frame(extra_shape_args$args_extra)


  extralayers$layers<-as.logical(extralayers$layers)

  extras<-attr(rst,"extra_shape")
  if(is.null(extras)){
    return(p)
  }
  if(!any(extralayers$layers)){
    return(p)
  }
  if(any(extralayers$layers)){
    extralayers<-subset(extralayers,isTRUE(layers))

    for(i in 1:length(  extralayers$layers)){
      col_extra<-extralayers$colors[i]
      p<-p+geom_sf(data=st_as_sf(extras[[i]]), col=mylighten(col_extra,as.numeric(extralayers$alphas[i])), lty=1)
      names( p$layers)[length( p$layers)]<-paste0("extra",i)
    }
    return(p)
  }
}

gg_rst<-function(rst=NULL,data=NULL,limits=NULL,main="",subtitle="",axis.text_size=13,axis.title_size=13,plot.title_size=13,plot.subtitle_size=13,legend.text_size=13,layer_shape=NULL,extra_shape=NULL,breaks=T,n_breaks=5,pal="turbo",key.height=NULL,newcolhabs=list(turbo=viridis::turbo),add_extra_shape=F,add_base_shape=T,add_layer_shape=F,show_labels=F,labels=attr(rst,"factors")[1],cex.fac=13,col.fac="red",show_coords=F,col.coords="black",cex.coords=13,
                 show_guides=F,layer_col="gray",lighten=0.4,layer_shape_border="gray",
                 keyscale=30,width_hint=0.15,cex_scabar=0.7,fillOpacity=1,reverse_palette=F,
                 crs.info="+proj=longlat +datum=WGS84 +no_defs",scale_radius=T,
                 min_radius=1,max_radius=5,addCircles=F,addMinicharts=F,buffer_zize=1,
                 fun="sum",factor_chart=1,args_extra_shape=NULL,data_depth=NULL,
                 base_shape_args,layer_shape_args,...) {
  req(!is.null(rst)|!is.null(data))
  if(class(rst)[1]=="RasterLayer"){
    crs.info<-crs(rst)
    rasterpoints <-  rasterToPoints(rst) |> data.frame()
    coords<-coordinates(rst) |> data.frame()
  } else{
    coords<-attr(data,"coords")
    rasterpoints<-data.frame(cbind(coords,z=data[,1]))
    rst<-data
  }

  colnames(rasterpoints)<-c("x","y","z")

  base_shape<-attr(rst,'base_shape')
  layer_shape<-attr(rst,'layer_shape')
  if(!exists('limits')){
    limits<-NULL
  }
  if(is.null(limits)){
    limits<-get_limits(NULL,base_shape,layer_shape,coords)
    limits<-list(x_min=limits[1,1],x_max=limits[2,1], y_min=limits[1,2],y_max=limits[2,2])
  }

  if(!exists('breaks')){
    breaks<-NULL
  }

  if(!is.factor(rst[,1])){
    if(isFALSE(breaks)){breaks=pretty(rasterpoints$z)} else{
      breaks=    breaks_interval(rasterpoints$z,n_breaks)
    }
  } else{
    if(isFALSE(breaks)){breaks=pretty(rasterpoints$z)} else{
      breaks=    levels(rst[,1])
    }
  }
  {


    base_shape0<-st_as_sf(coords,coords = colnames( coords),crs=crs.info)
    p<-ggplot(base_shape0) #+ coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)
    xlim<-unlist(limits[1:2])
    ylim<-unlist(limits[3:4])
    p<-add_gg_shape(p,base_shape_args,base_shape,xlim,ylim,crs.info)



    if(class(rst)=="RasterLayer"){

      p<-rst_tile(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,breaks)

    } else{
      if(isTRUE(addCircles))
        p<-gg_circles(p,rasterpoints,rst,newcolhabs,pal,fillOpacity,reverse_palette,name,breaks,min_radius,max_radius, scale_radius)
      if(isTRUE(addMinicharts)){
        p<-gg_pie(p,rst,factor_chart,buffer_zize,fun,min_radius,max_radius,newcolhabs,pal,reverse_palette, fillOpacity)
      }
    }
    names( p$layers)[length( p$layers)]<-paste0('points')

    extra_shape<-attr(rst,'extra_shape')
    if(!is.null(args_extra_shape)){
      p<-gg_add_extra_shape(p,rst,args_extra_shape)
    }

    p0<-p


    p<-p+
      xlab("Longitude") +
      ylab("Latitude") +
      ggtitle(main, subtitle = subtitle) +
      theme(panel.grid.major = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(fill=NA,
                                        color="black",
                                        linewidth=0.5,
                                        linetype="solid"),
            axis.text=element_text(size=axis.text_size),
            axis.title=element_text(size=axis.title_size,face="bold"),
            plot.title=element_text(size=plot.title_size),
            plot.subtitle=element_text(size=plot.subtitle_size),
            legend.text=element_text(size=legend.text_size),
            legend.title=element_text(size=legend.text_size))
    if(!is.null(key.height)){
      p<-p+theme(legend.key.size=unit(key.height, 'pt'))
    }

    if(isTRUE(show_labels)){
      labels=attr(data,"factors")[labels]
      coords_labels<-attr(data,"coords")
      labs<-labels[rownames(coords_labels),]
      df_labels<-cbind(coords_labels,labs)
      colnames(df_labels)<-c("x","y","label")
      p<-p+  geom_text( data=df_labels, aes(x=x, y=y, label=label),size=cex.fac,colour=col.fac)
    }
    if(isTRUE(show_coords)){
      coords_data<-attr(rst,"coords")
      colnames(coords_data)<-c("x","y")
      p<-p+geom_point( data=coords_data, aes(x=x, y=y),  size=cex.coords, pch=3, colour=col.coords)}
    if(isTRUE(show_guides)){
      coords_data<-attr(rst,"coords")
      colnames(coords_data)<-c("x","y")

      xcoords<-pretty(coords_data[,1])
      ycoords<-pretty(coords_data[,2])
      p<-p+geom_hline(yintercept=ycoords,color = gray(.5), linetype = "dashed", linewidth = .15)+
        geom_vline(xintercept =xcoords,color = gray(.5), linetype = "dashed", linewidth = .15)
    }
  }


  p<-p+annotation_scale(location = "br", width_hint = width_hint,text_cex=cex_scabar,height  =unit(cex_scabar/4,"cm")) +
    annotation_north_arrow(location = "tl",
                           which_north = "true",
                           width = unit(keyscale, "pt"),
                           height  = unit(keyscale, "pt"),
                           pad_x = unit(.1, "in"),
                           pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering)



  p<-add_gg_shape(p,layer_shape_args,layer_shape,xlim,ylim,crs.info)

  #  point_layer<-p$layers[ grep("points",  names(p$layers))]
  #  old_layer<-p$layers[-grep("points",  names(p$layers))]
  #  new_p <- append(old_layer, point_layer, after=0)
  # p$layers<-new_p



  if(!is.null(data_depth)) {
     point_layer<-p$layers[ grep("points",  names(p$layers))]
     old_layer<-p$layers[-grep("points",  names(p$layers))]
     new_p <- append(old_layer, point_layer, after=data_depth-1)
     p$layers<-new_p

  }

  p<-p+ coord_sf(xlim = xlim, ylim = ylim, expand = FALSE)

  return(p)

}
