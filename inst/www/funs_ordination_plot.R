
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.

#' @export
getbp_som2<-function(m,indicate,npic,hc){

  if(is.null(indicate)){return(NULL)}
  grid<-m$grid$pts
  grid.size<-nrow(grid)
  nb <- table(factor(m$unit.classif, levels=1:grid.size))

  CORMAP<-lapply(m$codes,function(x){
    apply(x,2,weighted.correlation,w=nb,grille=m)
  })
  names(CORMAP)<-NULL
  CORMAP<-do.call(cbind,CORMAP)
  sigma2<-lapply(m$codes,function(xx){
    sqrt(apply(xx,2,function(x,effectif){m2<-sum(effectif*(x- weighted.mean(x,effectif, na.rm=T))^2, na.rm=T)/(sum(effectif, na.rm=T)-1)},effectif=nb))
  })
  names(sigma2)<-NULL
  sigma2<-do.call(c,sigma2)
  scores<-coord_vars<-data.frame(t(data.frame(CORMAP, row.names=c('x','y'))))
  scores[,1]<-  scales::rescale(coord_vars[,1], c(min(grid[,1]), max(grid[,1])))
  scores[,2]<-  scales::rescale(coord_vars[,2], c(min(grid[,2]), max(grid[,2])))
  #scores<-coord_vars
  scores<-na.omit(scores)
  sigma2<-sigma2[rownames(scores)]

  if(indicate=="cor"){
    indicadores<-rownames(biplot_chull(coord_vars,apply(grid,2,mean),biplot_n=npic))
  } else if(indicate=="var") {
    indicadores<-na.omit(names(sort(sigma2,decreasing=T))[1:npic])
  } else if(indicate=="cor_hc"){
    centers=apply(scores,2,mean)
    grid2<-data.frame(grid)
    grid2$hc<-hc
    df_temp<-classif_species(scores,grid2)
    indicadores<-get_maxdistances_clusters(df_temp,npic)
  }



  bp<-result<-scores[indicadores,]
  bp$id<-rownames(bp)
  bp
}
#' @export
classif_species<-function(coords_unclassif,coords_clusters){
  # Converta os data.frames em formatos mais manipuláveis
  coords_unclassif <- as.data.frame(coords_unclassif)
  coords_clusters <- as.data.frame(coords_clusters)

  # Função para calcular a distância euclidiana
  calc_dist <- function(x1, y1, x2, y2) {
    return(sqrt((x1 - x2)^2 + (y1 - y2)^2))
  }

  # Use um loop para percorrer cada linha de coords_unclassif
  for(i in 1:nrow(coords_unclassif)) {

    # Inicialize uma variável para armazenar a menor distância e o cluster correspondente
    min_dist <- Inf
    min_cluster <- NA

    # Obtenha as coordenadas do ponto não classificado atual
    x_unclassif <- coords_unclassif[i, 'x']
    y_unclassif <- coords_unclassif[i, 'y']

    # Use outro loop para percorrer cada linha de coords_clusters
    for(j in 1:nrow(coords_clusters)) {

      # Obtenha as coordenadas e o cluster do ponto de cluster atual
      x_cluster <- coords_clusters[j, 'x']
      y_cluster <- coords_clusters[j, 'y']
      cluster <- coords_clusters[j, 'hc']

      # Calcule a distância entre os pontos
      dist <- calc_dist(x_unclassif, y_unclassif, x_cluster, y_cluster)

      # Se a distância for menor que a menor distância encontrada até agora,
      # atualize a menor distância e o cluster correspondente
      if(dist < min_dist) {
        min_dist <- dist
        min_cluster <- cluster
      }
    }

    # Atribua o cluster com a menor distância à linha correspondente em coords_unclassif
    coords_unclassif[i, 'cluster'] <- min_cluster
  }

  coords_unclassif

}
#' @export
get_maxdistances_clusters <- function(coords, n, centro = c(0, 0)) {
  if(n < 1){n <- 1}

  # Calcule a distância de cada ponto a partir do centro
  coords$dist_to_center <- sqrt((coords$x - centro[1])^2 + (coords$y - centro[2])^2)

  # Verifique os cluster únicos disponíveis
  unique_cluster <- unique(coords$cluster)

  # Determine o cluster do ponto com a maior distância
  max_distance_cluster <- coords$cluster[which.max(coords$dist_to_center)]

  # Ajuste a ordem dos cluster com base no cluster do ponto com a maior distância
  start_idx <- which(unique_cluster == max_distance_cluster)
  cluster_order <- c(unique_cluster[start_idx:length(unique_cluster)], unique_cluster[1:(start_idx-1)])

  results <- c()
  iteration <- 1
  while(length(results) < n) {
    for (cl in cluster_order) {
      subset_coords <- coords[coords$cluster == cl,]
      subset_coords_sorted <- subset_coords[order(-subset_coords$dist_to_center),]
      if(nrow(subset_coords_sorted) >= iteration) {
        results <- unique(c(results, rownames(subset_coords_sorted)[iteration]))

        if(length(results) == n) break
      }
    }
    iteration <- iteration + 1
  }

  return(results)
}
#' @export
get_maxdistances <- function(coords, top, centro = c(0, 0)) {
  if(top<1){top<-1}
  # 1. Calculate the distance of each point from the center
  coords$dist_to_center <- sqrt((coords$x - centro[1])^2 + (coords$y - centro[2])^2)
  # 2. Calculate the angle of each point with respect to the x-axis
  coords$angle <- atan2(coords$y - centro[2], coords$x - centro[1])
  # 3. Order by angle descending (clockwise)
  coords_sorted_by_angle <- coords[order(coords$angle),]
  # 4. Order the top "top" points by distance descending
  coords_sorted_by_distance <- coords_sorted_by_angle[1:top,][order(-coords_sorted_by_angle$dist_to_center[1:top]),]

  top_points <- rownames(na.omit(coords_sorted_by_distance))
  return(top_points)
}
#' @export
get_maxdistances_quadrants <- function(coords, n, centro = c(0, 0)) {
  if (n < 1) {
    n <- 1
  }
  coords$dist_to_center <- sqrt((coords$x - centro[1])^2 + (coords$y - centro[2])^2)
  coords$quadrant <- ifelse(coords$x < centro[1] & coords$y < centro[2], "BottomLeft",
                            ifelse(coords$x < centro[1] & coords$y >= centro[2], "TopLeft",
                                   ifelse(coords$x >= centro[1] & coords$y >= centro[2], "TopRight", "BottomRight")))
  max_distance_quadrant <- coords$quadrant[which.max(coords$dist_to_center)]
  all_quadrants <- c("BottomLeft", "TopLeft", "TopRight", "BottomRight")
  start_idx <- which(all_quadrants == max_distance_quadrant)
  quadrant_order <- c(all_quadrants[start_idx:length(all_quadrants)], all_quadrants[1:(start_idx - 1)])

  results <- character(0)
  selected_rows <- numeric(0)

  n <- min(n, nrow(coords))

  # Revisão: simplificar a iteração e seleção de pontos
  for (q in quadrant_order) {
    subset_coords <- coords[coords$quadrant == q, ]
    subset_coords_sorted <- subset_coords[order(-subset_coords$dist_to_center), ]

    if (n > length(results)) {
      rows_to_add <- rownames(subset_coords_sorted)[!(rownames(subset_coords_sorted) %in% selected_rows)]
      num_to_add <- min(n - length(results), length(rows_to_add))
      if (num_to_add > 0) {
        results <- c(results, rows_to_add[1:num_to_add])
        selected_rows <- c(selected_rows, rows_to_add[1:num_to_add])
      }
    }
  }

  return(results)
}


#' @export
switch_theme<-function(p,theme, base_size){
  p<-switch(theme,
            'theme_grey'={p+theme_grey(base_size)},
            'theme_bw'={p+theme_bw(base_size)},
            'theme_linedraw'={p+theme_linedraw(base_size)},
            'theme_light'={p+theme_light(base_size)},
            'theme_dark'={p+theme_dark(base_size)},
            'theme_minimal'={p+theme_minimal(base_size)},
            'theme_classic'={p+theme_classic(base_size)},
            'theme_void'={p+theme_void(base_size)})
  p
}
#' @export
biplot_chull<-function(biplot_coords,center=c(0,0),biplot_n=10){
  biplot_coords0<-biplot_coords


  borders<-chull(biplot_coords)
  center=data.frame(x=center[1],y=center[2])
  rownames(center)<-"center"
  result<-list()
  repeat({
    borders<-unique(borders)
    binew<-biplot_coords[ borders,]
    biplot_coords<-biplot_coords[-borders,]
    borders<-chull(biplot_coords)
    bires<-binew
    bires$chull<-length(result)+1
    result[[length(result)+1]]<-bires
    if(nrow(do.call(rbind,result))>=biplot_n){


      break()
    }
  })

  result_chull<-do.call(rbind,result)
  result_chull$dist<-as.matrix(dist(rbind(center,result_chull[,1:2])))[1,-1]

  result_chull$chull<--result_chull$chull


  order_biplot<-order(result_chull$chull,result_chull$dist,decreasing = T)[1:biplot_n]


  binew<-result_chull[rownames(result_chull)[order_biplot],colnames(biplot_coords0)]
  binew

}
#' @export
ggpca<-function(model, base_size=12, theme='theme_bw', title="Principal component analysis", show_intercept=T, constr=F, points=T, points_factor=NULL, points_palette=colorRampPalette("black"), points_shape=16, points_size=4, text=F, text_factor=NULL, text_palette=colorRampPalette("gray"), text_size=4, biplot=T, biplot_n=5,  biplot_size=4, biplot_color="blue", biplot_arrow_color="blue", loading_axis=T, lo_x.text="PC1 loadings",lo_y.text="PC2 loadings", lo_axis_color=T,expandX =0.1, expandY=0.1,scale_shape=T){
  {

    comps<-summary(model)

    xlab<-paste("PC I (",round(comps$importance[2,1]*100,2),"%", ")", sep="")
    ylab<-paste("PC II (",round(comps$importance[2,2]*100,2),"%", ")", sep="")
    PCA<-model
    choices = 1:2
    scale = 1
    #scores= PCA$x
    #lam = PCA$sdev[choices]
    #n = nrow(scores)
    #lam = lam * sqrt(n)
    #x = t(t(scores[,choices])/ lam)
    #y = t(t(PCA$rotation[,choices]) * lam)

    x<-data.frame(scores(model)[,c(1:2)])
    y<-data.frame(model$rotation[,c(1:2)])

    df1<-scores<-data.frame(x)
    df2<-loadings<-data.frame(y)
    colnames(df1)<-colnames(df2)<-colnames(x)<-colnames(y)<-c("x","y")
    range_scores_x <- range(df1$x)
    range_scores_y <- range(df1$y)

    # Intervalo para os loadings
    range_loadings_x <- range(df2$x)
    range_loadings_y <- range(df2$y)
    scale_factor_x <- diff(range_scores_x) / diff(range_loadings_x)
    scale_factor_y <- diff(range_scores_y) / diff(range_loadings_y)

    # Usa o menor dos dois fatores para preservar a proporção
    uniform_scale <- min(scale_factor_x, scale_factor_y)

    # Ajusta os loadings usando o fator de escala uniforme
    df2$x <- df2$x * uniform_scale
    df2$y <- df2$y * uniform_scale
    p<-ggplot(df1, aes(x, y))
  }

  {
    if(isTRUE(loading_axis)){
      p<-p+
        scale_y_continuous(
          expand=expansion(expandY),
          sec.axis = sec_axis(~ ./uniform_scale , name =lo_y.text ))+
        scale_x_continuous(
          expand=expansion(expandX),
          sec.axis = sec_axis(~ ./uniform_scale , name = lo_x.text))
    } else{
      p<-p+
        scale_y_continuous(
          expand=expansion(expandY))+
        scale_x_continuous(
          expand=expansion(expandX))
    }

    if(isTRUE(points)){
      df1$points_factor<-factor("")
      show.legend=F
      if(!is.null(points_factor)){
        df1$points_factor<-points_factor[,1]
        show.legend=T}
      col_points=points_palette(nlevels(df1$points_factor))
      p<-p+ggnewscale::new_scale_color()
      if(isTRUE(scale_shape)){
        p<-p+geom_point(aes(x,y,color=points_factor, shape=points_factor), data=df1,size=points_size,show.legend=show.legend)+scale_shape(colnames(points_factor))
      } else{
        p<-p+geom_point(aes(x,y,color=points_factor), data=df1,size=points_size,show.legend=show.legend)
      }
      p<-p+
        scale_color_manual(colnames(points_factor),values=col_points)
    }

    if(isTRUE(text)){
      df1$text_factor<-factor("")
      df1$text_label<-rownames(df1)
      col_text=text_palette(1)
      if(!is.null(text_factor)){
        df1$text_factor<-text_factor[,1]
        col_text=text_palette(nlevels(df1$text_factor))
        df1$text_label<-df1$text_factor
      }


      p<-p+ggnewscale::new_scale_color()
      p<-p+geom_text(aes(label=text_label, color=text_factor),data=df1,size=text_size, show.legend=F)+
        scale_color_manual(values=col_text)  +
        guides(color=FALSE)
    }


  }

  if(isTRUE(biplot)) {
    df2<-biplot_chull(df2,biplot_n=biplot_n)
    p<-p+geom_segment(data = df2,
                      aes(x = 0, y = 0, xend = x, yend = y),
                      arrow = arrow(type = "closed", length = unit(8, "pt")),
                      alpha = 0.75, color = biplot_arrow_color)+
      geom_text(data=df2,
                aes(x=x,y=y,label=rownames(df2),
                    hjust=0.5*(1-sign(x)),vjust=0.5*(1-sign(y))),
                color=biplot_color, size=biplot_size)


  }
  if(isTRUE(show_intercept)){
    p<-p+geom_hline(yintercept=0, linetype="dotted")
    p<-p+geom_vline(xintercept=0, linetype="dotted")
  }
  {
    p<-switch_theme(p,theme, base_size)
  }
  if(isTRUE(loading_axis)){
    if(isTRUE(lo_axis_color)) {
      p<-p+theme(
        axis.text.y.right = element_text(color = biplot_arrow_color),
        axis.title.y.right = element_text(color = biplot_arrow_color),
        axis.text.x.top = element_text(color = biplot_arrow_color),
        axis.title.x.top = element_text(color = biplot_arrow_color),
      )
    }
  }
  p<-p+xlab(xlab)+ylab(ylab)+ggtitle(title)

  return(p)
}
#' @export
clockvar<-function(df3,species_n,species_type){
  if(species_type=="clockwise_distance"){
    df3<-df3[get_maxdistances(df3,species_n),]
  } else{
    df3<-df3[get_maxdistances_quadrants(df3,species_n),]
  }
}
#' @export
ggrda<-function(model, base_size=12, theme='theme_bw', title="Redundancy analysis", show_intercept=T, constr=F, points=T, points_factor=NULL, points_palette=colorRampPalette("black"), points_shape=16, points_size=4, text=T, text_factor=NULL, text_palette=colorRampPalette("gray"), text_size=4, biplot=T, biplot_n=5,  biplot_size=4, biplot_color="blue", biplot_arrow_color="blue", species=T, species_n=5,  species_plot="text", species_size=4, species_shape=3, species_color="red",scale_shape=F,expandX=0.1,expandY=0.1){

  {

    un<-round(summary(model)$cont$importance[2,1:2]*100,2)
    con<-round(summary(model)$concont$importance[2,1:2]*100,2)
    if(isFALSE(constr)){labs<-un} else {labs<-con}
    xlab<-paste("RDA I -"," (",labs[1]," %", ")*", sep="")
    ylab<-paste("RDA II -"," (",labs[2]," %", ")*", sep="")

    smry <- summary(model)
    df1  <- data.frame(smry$sites[,1:2])
    df2  <- data.frame(smry$biplot[,1:2])
    df3  <- data.frame(smry$species[,1:2])
    colnames(df1)<-colnames(df2)<-colnames(df3)<-c("x","y")
    range_df1_x <- range(df1$x)
    range_df1_y <- range(df1$y)

    range_df2_x <- range(df2$x)
    range_df2_y <- range(df2$y)

    range_df3_x <- range(df3$x)
    range_df3_y <- range(df3$y)


    scale_factor_x <- diff(range_df1_x) / diff(range_df2_x)/diff(range_df3_x)
    scale_factor_y <- diff(range_df1_y) / diff(range_df2_y)/diff(range_df3_y)

    # Usa o menor dos dois fatores para preservar a proporção
    uniform_scale <- min(scale_factor_x, scale_factor_y)

    # Ajusta os loadings usando o fator de escala uniforme
    df2$x <- df2$x * uniform_scale
    df2$y <- df2$y * uniform_scale

    df3$x <- df3$x * uniform_scale
    df3$y <- df3$y * uniform_scale
    df1$shape<-points_shape
    df3$shape<-species_shape
  }


  p<-ggplot(df1, aes(x, y))
  p<-p+
    scale_y_continuous(
      expand=expansion(expandY))+
    scale_x_continuous(
      expand=expansion(expandX))

  if(isTRUE(points)){


    df1$points_factor<-factor("")
    show.legend=F
    if(!is.null(points_factor)){
      df1$points_factor<-points_factor[,1]
      show.legend=T

    }
    col_points=points_palette(nlevels(df1$points_factor))
    p<-p+geom_point(aes(x,y,color=points_factor, shape=shape), data=df1,size=points_size,show.legend=show.legend, shape=points_shape)+   scale_color_manual(colnames(points_factor),values=col_points)
  }
  p0<-p
  if(isTRUE(species)){
    species_plot<-match.arg(species_plot,c("points","text"))


    df3<-biplot_chull(df3,biplot_n=species_n)
    df3$points_factor<-factor("Response variables")
    if(species_plot=="points"){
      p<-p+geom_point(aes(x,y, shape=shape, color=points_factor),data=df3, color=species_color, size=species_size, fill = 'red')+ scale_shape_identity(name="",guide="legend", labels="Response") +  guides(
        color=guide_legend(order=1),
        shape = guide_legend(
          order=2,override.aes = list(linetype = c(1), shape = c(species_shape), label=NA)
        )
      )

    } else{
      p<-p+geom_text(aes(x,y, label=rownames(df3)),data=df3, color=species_color, size=species_size)
      p+scale_size_identity()
    }
  }
  if(isTRUE(text)){
    df1$text_factor<-factor("")
    df1$text_label<-rownames(df1)
    col_text=text_palette(1)
    if(!is.null(text_factor)){
      df1$text_factor<-text_factor[,1]
      col_text=text_palette(nlevels(df1$text_factor))
      df1$text_label<-df1$text_factor
    }


    suppressWarnings({
      p<-p+ggnewscale::new_scale_color()+geom_text(aes(label=text_label, color=text_factor),data=df1,size=text_size, show.legend=F)+
        scale_color_manual(values=col_text)  +
        guides(color=FALSE)
    })


  }
  if(isTRUE(biplot)) {
    if(biplot_n>nrow(df2)){
      biplot_n<-nrow(df2)
    }
    df2<-biplot_chull(df2,biplot_n=biplot_n)

    p<-p+
      geom_segment(data=df2, aes(x=0, xend=x, y=0, yend=y),
                   color=biplot_arrow_color, arrow=arrow(length=unit(0.01,"npc"))) +
      geom_text(data=df2,
                aes(x=x,y=y,label=rownames(df2),
                    hjust=0.5*(1-sign(x)),vjust=0.5*(1-sign(y))),
                color=biplot_color, size=biplot_size)

  }
  if(isTRUE(show_intercept)){
    p<-p+geom_hline(yintercept=0, linetype="dotted")
    p<-p+geom_vline(xintercept=0, linetype="dotted")
  }
  {
    p<-switch_theme(p,theme, base_size)
  }
  p<-p+xlab(xlab)+ylab(ylab)+ggtitle(title)
  return(p)
}
#' @export
ggmds<-function(model, base_size=12, theme='theme_bw', title="Multidimensional scaling", show_intercept=T, constr=F, points=T, points_factor=NULL, points_palette=colorRampPalette("black"), points_shape=16, points_size=4, text=F, text_factor=NULL, text_palette=colorRampPalette("gray"), text_size=4, expandX =0.1, expandY =0.1,xlab="MDS I",ylab="MDS II", scale_shape=F,mds_stress=T){
  {

    df1<-data.frame(scores(model,"sites"))
    colnames(df1)<-c("x","y")

    p<-ggplot(df1, aes(x, y))
  }

  {

    p<-p+
      scale_y_continuous(
        expand=expansion(expandY))+
      scale_x_continuous(
        expand=expansion(expandX))


    if(isTRUE(points)){
      df1$points_factor<-factor("")
      show.legend=F
      if(!is.null(points_factor)){
        df1$points_factor<-points_factor[,1]
        show.legend=T}
      col_points=points_palette(nlevels(df1$points_factor))
      p<-p+ggnewscale::new_scale_color()
      if(isTRUE(scale_shape)){
        p<-p+geom_point(aes(x,y,color=points_factor, shape=points_factor), data=df1,size=points_size,show.legend=show.legend)+scale_shape(colnames(points_factor))
      } else{
        p<-p+geom_point(aes(x,y,color=points_factor), data=df1,size=points_size,show.legend=show.legend)
      }
      p<-p+
        scale_color_manual(colnames(points_factor),values=col_points)
    }

    if(isTRUE(text)){
      df1$text_factor<-factor("")
      df1$text_label<-rownames(df1)
      col_text=text_palette(1)
      if(!is.null(text_factor)){
        df1$text_factor<-text_factor[,1]
        col_text=text_palette(nlevels(df1$text_factor))
        df1$text_label<-df1$text_factor
      }


      p<-p+ggnewscale::new_scale_color()
      p<-p+geom_text(aes(label=text_label, color=text_factor),data=df1,size=text_size, show.legend=F)+
        scale_color_manual(values=col_text)  +
        guides(color=FALSE)
    }


  }

  if(isTRUE(show_intercept)){
    p<-p+geom_hline(yintercept=0, linetype="dotted")
    p<-p+geom_vline(xintercept=0, linetype="dotted")
  }
  p<-switch_theme(p,theme, base_size)
  p<-p+xlab(xlab)+ylab(ylab)+ggtitle(title)
  stress=paste0(
    paste("Stress:",round(model$stress,4)), paste0("\nDissimilarity:", "'",model$distmethod,"'")
  )
  if(isTRUE(mds_stress)){
    p<-p + annotate(geom="text", x=Inf, y=Inf, label=stress,
                    vjust=1.1, hjust=1.1)
  }

  return(p)
}
