
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.
render_list<-function(result, limited=T,auto=T,rownames=T,n_copy=5,n_result=20, n_print=15){

  r<-try(data.frame(result),silent = T)
  try({
    colnames(r)<-colnames(result)
  },silent=T)
  dom="ltpB"
  buttons = c('copy', 'csv', 'excel')
  if(!inherits(r,"try-error")){
    result<-r
  }
  div(class="half-drop-inline",style="width:500px",
      if(!is.null(dim(result))){
        if(length(unlist(result))<n_print){
          return(renderPrint(result))
        }
        if(nrow(result)<n_result){
          dom="tB"
        }
        if(limited)
          if(nrow(result)<n_copy){
            buttons = c('copy')
          }

        fixed_dt(
          result,pageLength = 20,dom=dom,
          round=NULL,scrollY = NULL,
          extensions = c("FixedHeader",'Buttons'),
          buttons =buttons,
          rownames=rownames
        )
      } else{
        renderPrint(result)
      }


  )

}
get_selected_from_choices<-function(selected, choices){
  if(!is.null(selected)){
    if(!any(selected%in%choices)){
      selected<-NULL
    }
  }
  selected[selected%in%choices]
}
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
# Continuous color scale for points colored by a numeric variable (breaks in attr "breaks")
ord_gradient_scale<-function(points_factor,points_palette,name=colnames(points_factor)){
  v<-points_factor[,1]
  br<-attr(points_factor,"breaks")
  ggplot2::scale_color_gradientn(
    name=name,
    colours=points_palette(256),
    limits=range(c(v,br),na.rm=TRUE),
    breaks=if(is.null(br)) ggplot2::waiver() else br
  )
}

ggpca<-function(model, base_size=12, theme='theme_bw', title="Principal component analysis", show_intercept=T, constr=F, points=T, points_factor=NULL, points_palette=colorRampPalette("black"), points_shape=16, points_size=4, text=F, text_factor=NULL, text_palette=colorRampPalette("gray"), text_size=4, biplot=T, biplot_n=5,  biplot_size=4, biplot_color="blue", biplot_arrow_color="blue", loading_axis=T, lo_x.text="PC1 loadings",lo_y.text="PC2 loadings", lo_axis_color=T,expandX =0.1, expandY=0.1,scale_shape=T,points_legend=T,xlab="PC I",ylab="PC II",show_axis_explain=T){
  {

    comps<-summary(model)

    if(isTRUE(show_axis_explain)){
      xlab<-paste(xlab," (",round(comps$importance[2,1]*100,2),"%", ")", sep="")
      ylab<-paste(ylab," (",round(comps$importance[2,2]*100,2),"%", ")", sep="")
    }

    PCA<-model
    choices = 1:2
    scale = 1
    #scores= PCA$x
    #lam = PCA$sdev[choices]
    #n = nrow(scores)
    #lam = lam * sqrt(n)
    #x = t(t(scores[,choices])/ lam)
    #y = t(t(PCA$rotation[,choices]) * lam)

    x<-data.frame(vegan::scores(model)[,c(1:2)])
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

    if(isTRUE(points)&&!is.null(points_factor)&&is.numeric(points_factor[,1])){
      # numeric variable: continuous gradient with the chosen breaks
      df1$points_value<-points_factor[,1]
      p<-p+ggnewscale::new_scale_color()+
        geom_point(aes(x,y,color=points_value),shape=points_shape,data=df1,size=points_size)+
        ord_gradient_scale(points_factor,points_palette)
    } else if(isTRUE(points)){
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
        p<-p+geom_point(aes(x,y,color=points_factor), shape=points_shape,data=df1,size=points_size,show.legend=show.legend)
      }
      p<-p+
        scale_color_manual(colnames(points_factor),values=col_points)
      if(col_points[1]==col_points[2]){
        p<-p+guides(color="none")
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
  if(isFALSE(points_legend)){
    p<-p+guides(color="none")
  }
  p<-p+xlab(xlab)+ylab(ylab)+ggtitle(title)

  return(p)
}

library(vegan)
#' @export
ggrda<-function(model, base_size=12, theme='theme_bw', title="Redundancy analysis", show_intercept=T, constr=F, points=T, points_factor=NULL, points_palette=colorRampPalette("black"), points_shape=16, points_size=4, text=T, text_factor=NULL, text_palette=colorRampPalette("gray"), text_size=4, biplot=T, biplot_n=5,  biplot_size=4, biplot_color="blue", biplot_arrow_color="blue", species=T, species_n=5,  species_plot="text", species_size=4, species_shape=3, species_color="red",scale_shape=F,expandX=0.1,expandY=0.1,legend_points="",legend_response="",show_response_legend=T,
                show_points_legend=T,xlab="RDA I", ylab="RDA II",show_axis_explain=T){

  smry <- summary(model)

  # ------------------------------------------------------------
  # 1. Escolher se o gráfico será constrained ou unconstrained
  # ------------------------------------------------------------

  n_constr <- model$CCA$rank
  n_uncon  <- model$CA$rank

  if (isTRUE(constr)) {

    if (n_constr < 2) {
      stop("O modelo não tem pelo menos dois eixos constrained.")
    }

    choices_use <- 1:2

    labs <- round(smry$concont$importance[2, 1:2] * 100, 2)

    if (is.null(xlab)) xlab <- "RDA1"
    if (is.null(ylab)) ylab <- "RDA2"

  } else {

    if (n_uncon < 2) {
      stop("O modelo não tem pelo menos dois eixos unconstrained.")
    }

    # Os eixos unconstrained vêm depois dos eixos constrained
    choices_use <- (n_constr + 1):(n_constr + 2)

    labs <- round(smry$cont$importance[2, 1:2] * 100, 2)

    if (is.null(xlab)) xlab <- "PC1"
    if (is.null(ylab)) ylab <- "PC2"

    if (isTRUE(biplot)) {
      warning("As setas ambientais do biplot são interpretáveis nos eixos constrained. Como constr = FALSE, biplot foi definido como FALSE.")
      biplot <- FALSE
    }
  }

  if (isTRUE(show_axis_explain)) {
    xlab <- paste0(xlab, " (", labs[1], "%)")
    ylab <- paste0(ylab, " (", labs[2], "%)")
  }

  # ------------------------------------------------------------
  # 2. Extrair scores
  # ------------------------------------------------------------

  scores_model <- vegan::scores(model, choices = choices_use)

  df1 <- data.frame(scores_model$sites[, 1:2, drop = FALSE])
  colnames(df1) <- c("x", "y")

  df1$shape <- points_shape

  # Biplot ambiental
  if (isTRUE(biplot) && !is.null(scores_model$biplot)) {
    df2 <- data.frame(scores_model$biplot[, 1:2, drop = FALSE])
    colnames(df2) <- c("x", "y")
  } else {
    df2 <- NULL
  }

  # Espécies / variáveis resposta
  if (isTRUE(species) && !is.null(scores_model$species)) {
    df3 <- data.frame(scores_model$species[, 1:2, drop = FALSE])
    colnames(df3) <- c("x", "y")
    df3$shape <- species_shape
  } else {
    df3 <- NULL
    species <- FALSE
  }

  # ------------------------------------------------------------
  # 3. Escalonar biplot e espécies em relação aos sites
  # ------------------------------------------------------------

  range_df1_x <- range(df1$x, na.rm = TRUE)
  range_df1_y <- range(df1$y, na.rm = TRUE)

  if (!is.null(df2) && nrow(df2) > 0) {

    range_df2_x <- range(df2$x, na.rm = TRUE)
    range_df2_y <- range(df2$y, na.rm = TRUE)

    scale_biplot_x <- diff(range_df1_x) / diff(range_df2_x)
    scale_biplot_y <- diff(range_df1_y) / diff(range_df2_y)

    scale_biplot <- min(scale_biplot_x, scale_biplot_y, na.rm = TRUE)

    if (is.finite(scale_biplot)) {
      df2$x <- df2$x * scale_biplot * 0.7
      df2$y <- df2$y * scale_biplot * 0.7
    }
  }

  if (!is.null(df3) && nrow(df3) > 0) {

    range_df3_x <- range(df3$x, na.rm = TRUE)
    range_df3_y <- range(df3$y, na.rm = TRUE)

    scale_species_x <- diff(range_df1_x) / diff(range_df3_x)
    scale_species_y <- diff(range_df1_y) / diff(range_df3_y)

    scale_species <- min(scale_species_x, scale_species_y, na.rm = TRUE)

    if (is.finite(scale_species)) {
      df3$x <- df3$x * scale_species * 0.7
      df3$y <- df3$y * scale_species * 0.7
    }
  }

  # ------------------------------------------------------------
  # 4. Criar gráfico base
  # ------------------------------------------------------------

  p <- ggplot2::ggplot(df1, ggplot2::aes(x, y)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = expandY)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = expandX))

  # ------------------------------------------------------------
  # 5. Pontos das amostras
  # ------------------------------------------------------------

  col_points <- NA

  if (isTRUE(points) && !is.null(points_factor) && is.numeric(points_factor[, 1])) {

    # numeric variable: continuous gradient with the chosen breaks
    df1$points_value <- points_factor[, 1]
    col_points <- points_palette(256)
    p <- p +
      ggplot2::geom_point(
        data = df1,
        ggplot2::aes(x = x, y = y, color = points_value),
        size = points_size,
        shape = points_shape,
        show.legend = isTRUE(show_points_legend)
      ) +
      ord_gradient_scale(points_factor, points_palette,
                         name = if (nzchar(legend_points)) legend_points else colnames(points_factor))

  } else if (isTRUE(points)) {

    df1$points_factor <- factor("")
    show.legend <- FALSE

    if (!is.null(points_factor)) {
      df1$points_factor <- as.factor(points_factor[, 1])
      show.legend <- TRUE
    }

    col_points <- points_palette(nlevels(df1$points_factor))

    p <- p +
      ggplot2::geom_point(
        data = df1,
        ggplot2::aes(x = x, y = y, color = points_factor),
        size = points_size,
        shape = points_shape,
        show.legend = show.legend
      ) +
      ggplot2::scale_color_manual(
        values = col_points,
        name = legend_points
      )
  }

  # ------------------------------------------------------------
  # 6. Espécies / variáveis resposta
  # ------------------------------------------------------------

  if (isTRUE(species) && !is.null(df3)) {

    species_plot <- match.arg(species_plot, c("points", "text"))

    if (species_n > nrow(df3)) {
      species_n <- nrow(df3)
    }

    df3 <- biplot_chull(df3, biplot_n = species_n)
    df3$points_factor <- factor("Response variables")

    if (species_plot == "points") {

      p <- p +
        ggplot2::geom_point(
          data = df3,
          ggplot2::aes(x = x, y = y),
          color = species_color,
          size = species_size,
          shape = species_shape
        )

    } else {

      p <- p +
        ggplot2::geom_text(
          data = df3,
          ggplot2::aes(x = x, y = y, label = rownames(df3)),
          color = species_color,
          size = species_size
        )
    }
  }

  # ------------------------------------------------------------
  # 7. Texto das amostras
  # ------------------------------------------------------------

  if (isTRUE(text)) {

    df1$text_factor <- factor("")
    df1$text_label <- rownames(df1)
    col_text <- text_palette(1)

    if (!is.null(text_factor)) {
      df1$text_factor <- as.factor(text_factor[, 1])
      col_text <- text_palette(nlevels(df1$text_factor))
      df1$text_label <- df1$text_factor
    }

    suppressWarnings({
      p <- p +
        ggnewscale::new_scale_color() +
        ggplot2::geom_text(
          data = df1,
          ggplot2::aes(label = text_label, color = text_factor),
          size = text_size,
          show.legend = FALSE
        ) +
        ggplot2::scale_color_manual(values = col_text) +
        ggplot2::guides(color = "none")
    })
  }

  # ------------------------------------------------------------
  # 8. Setas ambientais do biplot
  # ------------------------------------------------------------

  if (isTRUE(biplot) && !is.null(df2)) {

    if (biplot_n > nrow(df2)) {
      biplot_n <- nrow(df2)
    }

    df2 <- biplot_chull(df2, biplot_n = biplot_n)

    p <- p +
      ggplot2::geom_segment(
        data = df2,
        ggplot2::aes(x = 0, xend = x, y = 0, yend = y),
        color = biplot_arrow_color,
        arrow = ggplot2::arrow(length = grid::unit(0.01, "npc"))
      ) +
      ggplot2::geom_text(
        data = df2,
        ggplot2::aes(
          x = x,
          y = y,
          label = rownames(df2),
          hjust = 0.5 * (1 - sign(x)),
          vjust = 0.5 * (1 - sign(y))
        ),
        color = biplot_color,
        size = biplot_size
      )
  }

  # ------------------------------------------------------------
  # 9. Linhas de referência
  # ------------------------------------------------------------

  if (isTRUE(show_intercept)) {
    p <- p +
      ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted")
  }

  # ------------------------------------------------------------
  # 10. Tema, título e legendas
  # ------------------------------------------------------------

  p <- switch_theme(p, theme, base_size)

  p <- p +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::ggtitle(title)

  if (length(unique(col_points)) == 1) {
    p <- p + ggplot2::guides(color = "none")
  }

  if (isFALSE(show_points_legend)) {
    p <- p + ggplot2::guides(color = "none")
  }

  if (isFALSE(show_response_legend)) {
    p <- p + ggplot2::guides(shape = "none")
  }

  return(p)
}
#' @export
ggmds<-function(model, base_size=12, theme='theme_bw', title="Multidimensional scaling", show_intercept=T, constr=F, points=T, points_factor=NULL, points_palette=colorRampPalette("black"), points_shape=16, points_size=4, text=F, text_factor=NULL, text_palette=colorRampPalette("gray"), text_size=4, expandX =0.1, expandY =0.1,xlab="MDS I",ylab="MDS II", scale_shape=F,mds_stress=T){
  {

    df1<-data.frame(vegan::scores(model,"sites"))
    colnames(df1)<-c("x","y")

    p<-ggplot(df1, aes(x, y))
  }

  {

    p<-p+
      scale_y_continuous(
        expand=expansion(expandY))+
      scale_x_continuous(
        expand=expansion(expandX))


    if(isTRUE(points)&&!is.null(points_factor)&&is.numeric(points_factor[,1])){
      # numeric variable: continuous gradient with the chosen breaks
      df1$points_value<-points_factor[,1]
      p<-p+ggnewscale::new_scale_color()+
        geom_point(aes(x,y,color=points_value),shape=points_shape,data=df1,size=points_size)+
        ord_gradient_scale(points_factor,points_palette)
    } else if(isTRUE(points)){
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
