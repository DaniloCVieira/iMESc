
## Copyright © 2023 [Danilo Candido Vieira]
## Licensed under the CC BY-NC-ND 4.0 license.



#' @export
tipify_ui<-function(el, title, placement = "bottom", trigger = "hover", options = NULL){
  shinyBS::tipify(el, title, placement = placement, trigger = trigger, options = options)
}


#' @export
getdata_model<-function(m,which="train"){

  pic<-which(colnames(  m$trainingData)==".outcome")
  if(which=="train"){
    m$trainingData[-pic]
  } else{
    m$trainingData[,pic]
  }

}

#' @export
get_feature_data_plot<-function(df,m, npic){
  colnames(df)[2]<-"Metric"
  req(is.data.frame(df))


  df[,2]<-df$actual_metric-df[,2]
  df<-df[which(df$var%in%names(sort(sapply(split(df,df$var),function(x) mean(x$Metric)),decreasing=T)[1:npic])),]
}

#' @export





#' @export
sig_feature<-function(rands, m,newdata,obc,sig_level){
  metric<-ifelse(m$modelType=='Regression','Rsquared','Accuracy')
  lis<-split(rands,rands$var)
  actual_value<-caret::postResample(predict(m,as.matrix(newdata)), obc)[metric]
  x<-lis[['CASC_TOTAL_0A10']]
  res<-sapply(lis,function(x){
    permuted_values<-x[,metric]

    #distribuicao<-qnorm(permuted_values,mean=mean(permuted_values), sd=sd(permuted_values))
    p_value<-pnorm(actual_value,mean=mean(permuted_values), sd=sd(permuted_values), lower.tail = F)
    p_result<-p_value
    p_result
    xmean<-mean(permuted_values)
    c(actual_value=actual_value,mean=xmean,sd=sd(permuted_values),dff=actual_value-xmean,p_value=p_result)

  })
  perm_summary<-data.frame(t(res))
  colnames(perm_summary)<-c(
    'actual_value',
    paste0("mean_",metric,"(rand)"),
                            paste0("sd",metric,"(rand)"),
                            paste0("TrueAcc - ",paste0("mean_",metric,"(rand)")),
                            "p_value")

  perm_summary$sig<-""
  sigs<-which(perm_summary$p_value<sig_level)
  if(length(sigs)>0){
    perm_summary$sig[sigs]<-"*"
  }


  perm_summary
  }





#' @export
is.factor.matrix<-function (datamat, tolerance = 1e-08, completeThreshold = 5){
  idx <- apply(datamat, 1, function(x) !any(is.na(x)))
  if (sum(idx) > completeThreshold) {
    datamat <- datamat[idx, ]
    is.matrix(datamat) && min(datamat, na.rm = TRUE) >= 0 &&
      max(datamat, na.rm = TRUE) <= 1 && all(abs(rowSums(datamat) -
                                                   1) < tolerance)
  }  else {
    FALSE
  }
}

#' @export
feat<-function(m,moldels_importance,palette="turbo", newcolhabs=list(turbo=viridis::turbo),nvars=20,xlab="Importace",ylab="Variables", feat_type=c("Mean","side-by-side", "stacked")){

  if(length(m$levels)>1){
    m_levels<-m$levels
  } else{
    if(!is.na(m$levels)){
      m_levels<-m$levels
    } else{
      m_levels<-1
    }
  }


  imp_name<-"Mean importance"
  if(is.null(feat_type)){
    feat_type<-"Mean"
    imp_name<-"Importance"
  }

  ptype<-feat_type
  mi<-data.frame(moldels_importance)
  colnames(mi)<-paste0(m_levels)
  if(!feat_type%in%c("Mean","side-by-side", "stacked")){
    mi<-mi[feat_type]
  }

  df<-reshape2::melt(data.frame(id=rownames(mi),mi),"id")
  if(!feat_type%in%c("Mean","side-by-side", "stacked")){
    df$variable<-factor(df$variable, labels=feat_type, levels=levels(df$variable))

  } else{
    df$variable<-factor(df$variable, labels=m_levels, levels=levels(df$variable))

  }

  if(ptype=='Mean'){
    df2<-aggregate(df[-c(1:2)],df[1],sum)
    df2$variable<-imp_name
    df<-df2

  }

  mean_vals<-tapply(df$value,df$id, sum)
  col_names<-names(mean_vals)[order(mean_vals,decreasing = T)[1:nvars]]
  picvar<-which(df$id%in%col_names)
  colvalues<-newcolhabs[[palette]](length(m_levels))
  names(colvalues)<-m_levels
  if(!feat_type%in%c("Mean","side-by-side", "stacked")){
    colvalues<-colvalues[feat_type]
  }
  if(feat_type=="Mean"){
    if(length(m_levels)>1){
    col0<-colorspace::mixcolor(0.5,hex2RGB(colvalues[1]), hex2RGB(colvalues[2]))
    for(i in 3:length(colvalues)){
      col0<-colorspace::mixcolor(0.5,col0, hex2RGB(colvalues[i]))
    }
    colvalues=rgb(col0@coords[1,1],col0@coords[1,2],col0@coords[1,3])

  }}


  p<-ggplot(df[picvar,], aes(reorder(id,value, sum), value, fill=variable))+
    geom_bar(stat = "identity")+ scale_fill_manual(name='',values=as.character(colvalues))+coord_flip()



  if(imp_name=="Importance"|length(m_levels)==1){
  p<-p+ theme(legend.position = "none")}
  if(ptype=="side-by-side"){
    p<-p + facet_grid(~factor(variable, levels=colnames(mi)))

  }
  p+xlab(ylab)+ylab(xlab)
}















#' @export
tiphelp<-function(text,placement ="bottom"){
  tipify_ui(a(icon("fas fa-question-circle")),text,placement =placement)
}
tipright<-function(text,placement ="right",...){
  tipify_ui(a(icon("fas fa-question-circle")),text,placement =placement)
}
#' @export
tiphelp3<-function(title,text,placement ="bottom"){
  span(
    style="color: #3c8dbc;",
    title,tipify_ui(icon("fas fa-question-circle"),text,placement =placement ))
}



#' @export
pophelp<-function(title,text, placement="right"){
  shinyBS::popify(a(icon(verify_fa = FALSE,name=NULL,class="fas fa-question-circle")),title,text, placement = placement, trigger="hover")
}


#' @export
datalist_render<-function(datalist=NULL,bagdata=F,width="90px"){
  data=datalist
  factors=attr(data,"factors")
  datalist_name=attr(data,"datalist")

  coords=attr(data,"coords")
  base_shape=attr(data,"base_shape")
  layer_shape=attr(data,"layer_shape")
  data.factors=attr(data,"data.factors")
  transf=attr(data,"transf")
  div(tags$style(HTML(
    "
    .rdl{
        width: fit-content
    }
       .rdl_name h5{
    background: #e6e6e6ff;
    color: black;
    border: 1px dashed black;
    padding: 10px;

       }

    .rdl_items h5{
    color: #05668D
    }
    .rdl_items h5 div:before{
    color: green;
    width: 40px;
    margin-right: 5px;
    padding-bottom: 5px;
    content:' ';
    display: inline-block;
    border-top: 1px dashed black
    }



    .rdl_blocks{
    color: gray;
    padding-left: 20px
    }
    .rdl_in{
    padding-left: 45px
    }

    "
  )),
  div(class="rdl",

      div(class="rdl_name",
          h5(strong("Datalist:"),if(length(datalist_name)>0) {em(datalist_name)})
      ),
      div(class="rdl_items",

          if(length(data)>0){
            div(class="rdl_blocks",
                h5(div("Data-Attribute")),
                div(class="rdl_in",
                    em(paste0("n.obs:",nrow(data), ";"),
                       paste0("nvar-numeric:", ncol(data)), )
                )
            )},
          if(length(factors)>0){
            div(class="rdl_blocks",
                h5(div("Factor-Attribute")),
                div(class="rdl_in",
                    em(paste0("n.obs:",nrow(factors), ";"),
                       paste0("nvar:", ncol(factors)))
                )
            )},
          if(length(coords)>0){
            div(
              class="rdl_blocks",
              div(h5(div("Coords-Attribute"))),
              div(class="rdl_in",
                  em(paste0("n.obs:",nrow(coords), ";"),
                     paste0("nvar:", ncol(coords)))
              )


            )},
          if(length(base_shape)>0){
            div(
              class="rdl_blocks",
              div(h5(div("Base-shape-Attribute"))),
              div(class="rdl_in",
                  em("sf object"))

            )},
          if(length(layer_shape)>0){
            div(class="rdl_blocks",
                div(h5(div("Layer-shape-Attribute"))),
                div(class="rdl_in",
                    em("sf object"))
            )}


      ))
  )

}
reshape_names<-function(names){
  names<-  make.names(names, unique = TRUE)
  names <- gsub("\\.+", ".", names)
  names
}

reshape_colnames<-function(data){
  colnames(data)<-  make.names(colnames(data), unique = TRUE)
  colnames(data) <- gsub("\\.+", ".", colnames(data))
  data
}

reshape_datalist_colnames<-function(data){
  datas<-list(factors=attr(data,"factors"),coords=attr(data,"coords"))
  newdata<-reshape_colnames(data)
  datas<-datas[sapply(datas,length)>0]
  defdata<-lapply(datas,reshape_colnames)
  attr(newdata,"factors")<-defdata$factors
  attr(newdata,"coords")<-defdata$coords
  newdata
}



#' @export
data_migrate<-function(data,newdata, newname=NULL){
  {
    #newdata<-reshape_datalist_colnames(newdata)
   # data<-reshape_datalist_colnames(data)
    attr(newdata, "datalist_root")=attr(data, "datalist_root")
    #attr(newdata, "data.factor")=attr(data,"data.factor")[rownames(newdata),]
    attr(newdata, "datalist")=attr(data, "datalist")



    attr(newdata, "filename")=attr(data, "filename")
    factors<-attr(data, "factors")[rownames(newdata), , drop=FALSE]
    fac2<-data.frame(lapply(factors,function(x){
      factor(x,levels=levels(x)[levels(x)%in%x])
    }))
    rownames(fac2)<-rownames(newdata)
    colnames(fac2)<-colnames(factors)
    attr(newdata, "factors")=fac2
    attr(newdata, "coords")= attr(data,"coords")[rownames(newdata), , drop=FALSE]

    reshape_colnames(data)

    attr(newdata, "base_shape")= attr(data,"base_shape")
    attr(newdata, "layer_shape")=attr(data,"layer_shape")
    attr(newdata, "extra_shape")=attr(data,"extra_shape")
    scale_attr=attr(data,"scale")
    transf<-attr(data, "transf")
    if(!is.null(transf)){
      colnames(transf)<-paste("Change",1:ncol(transf))
    }
    attr(newdata, "transf")=transf
    attr(newdata, "nobs_ori")=attr(data, "nobs_ori")
    attr(newdata, "nvar_ori")=attr(data, "nvar_ori")
   # attr(newdata, "rf")=attr(data, "rf")
    #attr(newdata, "nb")=attr(data, "nb")
    #attr(newdata, "svm")=attr(data, "svm")
    #attr(newdata, "som")=attr(data, "som")
    attr(newdata, "scale")=scale_attr
    return(newdata)
  }
}



#' @export
ppsummary <- function(m){
  write.table(format(m, justify="left", trim=T),
              row.names=F, col.names=F, quote=F)
}


#' @export
textintro<-function(...){
  column(12,

        column(12,
               br(),
               h4(strong("Welcome to",span('iMESc',style="font-family: 'Alata', sans-serif;"),style="color: #05668D")),
               p("iMESc is a shiny-based application that allows the performance of end-to-end machine learning workflows. The available resources meet several needs of environmental workflows, but it is not restricted to. iMESc includes tools for data pre-processing, to perform descriptive statistics, supervised and unsupervised machine learning algorithms and interactive data visualization by means of graphs, maps, and tables. Throughout the app, data input and output are organized in modules enabling the creation of multiple ML pipelines. Additionally, it allows saving the workspace in a single file, contributing to the best practices in data-sharing and analysis reproducibility. The app is entirely written in the R programming language and thus it is free."),
               p("iMESc allows users to create a savepoint, a single R object that can be reloaded later to restore analysis output."),
               p("Get started by creating a Datalist. Use the",icon(verify_fa = FALSE,name=NULL,class="fas fa-plus"),"button."),
               p("To ensure that iMESc content fits nicely on the screen, we recommend a landscape", strong("minimum resolution  of 1377 x 768 pixels.")))


      )

}


#' @export
textauthors<-function(...){
  column(
    12,
    style="background: white; margin-left: 20px",

    h5(
      strong("Author/Developer")
    ),

    column(
      12,
      p("Danilo C Vieira")
    ),

    h5(
      strong("Contributors")
    ),

    column(
      12,
      p("Fabiana S. Paula"),
      p("Luciana E. Yaginuma"),
      p("Dr. Gustavo Fonseca")
    ),

    h5(
      strong('Acknowledgments')
    ),

    column(
      12,
      p("Special thanks to Daiane Faller, Juliane Castro Carneiro, and Julio Cezar F. Moreira for sparking the initial conversations about machine learning. Their insights were instrumental in shaping the direction and development of this app.")
    )
  )


}
#' @export
#' @export
Read_Shapefile <- function(shp_path) {

  infiles <- shp_path$datapath # get the location of files
  required <- c("\\.shp", "\\.dbf", "\\.shx")
  val <- sapply(required, function(x) grepl(x, infiles))
  validate(need(nrow(val) > 0, "Require at least three files: '.shp', '.dbf' and '.shx'"))
  val <- sum(rowSums(val)) == 3
  validate(need(val, "Require at least three files: '.shp', '.dbf' and '.shx'"))

  dir <- unique(dirname(infiles)) # get the directory
  outfiles <- file.path(dir, shp_path$name) # create new path name
  name <- strsplit(shp_path$name[1], "\\.")[[1]][1] # strip name

  # Use a for loop to rename files
  for (i in seq_along(infiles)) {
    file.rename(infiles[i], outfiles[i])
  }

  x <- read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
  return(x)
}

#' @export
textupload<-function(...){
  as.character(

    "csv or xlsx file where rows are the observations, columns are the variables  The first column must contain the observation labels. Columns containing characters are initially omitted and can later be included as binary columns by factor level."

  )}
#' @export
textlab<-function(...){
  as.character(
    "csv or xlsx file containg the factors for your data, which will be used for labeling, grouping and viewing the results. It can contain as many factors as you want."
  )
}





#' @export
textsupersom<-function(...){
  div(
    tags$style(HTML("
       h2 {
      font-size: 20px;
      font-weight: bold;
      }
      h3 {
      font-size: 20px;
      font-weight: lighter;
      }
      code {
      color: blue;
      }

    ")),

    fluidRow(column(12,
                    p("This functionality uses the ",code('som')," function from the",
                    actionLink("kohonen","kohonen"),"package."),
                    p(icon(verify_fa = FALSE,name=NULL,class="fas fa-exclamation-circle"), "arguments are described 'in the ",actionLink("supersomh","help page")," of the function;")
    ),
    column(12,
           htmlOutput("supersomhelp")
    )))
}





#' @export
textseed<-function(...){
  "A numeric value. If supplied, it ensure that you get the same result if you start with that same seed each time you run the analysis."
}



#' @export
textcoords<-function(...){
  paste(
    em('Required only for the spatial tools menu:'),
    "csv or xlsx file with the longitudes and latitudes of the observations. The first column must contain the name of the observations. The second and third columns must contain the longitude and latitude respectively"


  )
}






#' @export
datastr<-function(data){
  data_structure<-list( n.obs=nrow(data),
                        n.vars=ncol(data),
                        type.vars=table(unlist(lapply(data,function(x) class(x)))))
  return(data_structure)
}
#' @export
pfactors<-function(data.factors, palette="FantasticFox1",newcolhabs){

  for(i in 1:ncol(data.factors)){
    dt<-table(data.factors)
    par(mar=c(1,2,1,0.5))
    barplot(as.matrix(dt), beside=F,legend.text = names(dt), col=getcolhabs(newcolhabs,palette,length(dt)), main=colnames(data.factors)[i],las=1)
  }
  plofac<-recordPlot()
  return(plofac)

}
#' @export
new_limits <- function(bars, width=0.7) {
  original_width <- 1
  new_width <- original_width * width
  offset <- (original_width - new_width) / 2
  new_bars_x1 <- bars - 0.5 + offset
  new_bars_x2 <- bars + 0.5 - offset
  return(list(new_bars_x1 = new_bars_x1, new_bars_x2 = new_bars_x2))
}
blend_with_white <- function(color, percent=1) {
  col_rgb <- col2rgb(color)
  white_rgb <- matrix(rep(255, 3), nrow=3)
  blended_rgb <- percent * white_rgb + (1 - percent) * col_rgb
  return(rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue=255))
}
make_pastel <- function(colors, percent=0.7) {
  sapply(colors, function(color) blend_with_white(color, percent))
}

pfac<-function(factors, width=0.4,
               xlab="Factors",
               ylab='Number of Observations',
               title="Observation Totals by Factor and Level",
               base_size=12,
               border_palette=viridis::turbo,
               fill_palette=viridis::turbo,
               pastel=0.4,
               show_levels=T,
               show_obs=T,
               col_lev="lightsteelblue",
               col_obs="lightcyan"){

  df<-do.call(rbind,lapply(1:ncol(factors),function(i){
    x<-factors[,i]
    tt<-table(x)
    n=as.vector(tt)
    levels=names(tt)
    labels=paste0(levels)
    dd<-data.frame(factor=colnames(factors)[i],
                   level=levels,
                   nobs=as.vector(n),
                   labels=labels)

    dd
  }))
  df <- df[order(df$factor, -as.numeric(as.factor(df$level))),,drop=F]
  df$factor<-factor(df$factor, levels=rev(colnames(factors)))
  df$position <- ave(df$nobs, df$factor, FUN = function(x){
    res<-cumsum(x)
    c(0,res[-length(res)])
  })
  df$label_position_top <- df$position
  li<-split(df,df$factor)
  newl<-new_limits(1:ncol(factors),width)
  df<-data.frame(do.call(rbind,lapply(1:length(li),function(i){
    x<-li[[i]]
    x$pos_x<-newl[[1]][i]
    x$pos_x2<-newl[[2]][i]
    x
  })))
  blend_with_white <- function(color, percent=1) {
    col_rgb <- col2rgb(color)
    white_rgb <- matrix(rep(255, 3), nrow=3)
    blended_rgb <- percent * white_rgb + (1 - percent) * col_rgb
    return(rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue=255))
  }
  make_pastel <- function(colors, percent=0.7) {
    sapply(colors, function(color) blend_with_white(color, percent))
  }
  border_colors <- border_palette(256)
  fill_colors <- fill_palette(256)
  pastel_fill <- make_pastel(fill_colors, pastel)
  df$label_fill <- "lightcyan"
  df$nobs_fill <- "lightsteelblue"

  p<-ggplot(df, aes(x=factor, y=nobs)) +
    geom_bar(aes(fill=position, color=position), position="stack", stat="identity", show.legend=F, width=width) +
    scale_fill_gradientn(colors = pastel_fill, guide = "none") +
    scale_color_gradientn(colors = border_colors, guide = "none") +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    new_scale_fill()+
    theme_bw(base_size)
  lab_levels<-col_levels<-c()
  if(isTRUE(show_levels)) {
    req(length(col_lev)==1)
    p<-p + geom_label(
      aes(label = labels, y = label_position_top,x=pos_x2, fill = label_fill),
      label.r = unit(0, "lines"),
      label.size = 0,
      label.padding = unit(0.15, "lines"),
      hjust = 0,
      vjust = 1,
      show.legend = T
    )

    col_levels[ length(col_levels)+1]<-col_lev
    lab_levels[ length(lab_levels)+1]<-"Level"

  }
  if(isTRUE(show_obs)){
    req(length(col_obs)==1)
    p<-p +geom_label(
      aes(label = nobs, y = label_position_top,x=pos_x, fill = nobs_fill),
      label.r = unit(0, "lines"),
      label.size = 0,
      label.padding = unit(0.15, "lines"),
      hjust = 0,
      vjust = 0,
      show.legend = T
    )
    col_levels[length(col_levels)+1]<-col_obs
    lab_levels[length(lab_levels)+1]<-"Number of Observations"
  }
  if(isTRUE(show_obs)|isTRUE(show_levels)){
    p<-p+ scale_fill_manual(values = col_levels,
                            labels =lab_levels,
                            name = "")}

  p<-p+guides(fill = guide_legend(override.aes = list(label = "")))+ coord_flip()

  return(p)
}

.checkSelect<-function (select, scores) {
  if (is.logical(select) && !isTRUE(all.equal(length(select),
                                              NROW(scores)))) {
    warning("length of 'select' does not match the number of scores: ignoring 'select'")
  }
  else {
    scores <- if (is.matrix(scores)) {
      scores[select, , drop = FALSE]
    }
    else {
      scores[select]
    }
  }
  scores
}



#' @export
#'
pwRDA.source<-function (x.ord, y.ord, BPs) {
  y.ord <- as.matrix(y.ord)
  x.ord <- as.matrix(x.ord)
  n <- nrow(x.ord)
  k <- ncol(x.ord)
  bks <- c(0, BPs, nrow(x.ord))
  nBPs <- length(bks) - 1
  Xb <- matrix(0, ncol = nBPs * k, nrow = n)
  for (i in 1:(nBPs)) {
    Xb[(bks[i] + 1):bks[i + 1], ((k * (i - 1)) + 1):((k *
                                                        (i - 1)) + k)] <- x.ord[(bks[i] + 1):bks[i + 1],
                                                        ]
  }
  Xb <- jitter(Xb)
  Xbc = scale(Xb, center = T, scale = F)
  rda.0 <- vegan::rda(data.frame(y.ord) ~ ., data.frame(x.ord))
  rda.pw <- vegan::rda(data.frame(y.ord) ~ ., data.frame(Xb))
  Yc = scale(y.ord, center = T, scale = F)
  Y.avg = matrix(rep(apply(Yc, 2, mean), times = nrow(Yc)),
                 ncol = ncol(Yc), byrow = T)
  B.pw = solve(t(Xbc) %*% Xbc) %*% (t(Xbc) %*% Yc)
  coord <- rda.pw$CCA$biplot
  bew.bp <- t(cor(coord, t(cor(x.ord, Xb))))
  rda.pw$CCA$biplot <- bew.bp
  Ypred.pw = Xbc %*% B.pw
  Yres <- Yc - Ypred.pw
  TSS.pw = sum((Yc)^2)
  RSS.pw = sum((Ypred.pw - Y.avg)^2)
  r2.pw <- RSS.pw/TSS.pw
  n.pw <- nrow(Xbc)
  k.pw <- ncol(Xbc)
  Radj.pw <- 1 - ((1 - r2.pw) * ((n.pw - 1)/(n.pw - k.pw -
                                               1)))
  Xc = scale(jitter(x.ord), center = T, scale = F)
  B.full = solve(t(Xc) %*% Xc) %*% (t(Xc) %*% Yc)
  Ypred.full = Xc %*% B.full
  Yres <- Yc - Ypred.full
  TSS.full = sum((Yc)^2)
  RSS.full = sum((Ypred.full - Y.avg)^2)
  r2.full <- RSS.full/TSS.full
  n.full <- nrow(Xc)
  k.full <- ncol(Xc)
  Radj.full <- 1 - ((1 - r2.full) * ((n.full - 1)/(n.full -
                                                     k.full - 1)))
  F.stat <- ((RSS.full - RSS.pw)/(k.full - k.pw))/(RSS.full/(n.pw -
                                                               k.full))
  dg1 <- k.pw - k.full
  dg2 <- n.pw - k.pw
  F.stat <- ((RSS.pw - RSS.full)/(dg1))/(RSS.pw/(dg2))
  p.value <- 1 - pf(F.stat, dg1, dg2, lower.tail = T)
  summ <- c(Radj.full = Radj.full, Radj.pw = Radj.pw, F.stat = F.stat,
            p.value = p.value)
  pw <- list(summ = summ, rda.0 = rda.0, rda.pw = rda.pw)
  class(pw) <- "pw"
  return(invisible(pw))
}


#' @export
str_numerics<-function(numerics, cextext=1, col="gray", border="gray", width_varname=0.2, width_metrics=0.35, width_histo=0.35, round=2, show=c('Min.' ,'1st Qu.',"Mean",'Median','3rd Qu.','Max.')){

  par(mar=c(0.5,0,0,0), cex=2)
  m<-matrix(1:(ncol(numerics)*3), ncol=3)
  m=m+3
  m<-rbind(1:3, m)
  layout(m, widths = c(width_varname,width_metrics,width_histo))
  opar<-par(no.readonly=TRUE)
  plot.new()
  text(.5,.5,"Variable", cex=cextext)
  plot.new()
  text(  seq(0.1,0.9, length.out=length(show)),.5,show, cex=cextext)
  plot.new()
  text(.5,.5,"Histogram", cex=cextext)
i=1
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(.5,.5,colnames(numerics)[i], cex=cextext)
  }
  for(i in 1: ncol(numerics))
  {
    plot.new()
    text(  seq(0.1,0.9, length.out=length(show)),.5,round(summary(numerics[,i]),round)[show], cex=cextext)
  }

  for(i in 1: ncol(numerics))
  {

    hist(numerics[,i], ann=F, axes=F, col=col, border=border)
  }



}







#' @export
getHelp <- function(fun){
  temp = tools::Rd2HTML(gbRd::Rd_fun(fun),out = tempfile("docs"),dynamic=T)
  content = readr::read_file(temp)
  file.remove(temp)
  content
}
#' @export





#' @export
#' @export
divI<-function(abund,choices=c("N","S","margalef","D","H","J'","Dom_rel","Skewness")){
  res=list()
  if("N"%in%choices){res$N<-rowSums(abund)}
  if("S"%in%choices){res$S<-vegan::specnumber(abund)}
  if("margalef"%in%choices){res$margalef=(vegan::specnumber(abund)-1)/log(rowSums(abund))}
  if("D"%in%choices){res$D<-vegan::diversity(abund, index="simpson")}
  if("H"%in%choices){res$H<-vegan::diversity(abund)}
  if("J'"%in%choices){
    H<-vegan::diversity(abund)
    S<-vegan::specnumber(abund)
    res$J <- H/log(S)}
  if("Dom_rel"%in%choices){res$Dom_rel<-apply(vegan::decostand(abund, "total"),1,sort,T)[1,]}
  if("Skewness"%in%choices){res$Skewness=apply(vegan::decostand(abund,"log"),1,skewness)}
  return(data.frame(do.call(cbind,res)))
}







virtualPicker_unique<-function(id,label,choices,selected=NULL,search=T,multiple=F,allOptionsSelectedText="All",alwaysShowSelectedOptionsCount=F,width=NULL,position="bottom"){
  div(class="picker13",
      shinyWidgets::virtualSelectInput(
        id,

        label,choices=choices,selected=selected,multiple = multiple,  search =search,
        alwaysShowSelectedOptionsCount=alwaysShowSelectedOptionsCount,
        allOptionsSelectedText=allOptionsSelectedText,

        optionHeight='24px',position=position,
        width=width
      )
  )
}
